/**
 * Wire
 * Copyright (C) 2016 Wire Swiss GmbH
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package com.waz.zclient.messages.controllers

import java.util

import android.app.{Activity, ProgressDialog}
import android.content.DialogInterface.OnDismissListener
import android.content._
import android.net.Uri
import android.support.v4.app.{FragmentManager, ShareCompat}
import android.support.v7.app.AlertDialog
import android.widget.Toast
import com.waz.ZLog.ImplicitTag._
import com.waz.api.{Asset, ImageAsset, Message}
import com.waz.model._
import com.waz.service.ZMessaging
import com.waz.service.messages.MessageAndLikes
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils._
import com.waz.utils.events.{EventContext, EventStream, Signal}
import com.waz.zclient.common.controllers.{PermissionsController, WriteExternalStoragePermission}
import com.waz.zclient.controllers.global.KeyboardController
import com.waz.zclient.controllers.userpreferences.IUserPreferencesController
import com.waz.zclient.notifications.controllers.ImageNotificationsController
import com.waz.zclient.pages.main.conversation.controller.IConversationScreenController
import com.waz.zclient.pages.main.conversation.views.MessageBottomSheetDialog
import com.waz.zclient.pages.main.conversation.views.MessageBottomSheetDialog.{Callback, MessageAction}
import com.waz.zclient.ui.cursor.CursorLayout
import com.waz.zclient.ui.utils.KeyboardUtils
import com.waz.zclient.utils.ContextUtils._
import com.waz.zclient.{Injectable, Injector, R, WireContext}

import scala.util.Success

// TODO: rewrite to not use java message and asset api
class MessageActionsController(implicit injector: Injector, ctx: Context, ec: EventContext) extends Injectable {
  import com.waz.threading.Threading.Implicits.Ui

  private val context                   = inject[Activity]
  private lazy val keyboardController   = inject[KeyboardController]
  private lazy val userPrefsController  = inject[IUserPreferencesController]
  private lazy val clipboardManager     = inject[ClipboardManager]
  private lazy val permissions          = inject[PermissionsController]
  private lazy val imageNotifications   = inject[ImageNotificationsController]
  private lazy val fragmentManager      = inject[FragmentManager]

  private val zms = inject[Signal[ZMessaging]]

  val onMessageAction = EventStream[(MessageAction, Message)]()

  val onDeleteConfirmed = EventStream[(Message, Boolean)]() // Boolean == isRecall(true) or localDelete(false)
  val onAssetSaved = EventStream[Asset]()

  val messageToReveal = Signal[Option[MessageData]]()

  private var dialog = Option.empty[MessageBottomSheetDialog]

  private val callback = new Callback {
    override def onAction(action: MessageAction, message: Message): Unit = onMessageAction ! (action, message)
  }

  onMessageAction {
    case (MessageAction.COPY, message)             => copyMessage(message)
    case (MessageAction.DELETE_GLOBAL, message)    => recallMessage(message)
    case (MessageAction.DELETE_LOCAL, message)     => deleteMessage(message)
    case (MessageAction.FORWARD, message)          => forwardMessage(message)
    case (MessageAction.LIKE, message)             => toggleLike(message)
    case (MessageAction.UNLIKE, message)           => toggleLike(message)
    case (MessageAction.SAVE, message)             => saveMessage(message)
    case (MessageAction.REVEAL, message)           => revealMessageInConversation(message)
    case (MessageAction.DELETE, message)           => promptDeleteMessage(message)
    case _ => // should be handled somewhere else
  }

  private val onDismissed = new OnDismissListener {
    override def onDismiss(dialogInterface: DialogInterface): Unit = dialog = None
  }

  private def isConvMember(conv: ConvId) = zms.head.flatMap { zs =>
    zs.membersStorage.isActiveMember(conv, zs.selfUserId)
  }

  def showDialog(data: MessageAndLikes, fromCollection: Boolean = false): Boolean = {
    val msg = data.message
    if (msg.isEphemeral) return false
    (for {
      isMember <- isConvMember(msg.convId)
      _ <- keyboardController.hideKeyboardIfVisible()   // TODO: keyboard should be handled in more generic way
      message = ZMessaging.currentUi.messages.cachedOrUpdated(data)
    } yield {
      dialog.foreach(_.dismiss())
      dialog = Some(
        returning(new MessageBottomSheetDialog(context, R.style.message__bottom_sheet__base, message, isMember, fromCollection, callback)) { d =>
          d.setOnDismissListener(onDismissed)
          d.show()
        }
      )
    }).recoverWithLog()
    true
  }

  def showDeleteDialog(message: Message): Unit = {
    val options = new util.HashSet[MessageAction]()
    options.add(MessageAction.DELETE_LOCAL)
    options.add(MessageAction.DELETE_GLOBAL)
    val dialog = new MessageBottomSheetDialog(context, R.style.message__bottom_sheet__base, message, true, true, callback, options)
    dialog.show()
  }

  private def toggleLike(message: Message) = {
    if (message.isLikedByThisUser)
      message.unlike()
    else {
      message.like()
      userPrefsController.setPerformedAction(IUserPreferencesController.LIKED_MESSAGE)
    }
  }

  private def copyMessage(message: Message) = {
    val clip = ClipData.newPlainText(getString(R.string.conversation__action_mode__copy__description, message.getUser.getDisplayName), message.getBody)
    clipboardManager.setPrimaryClip(clip)
    Toast.makeText(context, R.string.conversation__action_mode__copy__toast, Toast.LENGTH_SHORT).show()
  }

  private def deleteMessage(message: Message) =
    showDeleteDialog(R.string.conversation__message_action__delete_for_me) {
      message.delete()
      onDeleteConfirmed ! (message, false)
    }

  private def recallMessage(message: Message) =
    showDeleteDialog(R.string.conversation__message_action__delete_for_everyone) {
      message.recall()
      onDeleteConfirmed ! (message, true)
    }

  private def promptDeleteMessage(message: Message) = {
    if (message.getUser.isMe)
      showDeleteDialog(message)
    else
      deleteMessage(message)
  }

  private def showDeleteDialog(title: Int)(onSuccess: => Unit) =
    new AlertDialog.Builder(context)
      .setTitle(title)
      .setMessage(R.string.conversation__message_action__delete_details)
      .setCancelable(true)
      .setNegativeButton(R.string.conversation__message_action__delete__dialog__cancel, null)
      .setPositiveButton(R.string.conversation__message_action__delete__dialog__ok, new DialogInterface.OnClickListener() {
        def onClick(dialog: DialogInterface, which: Int): Unit = {
          onSuccess
          Toast.makeText(context, R.string.conversation__message_action__delete__confirmation, Toast.LENGTH_SHORT).show()
        }
      })
      .create()
      .show()

  private def forwardMessage(message: Message) = {
    val asset = message.getAsset
    val intentBuilder = ShareCompat.IntentBuilder.from(context)
    intentBuilder.setChooserTitle(R.string.conversation__action_mode__fwd__chooser__title)
    if (asset.isEmpty) { // TODO: handle location and other non text messages
      intentBuilder.setType("text/plain")
      intentBuilder.setText(message.getBody)
      intentBuilder.startChooser()
    } else {
      val dialog = ProgressDialog.show(context,
        getString(R.string.conversation__action_mode__fwd__dialog__title),
        getString(R.string.conversation__action_mode__fwd__dialog__message), true, true, null)

      asset.getContentUri(new Asset.LoadCallback[Uri]() {
        def onLoaded(uri: Uri): Unit = {
          dialog.dismiss()
          intentBuilder.setType(asset.getMimeType)
          intentBuilder.addStream(uri)
          intentBuilder.startChooser()
        }
        def onLoadFailed(): Unit = {
          // TODO: show error info
          dialog.dismiss()
        }
      })
    }
  }

  private def saveMessage(message: Message) =
    permissions.withPermissions(WriteExternalStoragePermission) {  // TODO: provide explanation dialog - use requiring with message str
      if (message.getMessageType == Message.Type.ASSET) { // TODO: simplify once SE asset v3 is merged, we should be able to handle that without special conditions
        val asset = message.getImage
        asset.saveImageToGallery(new ImageAsset.SaveCallback() {
          def imageSaved(uri: Uri): Unit = {
            imageNotifications.showImageSavedNotification(asset.getId, uri)
            Toast.makeText(context, R.string.message_bottom_menu_action_save_ok, Toast.LENGTH_SHORT).show()
          }
          def imageSavingFailed(ex: Exception): Unit =
            Toast.makeText(context, com.waz.zclient.ui.R.string.content__file__action__save_error, Toast.LENGTH_SHORT).show()
        })
      } else {
        val dialog = ProgressDialog.show(context, getString(R.string.conversation__action_mode__fwd__dialog__title), getString(R.string.conversation__action_mode__fwd__dialog__message), true, true, null)
        val asset = message.getAsset
        asset.saveToDownloads(new Asset.LoadCallback[Uri]() {
          def onLoaded(uri: Uri): Unit = {
            onAssetSaved ! asset
            Toast.makeText(context, com.waz.zclient.ui.R.string.content__file__action__save_completed, Toast.LENGTH_SHORT).show()
            context.sendBroadcast(returning(new Intent(Intent.ACTION_MEDIA_SCANNER_SCAN_FILE))(_.setData(uri)))
            dialog.dismiss()
          }

          def onLoadFailed(): Unit = {
            Toast.makeText(context, com.waz.zclient.ui.R.string.content__file__action__save_error, Toast.LENGTH_SHORT).show()
            dialog.dismiss()
          }
        })
      }
    }

  private def revealMessageInConversation(message: Message) = {
    zms.head.flatMap(z => z.messagesStorage.get(MessageId(message.getId))).onComplete{
      case Success(msg) =>  messageToReveal ! msg
      case _ =>
    }
  }
}

/**
  * Temporary class to easily integrate java view with scala controllers.
  * FIXME: remove this class, this logic should be moved to some edit controller and cursor view
  */
class EditActionSupport(ctx: WireContext, cursor: CursorLayout) extends Injectable {
  import scala.concurrent.duration._
  private implicit val injector: Injector = ctx.injector

  private implicit val eventContext = inject[EventContext]
  private val actionsController = inject[MessageActionsController]
  private val convScreenController = inject[IConversationScreenController]
  private val keyboardController = inject[KeyboardController]

  actionsController.onMessageAction {
    case (MessageAction.EDIT, message) =>
      cursor.editMessage(message)
      convScreenController.setMessageBeingEdited(message)
      // TODO: don't show keyboard directly, KeyboardController should handle that in more general way
      // Add small delay so triggering keyboard works
      CancellableFuture.delayed(200.millis) {
        KeyboardUtils.showKeyboard(inject[Activity])
      } (Threading.Ui)
    case _ => // ignore
  }
}
