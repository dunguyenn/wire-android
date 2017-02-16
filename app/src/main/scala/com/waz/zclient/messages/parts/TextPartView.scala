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
package com.waz.zclient.messages.parts

import android.animation.ValueAnimator
import android.animation.ValueAnimator.AnimatorUpdateListener
import android.content.Context
import android.graphics.Color
import android.util.{AttributeSet, TypedValue}
import com.waz.api.Message
import com.waz.content.MessageIndexStorage
import com.waz.model.{MessageContent, MessageData}
import com.waz.service.messages.MessageAndLikes
import com.waz.threading.Threading
import com.waz.utils.events.Signal
import com.waz.zclient.controllers.global.AccentColorController
import com.waz.zclient.conversation.{CollectionController, CollectionUtils}
import com.waz.zclient.messages.MessageView.MsgBindOptions
import com.waz.zclient.messages.{ClickableViewPart, MsgPart}
import com.waz.zclient.ui.text.LinkTextView
import com.waz.zclient.ui.utils.ColorUtils
import com.waz.zclient.{R, ViewHelper}

import scala.util.Success

class TextPartView(context: Context, attrs: AttributeSet, style: Int) extends LinkTextView(context, attrs, style) with ViewHelper with ClickableViewPart with EphemeralPartView {
  def this(context: Context, attrs: AttributeSet) = this(context, attrs, 0)
  def this(context: Context) = this(context, null, 0)

  override val tpe: MsgPart = MsgPart.Text

  val collectionController = inject[CollectionController]
  val accentColorController = inject[AccentColorController]

  val textSizeRegular = context.getResources.getDimensionPixelSize(R.dimen.wire__text_size__regular)
  val textSizeEmoji = context.getResources.getDimensionPixelSize(R.dimen.wire__text_size__emoji)

  registerEphemeral(this)

  var part: Option[MessageContent] = None
  var accentColor = Color.TRANSPARENT
  accentColorController.accentColor{c => accentColor = c.getColor}

  val animator = ValueAnimator.ofFloat(1, 0).setDuration(750)
  animator.addUpdateListener(new AnimatorUpdateListener {
    override def onAnimationUpdate(animation: ValueAnimator): Unit = {
      val alpha = ((1 - animation.getAnimatedFraction) * 255).toInt
      setBackgroundColor(ColorUtils.injectAlpha(alpha, accentColor))
    }
  })

  Signal(collectionController.focusedItem, collectionController.contentSearchQuery, message, accentColorController.accentColor).on(Threading.Ui) {
    case (Some(searchedMessage), query, messageData, color) if query.originalString.nonEmpty && messageData.id.equals(searchedMessage.id) =>
      implicit val executionContext = Threading.Ui
      animator.start()
      zms.head.flatMap(_.messagesIndexStorage.getNormalizedContentForMessage(searchedMessage.id)).onComplete{
        case Success(Some(normalizedMessage)) =>
          val spannable = CollectionUtils.getHighlightedSpannableString(messageData.contentString, normalizedMessage, query.elements, ColorUtils.injectAlpha(0.5f, color.getColor()))._1
          setText(spannable)
        case _ =>
      }
    case (_, _, messageData, _) =>
      setTextLink(part.fold(messageData.contentString)(_.content))
      setBackgroundColor(Color.TRANSPARENT)
    case _ =>
      setTextLink("")
      setBackgroundColor(Color.TRANSPARENT)
  }

  override def set(msg: MessageAndLikes, part: Option[MessageContent], opts: MsgBindOptions): Unit = {
    super.set(msg, part, opts)
    setTextSize(TypedValue.COMPLEX_UNIT_PX, if (isEmojiOnly(msg.message, part)) textSizeEmoji else textSizeRegular)
    setTextLink(part.fold(msg.message.contentString)(_.content))
    this.part = part
  }

  def isEmojiOnly(msg: MessageData, part: Option[MessageContent]) =
    part.fold(msg.msgType == Message.Type.TEXT_EMOJI_ONLY)(_.tpe == Message.Part.Type.TEXT_EMOJI_ONLY)
}
