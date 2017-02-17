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
import com.waz.model.{MessageContent, MessageData, MessageId}
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

class TextPartView(context: Context, attrs: AttributeSet, style: Int) extends LinkTextView(context, attrs, style) with ViewHelper with ClickableViewPart with EphemeralPartView {
  def this(context: Context, attrs: AttributeSet) = this(context, attrs, 0)
  def this(context: Context) = this(context, null, 0)

  override val tpe: MsgPart = MsgPart.Text

  val collectionController = inject[CollectionController]
  val accentColorController = inject[AccentColorController]

  val textSizeRegular = context.getResources.getDimensionPixelSize(R.dimen.wire__text_size__regular)
  val textSizeEmoji = context.getResources.getDimensionPixelSize(R.dimen.wire__text_size__emoji)

  registerEphemeral(this)

  var messagePart = Signal[Option[MessageContent]]()
  var accentColor = Color.TRANSPARENT
  accentColorController.accentColor{c => accentColor = c.getColor}

  val animator = ValueAnimator.ofFloat(1, 0).setDuration(1500)
  animator.addUpdateListener(new AnimatorUpdateListener {
    override def onAnimationUpdate(animation: ValueAnimator): Unit = {
      val alpha = Math.min(animation.getAnimatedValue.asInstanceOf[Float], 0.5f)
      setBackgroundColor(ColorUtils.injectAlpha(alpha, accentColor))
    }
  })

  val messageSignal = for{
    messageData <- message
    part <- messagePart
    searchedMessage <- collectionController.focusedItem
    query <- collectionController.contentSearchQuery
    color <- accentColorController.accentColor
    normalizedMessage <- zms.flatMap(z => Signal.future(z.messagesIndexStorage.getNormalizedContentForMessage(searchedMessage.fold(MessageId())(_.id))))
  } yield (messageData, part, searchedMessage, query, color, normalizedMessage)

  messageSignal.on(Threading.Ui){
    case (messageData, part, Some(searchedMessage), query, color, Some(normalizedMessage)) if query.originalString.nonEmpty && messageData.id.equals(searchedMessage.id) =>
      animator.start()
      val spannable = CollectionUtils.getHighlightedSpannableString(messageData.contentString, normalizedMessage, query.elements, ColorUtils.injectAlpha(0.5f, color.getColor()))._1
      setText(spannable)
    case (messageData, part, _, _, _, _) =>
      setTextLink(part.fold(messageData.contentString)(_.content))
      setBackgroundColor(Color.TRANSPARENT)
    case _ =>
      setTextLink("")
      setBackgroundColor(Color.TRANSPARENT)
  }

  override def set(msg: MessageAndLikes, part: Option[MessageContent], opts: MsgBindOptions): Unit = {
    animator.end()
    super.set(msg, part, opts)
    setTextSize(TypedValue.COMPLEX_UNIT_PX, if (isEmojiOnly(msg.message, part)) textSizeEmoji else textSizeRegular)
    messagePart ! part
  }

  def isEmojiOnly(msg: MessageData, part: Option[MessageContent]) =
    part.fold(msg.msgType == Message.Type.TEXT_EMOJI_ONLY)(_.tpe == Message.Part.Type.TEXT_EMOJI_ONLY)
}
