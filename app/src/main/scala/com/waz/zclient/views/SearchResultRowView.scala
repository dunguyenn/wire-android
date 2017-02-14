/**
 * Wire
 * Copyright (C) 2017 Wire Swiss GmbH
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
package com.waz.zclient.views

import android.content.Context
import android.text.SpannableString
import android.text.format.DateFormat
import android.text.style.BackgroundColorSpan
import android.util.AttributeSet
import android.widget.LinearLayout
import com.waz.api.ContentSearchQuery
import com.waz.service.ZMessaging
import com.waz.threading.Threading
import com.waz.utils.events.Signal
import com.waz.zclient.common.views.ChatheadView
import com.waz.zclient.controllers.global.AccentColorController
import com.waz.zclient.messages.MsgPart.Text
import com.waz.zclient.messages.{MessageViewPart, MsgPart, UsersController}
import com.waz.zclient.ui.text.TypefaceTextView
import com.waz.zclient.ui.utils.{ColorUtils, TextViewUtils}
import com.waz.zclient.utils.ZTimeFormatter._
import com.waz.zclient.utils.{DateConvertUtils, ViewUtils}
import com.waz.zclient.{R, ViewHelper}
import org.threeten.bp.{LocalDateTime, ZoneId}

trait SearchResultRowView extends MessageViewPart with ViewHelper{
  val searchedQuery = Signal[ContentSearchQuery]()
}

class TextSearchResultRowView(context: Context, attrs: AttributeSet, style: Int) extends LinearLayout(context, attrs, style) with SearchResultRowView{
  def this(context: Context, attrs: AttributeSet) = this(context, attrs, 0)
  def this(context: Context) = this(context, null, 0)

  override val tpe: MsgPart = Text

  inflate(R.layout.search_text_result_row)

  val zms = inject[Signal[ZMessaging]]
  val accentColorController = inject[AccentColorController]
  val usersController = inject[UsersController]

  lazy val contentTextView = ViewUtils.getView(this, R.id.message_content).asInstanceOf[TypefaceTextView]
  lazy val infoTextView = ViewUtils.getView(this, R.id.message_info).asInstanceOf[TypefaceTextView]
  lazy val chatheadView = ViewUtils.getView(this, R.id.chathead).asInstanceOf[ChatheadView]

  val contentSignal = for{
    m <- message
    q <- searchedQuery if q.toString().nonEmpty
    color <- accentColorController.accentColor
    nContent <- zms.flatMap(z => Signal.future(z.messagesIndexStorage.getNormalizedContentForMessage(m.id)))
  } yield (m, q, color, nContent)

  contentSignal.on(Threading.Ui){
    case (msg, query, color, Some(normalizedContent)) =>
      contentTextView.setText(getHighlightedSpannableString(msg.contentString, normalizedContent, query.elements, ColorUtils.injectAlpha(0.5f, color.getColor())))
    case (msg, query, color, None) =>
      contentTextView.setText(msg.contentString)
    case _ =>
  }

  val infoSignal = for{
    m <- message
    u <- usersController.user(m.userId)
  } yield (m, u)

  infoSignal.on(Threading.Ui){
    case (msg, user) =>
      val timeStr = getSeparatorTime(getContext, LocalDateTime.now, DateConvertUtils.asLocalDateTime(msg.time), DateFormat.is24HourFormat(getContext), ZoneId.systemDefault, true, false)
      infoTextView.setText(TextViewUtils.getBoldText(getContext, s"[[${user.name}]] $timeStr"))
      chatheadView.setUserId(msg.userId)
    case _ =>
  }

  private def getHighlightedSpannableString(originalMessage: String, normalizedMessage: String, queries: Set[String], color: Int): SpannableString ={
    def getQueryPosition(normalizedMessage: String, query: String, fromIndex: Int = 0, acc: Seq[(Int, Int)] = Seq()): Seq[(Int, Int)] ={
      val beginIndex = normalizedMessage.indexOf(query, fromIndex)
      if (beginIndex < 0) {
        return acc
      }
      val endIndex = Math.min(beginIndex + query.length, normalizedMessage.length)
      getQueryPosition(normalizedMessage, query, endIndex, acc ++ Seq((beginIndex, endIndex)))
    }
    val spannableString = new SpannableString(originalMessage)
    queries.flatMap(getQueryPosition(normalizedMessage, _)).filter(_._1 >= 0).foreach(pos => spannableString.setSpan(new BackgroundColorSpan(color), pos._1, pos._2, 0))
    spannableString
  }
}
