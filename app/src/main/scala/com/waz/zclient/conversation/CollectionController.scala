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
package com.waz.zclient.conversation

import java.util
import java.util.concurrent.CopyOnWriteArraySet

import android.text.SpannableString
import android.text.style.BackgroundColorSpan
import com.waz.ZLog._
import com.waz.api.{ContentSearchQuery, IConversation, Message, TypeFilter}
import com.waz.model._
import com.waz.service.ZMessaging
import com.waz.threading.SerialDispatchQueue
import com.waz.utils.events.{EventStream, Signal, SourceSignal}
import com.waz.zclient.controllers.collections.CollectionsObserver
import com.waz.zclient.conversation.CollectionController.CollectionInfo
import com.waz.zclient.{Injectable, Injector}

class CollectionController(implicit injector: Injector) extends Injectable {

  private implicit val tag: LogTag = logTagFor[CollectionController]

  private implicit val dispatcher = new SerialDispatchQueue(name = "CollectionController")

  val zms = inject[Signal[ZMessaging]]

  val currentConv = zms.flatMap(_.convsStats.selectedConversationId).collect { case Some(id) => id }

  val msgStorage = zms.map(_.messagesStorage)

  val assetStorage = zms.map(_.assetsStorage)

  private val observers: java.util.Set[CollectionsObserver] = new util.HashSet[CollectionsObserver]

  val conversation = zms.zip(currentConv) flatMap { case (zms, convId) => zms.convsStorage.signal(convId) }

  val conversationName = conversation map (data => if (data.convType == IConversation.Type.GROUP) data.name.filter(!_.isEmpty).getOrElse(data.generatedName) else data.generatedName)

  val focusedItem: SourceSignal[Option[MessageData]] = Signal(None)

  val openedCollection = Signal[Option[CollectionInfo]]()

  val openContextMenuForMessage = EventStream[MessageData]()

  val clickedMessage = EventStream[MessageData]()

  val contentSearchQuery = Signal[ContentSearchQuery](ContentSearchQuery.empty)

  private def performOnObservers(func: (CollectionsObserver) => Unit) = {
    val collectionObservers: CopyOnWriteArraySet[CollectionsObserver] = new CopyOnWriteArraySet[CollectionsObserver](observers)
    import scala.collection.JavaConversions._
    for (observer <- collectionObservers) {
      func(observer)
    }
  }

  def openCollection = performOnObservers(_.openCollection())

  def closeCollection = { performOnObservers(_.closeCollection()); openedCollection ! None }

  def requestPreviousItem(): Unit = performOnObservers(_.previousItemRequested())

  def requestNextItem(): Unit = performOnObservers(_.nextItemRequested())

  def openShareCollectionItem(messageData: MessageData): Unit = performOnObservers(_.shareCollectionItem(messageData))

  def closeShareCollectionItem(): Unit = {performOnObservers(_.closeCollectionShare())}

  def addObserver(collectionsObserver: CollectionsObserver): Unit = observers.add(collectionsObserver)

  def removeObserver(collectionsObserver: CollectionsObserver): Unit = observers.remove(collectionsObserver)

  def clearSearch() = {
    focusedItem ! None
    contentSearchQuery ! ContentSearchQuery.empty
  }
}

object CollectionController {

  val GridColumns = 4
  def injectedCollectionController(injectable: Injectable)(implicit injector: Injector): CollectionController =  {
    injectable.inject[CollectionController]
  }

  case class CollectionInfo(conversation: ConversationData, empty: Boolean)

  trait ContentType {
    val msgTypes: Seq[Message.Type]
    val typeFilter: Seq[TypeFilter]
  }

  case object Links extends ContentType {
    override val msgTypes = Seq(Message.Type.RICH_MEDIA)
    override val typeFilter: Seq[TypeFilter] = Seq(TypeFilter(Message.Type.RICH_MEDIA, None))
  }

  case object Images extends ContentType {
    override val msgTypes = Seq(Message.Type.ASSET)
    override val typeFilter: Seq[TypeFilter] = Seq(TypeFilter(Message.Type.ASSET, None))
  }

  //Now we can add more types to this sequence for the "others" category
  case object Files extends ContentType {
    override val msgTypes = Seq(Message.Type.ANY_ASSET)
    override val typeFilter: Seq[TypeFilter] = Seq(TypeFilter(Message.Type.ANY_ASSET, None))
  }

  case object AllContent extends ContentType {
    //feels a little bit messy... maybe think of a neater way to represent the types
    override val msgTypes = Images.msgTypes ++ Files.msgTypes ++ Links.msgTypes
    override val typeFilter: Seq[TypeFilter] = Seq(
      TypeFilter(Message.Type.ASSET, Some(8)),
      TypeFilter(Message.Type.RICH_MEDIA, Some(3)),
      TypeFilter(Message.Type.ANY_ASSET, Some(3))
    )
  }
}

object CollectionUtils {
  def getHighlightedSpannableString(originalMessage: String, normalizedMessage: String, queries: Set[String], color: Int, beginThreshold: Int = -1): (SpannableString, Int) ={
    def getQueryPosition(normalizedMessage: String, query: String, fromIndex: Int = 0, acc: Seq[(Int, Int)] = Seq()): Seq[(Int, Int)] ={
      val beginIndex = normalizedMessage.indexOf(query, fromIndex)
      if (beginIndex < 0) {
        return acc
      }
      val endIndex = Math.min(beginIndex + query.length, normalizedMessage.length)
      getQueryPosition(normalizedMessage, query, endIndex, acc ++ Seq((beginIndex, endIndex)))
    }
    val matches = queries.flatMap(getQueryPosition(normalizedMessage, _)).filter(_._1 >= 0)
    if (matches.isEmpty) {
      return (new SpannableString(originalMessage), 0)
    }
    val minPos = if (beginThreshold == -1) 0 else Math.max(matches.map(_._1).min - beginThreshold, 0)
    val ellipsis = if (minPos > 0) "..." else ""
    val spannableString = new SpannableString(ellipsis + originalMessage.substring(minPos))
    val offset = minPos - ellipsis.length
    matches.foreach(pos => spannableString.setSpan(new BackgroundColorSpan(color), pos._1 - offset, pos._2 - offset, 0))
    (spannableString, matches.size)
  }
}
