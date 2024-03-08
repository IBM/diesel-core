/*
 * Copyright 2018 The Diesel Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package diesel

import diesel.Lexer.Token

trait UserDataProvider {
  def getUserData(key: Any): Option[Any]
  def setUserData(key: Any, value: Any): Unit
}

object UserDataProvider {
  def apply(data: Seq[(Any, Any)]): UserDataProvider =
    new DefaultUserDataProvider(data.toMap)
}

private class DefaultUserDataProvider(private var data: Map[Any, Any]) extends UserDataProvider {
  override def getUserData(key: Any): Option[Any] = data.get(key)

  override def setUserData(key: Any, value: Any): Unit = {
    data = data + (key -> value)
  }
}

case class ContextualUserData(parent: Option[ContextualUserData]) {

  private var data: Map[Any, Any] = Map()

  def get(key: Any): Option[Any] = data.get(key) match {
    case Some(value) => Some(value)
    case None        => parent match {
        case Some(value) => value.get(key)
        case None        => None
      }
  }

  def getLocal(key: Any): Option[Any] = data.get(key)

  def set(key: Any, value: Any): Unit = data = data + (key -> value)
}

trait Context extends UserDataProvider {

  def begin: Int

  def end: Int

  def offset: Int

  def length: Int

  def markers: Seq[Marker]

  def addMarkers(marker: Marker, markers: Marker*): Unit

  def setStyle(style: Style): Unit

  def setTokenStyle(token: Token, style: Style): Unit

  def getStyle: Option[Style]

  def getTokenStyles: Seq[(Token, Style)]

  def abort(): Unit

  def hasAborted: Boolean

  val children: Seq[GenericNode]

  val contextualUserData: ContextualUserData
}
