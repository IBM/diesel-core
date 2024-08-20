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
import diesel.Marker.{Descriptor, Kind, Severity}
import diesel.i18n.Messages.KeyResolver
import diesel.i18n.{Lang}

import scala.language.implicitConversions

object Marker {

  case class Descriptor(kind: Kind.Value, severity: Severity.Value)

  object Kind extends Enumeration {

    val Lexical, Syntactic, Semantic = Value
  }

  object Severity extends Enumeration {

    val Info, Warning, Error = Value
  }

  def count(markers: Seq[Marker], severity: Severity.Value): Int =
    markers.count(m => m.descriptor.severity >= severity)

  def countErrors(markers: Seq[Marker]): Int = count(markers, Severity.Error)

}

trait MarkerMessage {
  def format(locale: String): String
}

object MarkerMessage {
  object Implicits {
    implicit def strToMsg(s: String): SimpleMarkerMessage = SimpleMarkerMessage(s)
  }
}

// could be moved to samples
case class SimpleMarkerMessage(message: String) extends MarkerMessage {
  override def format(locale: String): String = message
}

sealed trait InternalMsg extends MarkerMessage {
  def lang(locale: String): Lang.Value      = Lang(locale).getOrElse(Lang.EN)
  def resolver(locale: String): KeyResolver = DieselI18n.keyResolver(lang(locale))
}

case class MissingTokenMsg(token: String)                               extends InternalMsg {
  override def format(locale: String): String = {
    DieselI18n.missingToken(token)(resolver(locale))
  }
}
case class InsertedTokenMsg(token: String)                              extends InternalMsg {
  override def format(locale: String): String = {
    DieselI18n.insertedToken(token)(resolver(locale))
  }
}
case class TokenMutationMsg(actualToken: String, expectedToken: String) extends InternalMsg {
  override def format(locale: String): String = {
    DieselI18n.tokenMutation(expectedToken, actualToken)(resolver(locale))
  }
}
case class UnknownTokenMsg(token: String)                               extends InternalMsg {
  override def format(locale: String): String = {
    DieselI18n.unknownToken(token)(resolver(locale))
  }
}
case object AmbiguousMsg                                                extends InternalMsg {
  override def format(locale: String): String = {
    DieselI18n.ambiguous()(resolver(locale))
  }
}
case object IncompatibleMsg                                             extends InternalMsg {
  override def format(locale: String): String = {
    DieselI18n.incompatible()(resolver(locale))
  }
}

case class Marker(descriptor: Descriptor, offset: Int, length: Int, message: MarkerMessage) {
  override def toString: String = {
    s"${descriptor.kind} ${descriptor.severity} at ($offset, $length): $message"
  }
}

object Errors {

  val SyntacticError: Descriptor = Descriptor(Kind.Syntactic, Severity.Error)
  val LexicalError: Descriptor   = Descriptor(Kind.Lexical, Severity.Error)
  val SemanticError: Descriptor  = Descriptor(Kind.Semantic, Severity.Error)

  object MissingToken {

    def apply(offset: Int, token: Token): Marker = {
      Marker(SyntacticError, offset, 0, MissingTokenMsg(token.text))
    }
  }

  object InsertedToken {

    def apply(offset: Int, token: Token): Marker = {
      Marker(SyntacticError, offset, token.text.length, InsertedTokenMsg(token.text))
    }
  }

  object TokenMutation {

    def apply(offset: Int, actualToken: Token, expectedToken: Token): Marker = {
      Marker(
        SyntacticError,
        offset,
        actualToken.text.length,
        TokenMutationMsg(actualToken.text, expectedToken.text)
      )
    }
  }

  object UnknownToken {

    def apply(offset: Int, token: Token): Marker = {
      Marker(LexicalError, offset, token.text.length, UnknownTokenMsg(token.text))
    }
  }

  object Ambiguous {

    def apply(offset: Int, length: Int): Marker = {
      Marker(SemanticError, offset, length, AmbiguousMsg)
    }
  }

  object Incompatible {

    def apply(offset: Int, length: Int): Marker = {
      Marker(SemanticError, offset, length, IncompatibleMsg)
    }
  }
}
