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

package diesel.voc

object Ast {

  sealed trait VocNode {
    val conceptId: String;
//    val multiple: Boolean;
  }

  case class Instance(conceptId: String, name: String) extends VocNode {
    val multiple: Boolean = false
  }
//
//  case class SyntaxExpr(
//    syntaxId: String,
//    conceptId: String,
//    multiple: Boolean,
//    children: Seq[VocNode]
//  ) extends VocNode

  case class Phrase(
    factTypeId: String,
    sentenceCategory: SentenceCategory,
    conceptId: String,
    multiple: Boolean,
    children: Seq[VocNode]
  ) extends VocNode

  case class DslConceptKey(conceptId: String) {
    override def toString: String = conceptId
  }

  trait CustomVocNode extends VocNode

}
