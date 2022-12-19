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

  sealed trait Expr {
    val conceptId: String
    val multiple: Boolean
  }

  case class Target(conceptId: String, multiple: Boolean) extends Expr

  case class Value[T](value: T, conceptId: String) extends Expr {
    override val multiple: Boolean = false
  }

  case class Instance(conceptId: String, name: String) extends Expr {
    override val multiple: Boolean = false
  }

  case class SyntaxExpr(syntaxId: String, conceptId: String, multiple: Boolean, children: Seq[Expr])
      extends Expr

  case class Phrase(
    factTypeId: String,
    sentenceCategory: SentenceCategory,
    conceptId: String,
    multiple: Boolean,
    children: Seq[Expr]
  ) extends Expr

  case class DslConceptKey(conceptId: String) {
    override def toString: String = conceptId
  }
}
