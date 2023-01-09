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

import diesel.Dsl.Concept

sealed trait Article {
  val name: String
}
case object NoArticle            extends Article {
  override val name: String = "NO_ARTICLE"
}
case object DefiniteArticle      extends Article {
  override val name: String = "DEFINITE_ARTICLE"
}
case object IndefiniteArticle    extends Article {
  override val name: String = "INDEFINITE_ARTICLE"
}
case object PossessiveArticle    extends Article {
  override val name: String = "POSSESSIVE_ARTICLE"
}
case object DemonstrativeArticle extends Article {
  override val name: String = "DEMONSTRATIVE_ARTICLE"
}
case object QuantitativeArticle  extends Article {
  override val name: String = "QUANTITATIVE_ARTICLE"
}
case object EachArticle          extends Article {
  override val name: String = "EACH_ARTICLE"
}

object Article {

  def fromString(s: String): Option[Article] = s match {
    case NoArticle.name            =>
      Some(NoArticle)
    case DefiniteArticle.name      =>
      Some(DefiniteArticle)
    case IndefiniteArticle.name    =>
      Some(IndefiniteArticle)
    case PossessiveArticle.name    =>
      Some(PossessiveArticle)
    case DemonstrativeArticle.name =>
      Some(DemonstrativeArticle)
    case QuantitativeArticle.name  =>
      Some(QuantitativeArticle)
    case EachArticle.name          =>
      Some(EachArticle)
    case _                         =>
      None
  }

}

case class VerbalizationContext(
  article: Article = NoArticle,
  plural: Boolean = false,
  partitive: Boolean = false,
  props: Map[String, String] = Map.empty,
  useTermProperties: Boolean = true
) {
  def getProperty(name: String): Option[String] = props.get(name)
}

trait Verbalizer {
  def verbalize(context: VerbalizationContext, concept: Concept[_]): String
  def verbalize(context: VerbalizationContext, label: String): String
}
