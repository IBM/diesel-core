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

sealed trait Verbalizable {
  def label: String
//  def getTerm(glossary: Glossary): Option[Term] = glossary.getTerm(label)
}

object UnspecifiedVerbalizable {
  val LABEL: String = "<...>"
}

case class UnspecifiedVerbalizable(labelOpt: Option[String] = None) extends Verbalizable {
  override def label: String = labelOpt.getOrElse(UnspecifiedVerbalizable.LABEL)
}

case class ConceptVerbalizable(concept: Concept) extends Verbalizable {
  override def label: String = concept.name
}

case class ConceptInstanceVerbalizable(instance: ConceptInstance) extends Verbalizable {
  override def label: String = instance.name
}

case class RoleVerbalizable(role: Role) extends Verbalizable {
  override def label: String = role.label.getOrElse("???")
}

case class LabelVerbalizable(label: String) extends Verbalizable

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
  sentence: Option[Sentence] = None,
  article: Article = NoArticle,
  plural: Boolean = false,
  partitive: Boolean = false,
  props: Map[String, String] = Map.empty,
  useTermProperties: Boolean = true
) {
  def getProperty(name: String): Option[String] = props.get(name)
}

trait ArticleBuilder {
  def getArticle(context: VerbalizationContext, verbalizable: Verbalizable): Option[String]
}

trait PluralBuilder {
  def getPlural(context: VerbalizationContext, verbalizable: Verbalizable): String
}

trait LabelBuilder {
  def getDisplayLabel(context: VerbalizationContext, verbalizable: Verbalizable): String
  def getDisplayPluralLabel(context: VerbalizationContext, verbalizable: Verbalizable): String
}

case class Verbalization(text: String, articleOffset: Int, articleLength: Int)

trait Verbalizer {

  def verbalize(context: VerbalizationContext, verbalizable: Verbalizable): String =
    verbalizeTerm(context, verbalizable).text
  def verbalizeTerm(context: VerbalizationContext, verbalizable: Verbalizable): Verbalization
  def articleBuilder: ArticleBuilder
  def pluralBuilder: PluralBuilder
  def labelBuilder: LabelBuilder
  //  def getPlural(context: VerbalizationContext, verbalizable: Verbalizable): String
  //  def getDisplayLabel(context: VerbalizationContext, verbalizable: Verbalizable): String
  //  def getDisplayPluralLabel(context: VerbalizationContext, verbalizable: Verbalizable): String
}

trait DefaultVerbalizer extends Verbalizer {

  override def verbalizeTerm(
    context: VerbalizationContext,
    verbalizable: Verbalizable
  ): Verbalization = {
    val label   =
      if (context.plural) {
        labelBuilder.getDisplayPluralLabel(context, verbalizable)
      } else {
        labelBuilder.getDisplayLabel(context, verbalizable)
      }
    val prepend = isRTLVerbalizer && shouldInsert(context.article)

    val builder = new StringBuilder()
    if (prepend) {
      builder.append(label)
      builder.append(' ')
    }

    var articleOffset = 0
    var articleLength = 0
    articleBuilder.getArticle(context, verbalizable).foreach { article =>
      articleOffset = builder.length()
      articleLength = article.length
      builder.append(article)
      if (!isRTLVerbalizer || shouldAddSpace(context.article)) {
        builder.append(" ")
      }
    }

    if (!prepend) {
      builder.append(label)
    }

    val text        = builder.toString()
    val trimmedText = text.trim
    val index       = text.indexOf(trimmedText)
    if (index >= 0) {
      articleOffset -= Math.max(0, index)
    }
    if (articleOffset + articleLength > trimmedText.length) {
      articleLength = trimmedText.length - articleOffset
    }
    Verbalization(trimmedText, articleOffset, articleLength)
  }

  protected val isRTLVerbalizer: Boolean = false

  protected def shouldInsert(article: Article): Boolean = false

  protected def shouldAddSpace(article: Article): Boolean = true
}

trait DefaultArticleBuilder extends ArticleBuilder {

  val glossary: Glossary

  override def getArticle(
    context: VerbalizationContext,
    verbalizable: Verbalizable
  ): Option[String] = {
    if (context.plural) {
      getPluralArticle(context, verbalizable)
    } else {
      getSingularArticle(context, verbalizable)
    }
  }

  def getPluralArticle(context: VerbalizationContext, verbalizable: Verbalizable): Option[String]

  def getSingularArticle(context: VerbalizationContext, verbalizable: Verbalizable): Option[String]

  def defaultGetArticle(
    context: VerbalizationContext,
    verbalizable: Verbalizable
  ): Option[String] = {
    if (context.article == NoArticle) {
      None
    } else {
      if (context.useTermProperties) {
        glossary.getTerm(verbalizable.label).flatMap { term =>
          term.getTermPropertyValue(Glossary.getArticlePropertyName(context))
            .filter(_ != "NO_ARTICLE")
        }
      } else {
        None
      }
    }
  }

}

class DefaultLabelBuilder(val verbalizer: Verbalizer, val glossary: Glossary) extends LabelBuilder {

  override def getDisplayLabel(context: VerbalizationContext, verbalizable: Verbalizable): String =
    verbalizable.label

  override def getDisplayPluralLabel(
    context: VerbalizationContext,
    verbalizable: Verbalizable
  ): String = {
    glossary.getTerm(verbalizable.label)
      .flatMap(_.pluralLabel)
      .getOrElse(verbalizer.pluralBuilder.getPlural(context, verbalizable))
  }
}
