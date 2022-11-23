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

package diesel.voc.i18n

import diesel.voc.{
  ArticleBuilder,
  DefaultArticleBuilder,
  DefaultLabelBuilder,
  DefaultVerbalizer,
  DefiniteArticle,
  DemonstrativeArticle,
  EachArticle,
  Female,
  Glossary,
  IndefiniteArticle,
  LabelBuilder,
  Male,
  Neutral,
  NoArticle,
  PluralBuilder,
  PossessiveArticle,
  QuantitativeArticle,
  Verbalizable,
  VerbalizationContext,
  Vocabulary
}

class EnglishVerbalizer(val voc: Vocabulary) extends DefaultVerbalizer {

  override val articleBuilder: ArticleBuilder = new EnglishArticleBuilder(voc)

  override def pluralBuilder: PluralBuilder = EnglishPluralBuilder

  override def labelBuilder: LabelBuilder = new DefaultLabelBuilder(this, voc.glossary)

}

class EnglishArticleBuilder(val voc: Vocabulary) extends DefaultArticleBuilder {

  override val glossary: Glossary = voc.glossary

  override def getSingularArticle(
    context: VerbalizationContext,
    verbalizable: Verbalizable
  ): Option[String] = {
    val res: Option[String] = defaultGetArticle(context, verbalizable) match {
      case Some(defaultArticle) =>
        Some(defaultArticle)
      case None                 =>
        context.article match {
          case NoArticle            =>
            None
          case IndefiniteArticle    =>
            val label = verbalizable.label
            if (EnglishUtil.isVowel(label.charAt(0))) {
              Some("an")
            } else {
              Some("a")
            }
          case DefiniteArticle      =>
            Some("the")
          case DemonstrativeArticle =>
            Some("this")
          case EachArticle          =>
            Some("each")
          case PossessiveArticle    =>
            Some(
              glossary.getTerm(verbalizable.label)
                .flatMap(_.gender)
                .map {
                  case Male    =>
                    "his"
                  case Female  =>
                    "her"
                  case Neutral =>
                    "its"
                }
                .getOrElse("its")
            )
          case QuantitativeArticle  =>
            Some("all")
        }
    }
    handlePartitive(res, context)
  }

  private def handlePartitive(res: Option[String], context: VerbalizationContext): Option[String] =
    res.map {
      r =>
        if (context.partitive) {
          "of" + (
            if (r.nonEmpty) " " + r else r
          )
        } else {
          r
        }
    }

  override def getPluralArticle(
    context: VerbalizationContext,
    verbalizable: Verbalizable
  ): Option[String] = {
    val res: Option[String] = defaultGetArticle(context, verbalizable) match {
      case s @ Some(_) =>
        s
      case None        =>
        context.article match {
          case NoArticle            =>
            None
          case DefiniteArticle      =>
            Some("the")
          case IndefiniteArticle    =>
            None
          case PossessiveArticle    =>
            val sentence = context.sentence
            val factType = sentence.flatMap(voc.getFactType)
            val owner    = factType.flatMap(_.getOwnerRole).isDefined
            if (owner) {
              val term = glossary.getTerm(verbalizable.label)
              term.flatMap(_.gender)
                .map {
                  case Male    =>
                    "his"
                  case Female  =>
                    "her"
                  case Neutral =>
                    "its"
                }
            } else {
              Some("its")
            }
          case DemonstrativeArticle =>
            Some("these")
          case QuantitativeArticle  =>
            Some("all")
          case EachArticle          =>
            None
        }
    }
    handlePartitive(res, context)
  }
}

object EnglishPluralBuilder extends PluralBuilder {
  override def getPlural(context: VerbalizationContext, verbalizable: Verbalizable): String = {
    val label = verbalizable.label
    val size  = label.length
    if (size > 1) {
      val lastChar = label.last
      lastChar match {
        case 'y' =>
          if (size >= 2 && !EnglishUtil.isVowel(label.charAt(size - 2))) {
            label + "ies"
          } else {
            label + "s"
          }
        case 's' =>
          label + "es"
        case _   =>
          label + "s"
      }
    } else {
      label + "s"
    }
  }
}

object EnglishUtil {

  private val VOWELS = "aeiouAEIOU".toSet

  def isVowel(char: Char): Boolean = VOWELS.contains(char)

}
