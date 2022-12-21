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

trait VocElement

case class Locale(language: String, country: Option[String])

object Locale {
  val ENGLISH: Locale = Locale("en", None)
  val US: Locale      = Locale("en", Some("US"))
}

case class Concept(name: String, identifier: String, parentIds: Seq[String]) extends VocElement

case class ConceptInstance(name: String, identifier: String, conceptId: String) extends VocElement

case class FactType(name: String, roles: Seq[Role], sentences: Seq[Sentence]) {
  def getOwnerRole: Option[Role] = roles.find(_.holder)
}

sealed trait SentenceCategory {
  val parent: Option[SentenceCategory] = None
}

object SentenceCategory {
  abstract case class DerivedSentenceCategory(p: SentenceCategory) extends SentenceCategory {
    override val parent: Option[SentenceCategory] = Some(p)
  }
  case object Navigation                                           extends SentenceCategory
  case object Operator                                             extends SentenceCategory
  case object Action                                               extends SentenceCategory

  object Getter            extends DerivedSentenceCategory(Navigation)
  object Multiple          extends DerivedSentenceCategory(Navigation)
  object Predicate         extends DerivedSentenceCategory(Getter)
  object NegativePredicate extends DerivedSentenceCategory(Getter)
  object Simplified        extends DerivedSentenceCategory(Navigation)
  object Self              extends DerivedSentenceCategory(Navigation)

  object Setter                  extends DerivedSentenceCategory(Action)
  object Clearer                 extends DerivedSentenceCategory(Action)
  object PredicateSetter         extends DerivedSentenceCategory(Setter)
  object PositivePredicateSetter extends DerivedSentenceCategory(Setter)
  object NegativePredicateSetter extends DerivedSentenceCategory(Setter)
  object AdditionSetter          extends DerivedSentenceCategory(Action)
  object SubtractionSetter       extends DerivedSentenceCategory(Action)
  object CollectionAdd           extends DerivedSentenceCategory(Action)
  object CollectionRemove        extends DerivedSentenceCategory(Action)
  object CollectionClear         extends DerivedSentenceCategory(Action)

  def fromString(text: String): Option[SentenceCategory] = text match {
    case "NAVIGATION" => Some(Navigation)
    case "OPERATOR"   => Some(Operator)
    case "ACTION"     => Some(Action)

    case "GETTER"             => Some(Getter)
    case "MULTIPLE"           => Some(Multiple)
    case "PREDICATE"          => Some(Predicate)
    case "NEGATIVE_PREDICATE" => Some(NegativePredicate)
    case "SIMPLIFIED"         => Some(Simplified)
    case "SELF"               => Some(Self)

    case "SETTER"                    => Some(Setter)
    case "CLEARER"                   => Some(Clearer)
    case "PREDICATE_SETTER"          => Some(PredicateSetter)
    case "POSITIVE_PREDICATE_SETTER" => Some(PositivePredicateSetter)
    case "NEGATIVE_PREDICATE_SETTER" => Some(NegativePredicateSetter)
    case "ADDITION_SETTER"           => Some(AdditionSetter)
    case "SUBTRACTION_SETTER"        => Some(SubtractionSetter)
    case "COLLECTION_ADD"            => Some(CollectionAdd)
    case "COLLECTION_REMOVE"         => Some(CollectionRemove)
    case "COLLECTION_CLEAR"          => Some(CollectionClear)

    case _ => None
  }
}

case class Sentence(
  category: SentenceCategory,
  template: String,
  syntacticRoles: Seq[SyntacticRole]
) extends VocElement {
  def getSyntacticRole(index: Int): Option[SyntacticRole] = syntacticRoles.lift(index)

  def getSubject: Option[SyntacticRole] = syntacticRoles.find(r => r.category == Subject)
}

sealed trait Cardinality {
  val name: String
}

case object Single extends Cardinality {
  override val name: String = "SINGLE"
}

case object Multiple extends Cardinality {
  override val name: String = "MULTIPLE"
}

object Cardinality {
  def fromString(text: String): Option[Cardinality] = text match {
    case "SINGLE"   => Some(Single)
    case "MULTIPLE" => Some(Multiple)
    case _          => None
  }
}

sealed trait SyntacticRoleCategory
object Object  extends SyntacticRoleCategory
object Subject extends SyntacticRoleCategory

object SyntacticRoleCategory {
  def fromString(text: String): Option[SyntacticRoleCategory] = text match {
    case "OBJECT"  => Some(Object)
    case "SUBJECT" => Some(Subject)
    case _         => None
  }
}

case class SyntacticRole(category: SyntacticRoleCategory, cardinality: Cardinality, roleIndex: Int)

case class Role(holder: Boolean, label: Option[String], conceptId: String, index: Int)

sealed trait Gender
case object Male    extends Gender
case object Female  extends Gender
case object Neutral extends Gender

object Gender {
  val MALE    = "MALE"
  val FEMALE  = "FEMALE"
  val NEUTRAL = "NEUTRAL"

  def fromString(text: String): Option[Gender] = text match {
    case MALE    => Some(Male)
    case FEMALE  => Some(Female)
    case NEUTRAL => Some(Neutral)
    case _       => None
  }
}

case class Term(
  label: String,
  termProperties: Map[String, String] = Map.empty
) {
  def getTermPropertyValue(key: String): Option[String] = termProperties.get(key)
  def pluralLabel: Option[String]                       = termProperties.get(Glossary.TERM_PLURAL_LITERAL)
  def gender: Option[Gender]                            =
    termProperties.get(Glossary.TERM_GENDER_LITERAL).flatMap(Gender.fromString)
}

case class Glossary(terms: Seq[Term]) {
  def getTerm(label: String): Option[Term] = terms.find(_.label == label)
}

object Glossary {
  val TERM_PLURAL_LITERAL                    = "plural"
  val TERM_GENDER_LITERAL                    = "gender"
  val SINGULAR_ARTICLE_SUFFIX                = "_SINGULAR"
  val PLURAL_ARTICLE_SUFFIX                  = "_PLURAL"
  val SINGULAR_DEFINITE_ARTICLE: String      = getArticlePropertyName(DefiniteArticle, isPlural = false)
  val PLURAL_DEFINITE_ARTICLE: String        = getArticlePropertyName(DefiniteArticle, isPlural = true)
  val SINGULAR_INDEFINITE_ARTICLE: String    =
    getArticlePropertyName(IndefiniteArticle, isPlural = false)
  val PLURAL_INDEFINITE_ARTICLE: String      = getArticlePropertyName(IndefiniteArticle, isPlural = true)
  val SINGULAR_QUANTITATIVE_ARTICLE: String  =
    getArticlePropertyName(QuantitativeArticle, isPlural = false)
  val PLURAL_QUANTITATIVE_ARTICLE: String    =
    getArticlePropertyName(QuantitativeArticle, isPlural = true)
  val SINGULAR_DEMONSTRATIVE_ARTICLE: String =
    getArticlePropertyName(DemonstrativeArticle, isPlural = false)
  val PLURAL_DEMONSTRATIVE_ARTICLE: String   =
    getArticlePropertyName(DemonstrativeArticle, isPlural = true)
  val EACH_DEMONSTRATIVE_ARTICLE: String     = getArticlePropertyName(EachArticle, isPlural = false)
  val POSSESSIVE_ARTICLE: String             = getArticlePropertyName(PossessiveArticle, isPlural = false)

  def getArticlePropertyName(context: VerbalizationContext): String =
    getArticlePropertyName(context.article, context.plural)

  def getArticlePropertyName(article: Article, isPlural: Boolean): String =
    s"${article.name}${if (isPlural) PLURAL_ARTICLE_SUFFIX else SINGULAR_ARTICLE_SUFFIX}"

  def empty: Glossary = Glossary(Seq.empty)
}

case class Vocabulary(
  glossary: Glossary,
  concepts: Seq[Concept] = Seq.empty,
  conceptInstances: Seq[ConceptInstance] = Seq.empty,
  factTypes: Seq[FactType] = Seq.empty
) {
  def getFactType(sentence: Sentence): Option[FactType] = {
    factTypes.find { ft =>
      ft.sentences.contains(sentence)
    }
  }

  def getConcept(conceptId: String): Option[Concept] = concepts.find(_.identifier == conceptId)

  def isAssignableFrom(parent: Concept, child: Concept): Boolean = {
    if (child.identifier == parent.identifier) {
      true
    } else {
      child.parentIds.exists { pId =>
        getConcept(pId) match {
          case Some(p) =>
            isAssignableFrom(parent, p)
          case None    =>
            false
        }
      }
    }
  }

}

object Vocabulary {

  object Constants {

    val PARTITIVE_ARTICLE: String = "PARTITIVE_ARTICLE"
    val NO_ARTICLE: String        = "NO_ARTICLE"
    val PLURAL_UNDEFINED: String  = "PLURAL_UNDEFINED"

  }

  val empty: Vocabulary = Vocabulary(Glossary.empty)

}
