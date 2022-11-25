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

import diesel.Dsl
import diesel.Dsl.{Associativity, Instance, Phrase, SPAndN, SPStr, Syntax, Concept => DslConcept}
import diesel.Lexer.Token
import diesel.voc.Ast.DslConceptKey
import diesel.voc.Ast

import diesel.i18n.DeclaringSourceName

import scala.reflect.classTag

trait VocDsl extends Dsl {

  def vocabulary: Vocabulary
  def verbalizer: Verbalizer
  def precedences: Map[String, (Associativity.Value, Int)] = Map(
    ("+", (Associativity.Left, 100))
  )

  protected def concept(conceptId: String): DslConcept[Ast.Expr] =
    dslConcepts(DslConceptKey(conceptId))

  private def mapPhrase(ft: FactType, sentence: Sentence)(vs: Seq[Ast.Expr]): Ast.Expr = {
    val indexed = vs.toIndexedSeq
    val args    = ft.roles.flatMap(r => indexed.lift(r.index))
    val subject = sentence.getSubject match {
      case Some(synRole) => ft.roles.find(r => r.index == synRole.roleIndex) match {
          case Some(role) => (role.conceptId, synRole.cardinality)
          case None       => ("ilog.rules.brl.Void", Single)
        }
      case None          => ("ilog.rules.brl.Void", Single)
    }
    Ast.Phrase(ft.name, sentence.category, subject._1, subject._2 == Multiple, args)
  }

  private val lookup: Map[String, Concept] = vocabulary.concepts.map(c => c.identifier -> c).toMap

  private val dslConcepts: Map[DslConceptKey, DslConcept[Ast.Expr]] = vocabulary.concepts
    .foldLeft(Map.empty[DslConceptKey, DslConcept[Ast.Expr]]) { case (acc, c) =>
      val more = createConcept(acc)(c)
      acc ++ more
    }

  private def createConcept(
    concepts: Map[DslConceptKey, DslConcept[Ast.Expr]],
    scope: Option[String] = None
  )(c: Concept): Seq[(DslConceptKey, DslConcept[Ast.Expr])] = {
    val key        = DslConceptKey(c.identifier)
    val sourceName = scope match {
      case Some(value) => DeclaringSourceName(s"${value}-${key}")
      case None        => DeclaringSourceName(s"${key}")
    }
    if (c.parentIds.isEmpty) {
      Seq(key -> concept[Ast.Expr](classTag[Ast.Expr], sourceName))
    } else {
      // TODO multiple inheritance?
      val parent = c.parentIds.headOption
        .map(DslConceptKey)
        .flatMap(concepts.get)
      if (parent.isDefined) {
        Seq(key -> concept[Ast.Expr, Ast.Expr](parent.get)(classTag[Ast.Expr], sourceName))
      } else {
        val created = c.parentIds.headOption.flatMap(lookup.get)
          .map(createConcept(concepts)(_))
          .getOrElse(Seq())
        val parent  = c.parentIds.headOption
          .flatMap(pid => created.find(_._1 == DslConceptKey(pid))).map(_._2)
        parent
          .map(p =>
            created :+ key -> concept[Ast.Expr, Ast.Expr](p)(classTag[Ast.Expr], sourceName)
          )
          .getOrElse(Seq())
      }
    }
  }

//  // TODO avoid counter dedupCounter
  var dedupCounter                       = 0
  private val dslPhrases: Seq[Phrase[_]] = vocabulary.factTypes.flatMap { ft =>
    ft.sentences.map { s =>
      implicit val sourceName = DeclaringSourceName("voc" + dedupCounter)
      dedupCounter += 1
      val dslResultConcept    = VocDslUtils.useConcept(s.getSubject, ft.roles, dslConcepts)
      val production          = VocDslUtils.toPhraseProduction(verbalizer, precedences)(
        s,
        ft.roles,
        VerbalizationContext(article = DefiniteArticle),
        dslConcepts
      )(mapPhrase(ft, s))
      if (s.syntacticRoles.last.cardinality == Multiple) {
        phraseMultiple(dslResultConcept)(production)
      } else {
        phrase(dslResultConcept)(production)
      }
    }
  }

  private val dslConceptInstances: Seq[Instance[Ast.Expr]] = vocabulary.conceptInstances.map { ci =>
    implicit val sourceName = DeclaringSourceName("voc" + dedupCounter)
    dedupCounter += 1
    val dslResultConcept    = dslConcepts(DslConceptKey(ci.conceptId))
    instance(dslResultConcept)(ci.name) map { _ => Ast.Instance(ci.conceptId, ci.identifier) }
  }

  // generate implicit variables for plural subjects, to be used with indefinite singular
  // TODO implicit this
//  private val dslSyntaxes_implicits: Seq[Syntax[Ast.Expr]] = vocabulary.factTypes
//    .filter(ft =>
//      ft.sentences.exists(s => s.syntacticRoles.exists(sr => sr.category == Subject && sr.cardinality == Multiple))
//    )
//    .flatMap(_.getOwnerRole)
//    .map { holder =>
//      val dslResultConcept = dslConcepts(DslConceptKey(holder.conceptId))
//      val vc               = VerbalizationContext(article = DemonstrativeArticle, plural = false)
//      val text             = verbalizer.verbalize(
//        vc,
//        RoleVerbalizable(holder)
//      )
//      syntax(dslResultConcept)(SPStr(text) map { case _ => Ast.SyntaxExpr("variable", ) }) // TODO SPStr(text) must be split into several words
//    }

  // Temporary in order to have some variables available to do test
  private val dslSyntaxes_implicits: Seq[Syntax[Ast.Expr]] = vocabulary.concepts map { concept =>
    implicit val sourceName = DeclaringSourceName("voc" + dedupCounter)
    dedupCounter += 1
    val dslResultConcept    = dslConcepts(DslConceptKey(concept.identifier))
    val vc                  = VerbalizationContext(article = DemonstrativeArticle)
    val text                = verbalizer.verbalize(vc, ConceptVerbalizable(concept))
    val words               = text.trim().split(" ").map(t => SPStr(t))
    val production          = if (words.length > 1) SPAndN(words.toSeq) else words(0)
    syntax(dslResultConcept)(production map { case _ =>
      Ast.SyntaxExpr("this", concept.identifier, multiple = false, Seq())
    })
  }

}

private object VocDslUtils {

  def getRole(index: Int, roles: Seq[Role]): Role = roles.find(_.index == index).get

  private case class MyHandler(roles: Seq[Role]) extends SentenceProcessor.Handler {

    private val parts: collection.mutable.ArrayBuffer[SentencePart] =
      new collection.mutable.ArrayBuffer()

    def getParts(): Seq[SentencePart] = parts.toSeq

    private var current: Option[Sentence] = None

    override def processSentence(sentence: Sentence, context: VerbalizationContext): Unit = {
      parts.clear()
      current = Some(sentence)
    }

    override def processSyntacticRole(
      syntacticRole: SyntacticRole,
      context: VerbalizationContext
    ): Unit = {
      syntacticRole.category match {
        case Object  =>
          parts.append(ObjectRolePart(syntacticRole))
        case Subject =>
          // TODO they can be different?
          // (syntacticRole.cardinality == Multiple) == context.plural
          parts.append(SubjectRolePart(
            getRole(syntacticRole.roleIndex, roles),
            plural = syntacticRole.cardinality == Multiple
          ))
      }
    }

    override def processText(text: String): Unit = {
      val words = text.trim().split(" ").map(TextPart)
      parts.appendAll(words)
    }
  }

  private trait SentencePart {
    def isText: Boolean = false
    def isRole: Boolean = false
  }
  private case class TextPart(text: String) extends SentencePart {
    override def isText: Boolean = true
  }
  private case class ObjectRolePart(role: SyntacticRole) extends SentencePart {
    override def isRole: Boolean = true
  }
  private case class SubjectRolePart(role: Role, plural: Boolean) extends SentencePart {
    override def isRole: Boolean = true
  }

  private def toParts(
    verbalizer: Verbalizer,
    sentence: Sentence,
    roles: Seq[Role],
    context: VerbalizationContext
  ): Seq[SentencePart] = {
    val handler = MyHandler(roles)
    SentenceProcessor.processSentence(
      verbalizer,
      sentence,
      context,
      handler
    )
    handler.getParts()
  }

  def useConcept[T](
    syntacticRole: Option[SyntacticRole],
    roles: Seq[Role],
    dslConcepts: Map[DslConceptKey, DslConcept[T]]
  ): DslConcept[T] = {
    val conceptId = syntacticRole match {
      case Some(value) => roles.find(_.index == value.roleIndex).map(_.conceptId).getOrElse(
          throw new IllegalStateException("impossible")
        )
      case None        => "ilog.rules.brl.Void"
    }
    val key       = DslConceptKey(conceptId)
    dslConcepts.getOrElse(
      key,
      throw new IllegalStateException(s"missing Dsl concept ${key}")
    )
  }

  import Dsl._

  def toPhraseProduction[T, S](
    verbalizer: Verbalizer,
    precedences: Map[String, (Associativity.Value, Int)]
  )(
    sentence: Sentence,
    roles: Seq[Role],
    context: VerbalizationContext,
    dslConcepts: Map[DslConceptKey, DslConcept[T]]
  )(f: Seq[T] => S): PhraseProduction[S] = {
    val parts = toParts(verbalizer, sentence, roles, context).toIndexedSeq
    PPAndN(parts.zipWithIndex
      .map {
        case (TextPart(t), index)               =>
          precedences.get(t) match {
            case Some(value) =>
              if (index == 0) {
                if (parts.isDefinedAt(index + 1)) {
                  if (parts.apply(index + 1).isRole && parts.length == 2) {
                    PPAssoc(PPStr(t), value._1, value._2)
                  } else {
                    PPStr(t)
                  }
                } else {
                  PPStr(t)
                }
              } else {
                if (parts.apply(index - 1).isRole) {
                  if (parts.isDefinedAt(index + 1)) {
                    if (parts.apply(index + 1).isRole && parts.length == 3) {
                      PPAssoc(PPStr(t), value._1, value._2)
                    } else {
                      PPStr(t)
                    }
                  } else {
                    if (parts.length == 2) {
                      PPAssoc(PPStr(t), value._1, value._2)
                    } else {
                      PPStr(t)
                    }
                  }
                } else {
                  PPStr(t)
                }
              }
            case None        => PPStr(t)
          }
        case (SubjectRolePart(role, plural), _) =>
          // TODO plural?
          PPStr(role.label.getOrElse("?")).subject
        case (ObjectRolePart(syntacticRole), _) =>
          val ref = PPExprRef[T](useConcept(Some(syntacticRole), roles, dslConcepts))
          if (syntacticRole.cardinality == Multiple) {
            ref
              .multiple[T]
            // TODO ?
//              .verbalization(SPVerbalizationContext(article = Some(DefiniteArticle)))
//              .verbalization(SPVerbalizationContext(plural = Some(false), article = Some(DefiniteArticle)))
//              .verbalization(SPVerbalizationContext(plural = Some(true), article = Some(DefiniteArticle)))
          } else {
            ref
              .verbalization(SPVerbalizationContext(article = Some(DefiniteArticle)))
          }
      })
      .map {
        case (_, seq) => f(seq.filterNot(isToken).map(_.asInstanceOf[T]))
      }
  }

  private def isToken(a: Any): Boolean = {
    a.isInstanceOf[Token] || (a.isInstanceOf[Seq[_]] && a.asInstanceOf[Seq[_]].forall(
      _.isInstanceOf[Token]
    ))
  }

}
