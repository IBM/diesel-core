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

import diesel.AstHelpers.predict
import diesel.Dsl.{
  Axiom,
  Concept,
  Expressions,
  Identifiers,
  Instance,
  Syntax,
  SyntaxGeneric,
  SyntaxTyped
}
import munit.FunSuite
import diesel.Bnf.{
  DslAxiom,
  DslBody,
  DslElement,
  DslInstance,
  DslSyntax,
  DslTarget,
  DslValue,
  ElementType
}

import scala.util.matching.Regex

class PredictionPropagationTest extends FunSuite {

  trait AValue
  trait ANumber                           extends AValue
  trait ABoolean                          extends AValue
  trait AString                           extends AValue
  case class ANumberValue(value: String)  extends ANumber
  case class ABooleanValue(value: String) extends ABoolean
  case class AStringValue(value: String)  extends AString

  case class AIs(left: AValue, right: AValue)             extends ABoolean
  case class AIsOneOf(left: AValue, right: AValue)        extends ABoolean
  case class AConcat(left: AValue, right: AValue)         extends AString
  case class AElvis(left: AValue, right: AValue)          extends AValue
  case class AVarRef(name: String)                        extends AValue
  case class AAnd(left: AValue, right: AValue)            extends ABoolean
  case class ListLiteral(head: AValue, tail: Seq[AValue]) extends AValue

  trait BootVoc extends Dsl with Identifiers {

    val value: Concept[AValue] = concept

    val number: Concept[ANumber] =
      concept[ANumber, AValue]("\\d+(\\.\\d+)?".r, ANumberValue("0"), Some(value)) map {
        case (_, t) =>
          ANumberValue(t.text)
      }

    val boolean: Concept[ABoolean] = concept(value)

    val booleanTrue: Instance[ABoolean] = instance(boolean)("true") map { _ =>
      ABooleanValue("true")
    }

    val booleanFalse: Instance[ABoolean] = instance(boolean)("false") map { _ =>
      ABooleanValue("false")
    }

    val string: Concept[AString] =
      concept[AString, AValue]("\"([^\"\\\\]|\\\\.)*\"".r, AStringValue(""), Some(value)) map {
        case (_, t) =>
          AStringValue(t.text.drop(1).dropRight(1))
      }

    val concat: Syntax[AString] = syntax(string)(
      number ~ "+".leftAssoc(30) ~ string map {
        case (_, (lhs, _, rhs)) =>
          AConcat(lhs, rhs)
      }
    )

    val is: Syntax[ABoolean] = syntax(boolean)(
      value ~ "is".leftAssoc(20) ~ value map {
        case (_, (lhs, _, rhs)) =>
          AIs(lhs, rhs)
      }
    )

    val isOneOf: Syntax[ABoolean] = syntax(boolean)(
      value ~ "is" ~ "one" ~ "of" ~ value.multiple[AValue] map {
        case (_, (lhs, _, _, _, rhs)) =>
          AIsOneOf(lhs, rhs)
      }
    )

    val elvis: SyntaxGeneric[AValue] =
      syntaxGeneric[AValue].accept(value) { builder =>
        builder(
          builder.concept ~ "?:".leftAssoc(10) ~ builder.concept map {
            case (_, (l, _, r)) =>
              AElvis(l, r)
          }
        )
      }

    val and: Syntax[ABoolean] = syntax(boolean)(
      boolean ~ "&&".leftAssoc(25) ~ boolean map {
        case (_, (lhs, _, rhs)) =>
          AAnd(lhs, rhs)
      }
    )

    def Variables: Expressions.Custom = Expressions.Custom("Variables")

    override def defaultExprs: Set[Expressions.Type] = Set(
      Expressions.Values,
      Expressions.Instances,
      Expressions.Syntaxes,
      Expressions.Targets,
      Variables
    )

    def identRegex: Regex                    = "[a-zA-Z_\\\\$][a-zA-Z0-9_\\\\$]*".r
    override def identScanner: Lexer.Scanner = identRegex

    val variableId = "variable tradfs"

    val literalListId = "literalList tradfs"

    val variableRef: SyntaxGeneric[AValue] = {
      syntaxGeneric[AValue]
        .accept((_: Dsl.Concept[AValue], types: Dsl.Expressions.Types, _: Dsl) =>
          types.has(Variables)
        ) {
          builder =>
            builder.userData(variableId) {
              idOrKeyword map {
                case (ctx, name) =>
                  if (!Set("x", "toto").contains(name.text)) { // TODO variable scope and check type
                    ctx.addMarkers(createMarker(NotAVariable, ctx.offset, ctx.length))
                  }
                  AVarRef(name.text)
              }
            }
        }
    }

    val listLiteral: Dsl.SyntaxGenericMulti[AValue, AValue] = {
      syntaxGeneric[AValue]
        .accept(value)
        .multi[AValue] { builder =>
          builder.userData(literalListId) {
            (("{" ~ builder.concept) ~ ("," ~ builder.concept).rep(true) ~ "}") map {
              case (_, (_, e, es, _)) =>
                ListLiteral(e, es.map(_._2))
            }
          }
        }
    }

    object NotAVariable extends MarkerMessage {
      def format(locale: String): String = ???
    }

    private def createMarker(m: MarkerMessage, o: Int, l: Int): Marker =
      Marker(Errors.SemanticError, o, l, m)
  }

  object MyDsl extends BootVoc {

    val expr: Axiom[AValue] = axiom(value)

    case class MyComputeFilter(expectedType: Concept[_], variables: Map[String, Concept[_]])
        extends CompletionComputeFilter {

      var visitedTypes = Set.empty[Concept[_]]

      def isDeclaredVariable(
        subIndex: Int,
        e: DslElement,
        predictionState: PredictionState
      ): Boolean = {
        val texts = predictionState.textsAt(subIndex)
        e.elementType match {
          case Some(ElementType(concept, _)) =>
            texts.exists(t =>
              variables.get(t).exists(c => MyDsl.isSubtypeOf(c, concept))
            )
          case None                          => false
        }
      }

      override def initVisit(predictionState: PredictionState): Seq[PredictionState] = {
        def isLiteralList(element: DslElement): Boolean = element match {
          case DslSyntax(syntax) => syntax.userData.contains(MyDsl.literalListId)
          case _                 => false
        }

        if (predictionState.element.exists(isLiteralList)) {
          predictionState.predecessorStates
        } else {
          val precedingListLiterals =
            predictionState.predecessorStates.filter(_.element.exists(isLiteralList))
          if (precedingListLiterals.nonEmpty) {
            precedingListLiterals
          } else
            super.initVisit(predictionState)
        }
      }

      def beginVisit(predictionState: PredictionState): Boolean = {
        visitedTypes = Set.empty[Concept[_]]
        if (predictionState.isAxiom) {
          visitedTypes = Set(expectedType)
          true
        } else {
          val accept =
            predictionState.leftSubIndex.forall { i =>
              val elements        = predictionState.elementsAt(i)
              val allVariableRefs = elements.forall {
                case DslSyntax(syntax) => syntax.userData.contains(MyDsl.variableId)
                case _                 => false
              }
              if (allVariableRefs)
                elements.exists(e => isDeclaredVariable(i, e, predictionState))
              else
                true
            }
          if (accept) {
            val propagate = predictionState.element match {
              case Some(DslSyntax(syntax)) if syntax == MyDsl.is      => true
              case Some(DslSyntax(syntax)) if syntax == MyDsl.isOneOf => true
              case Some(DslSyntax(syntax: SyntaxTyped[_]))            => syntax.name == "elvis"
              case _                                                  => false
            }
            if (propagate) {
              val propagates = predictionState.subIndex.filter(_ > 0)
                .map(_ =>
                  predictionState.elementsAt(0)
                    .filter {
                      case DslSyntax(syntax) => !syntax.userData.contains(MyDsl.variableId)
                      case _                 => true
                    }
                    .flatMap(_.elementType).map(_.concept)
                )
                .toSeq.flatten
              visitedTypes = visitedTypes ++ propagates
            }
            true
          } else { false }
        }
      }

      private def isContinue(concept: Concept[_]): Boolean = {
        this.visitedTypes.isEmpty || this.visitedTypes.exists(visited =>
          MyDsl.isSubtypeOf(visited, concept)
        )
      }

      private def isExpected(concept: Concept[_]): Boolean = {
        this.visitedTypes.isEmpty || this.visitedTypes.exists(visited =>
          MyDsl.isSubtypeOf(concept, visited) || MyDsl.isSubtypeOf(visited, concept)
        )
      }

      // axiom
      // value "+" value
      // foo "xx" bar

      override def continueVisit(
        element: DslElement
      ): Boolean = {
        element match {
          case DslInstance(_)                             => true
          case DslTarget(_)                               => true
          case DslValue(_)                                => true
          case DslBody(DslSyntax(syntax: SyntaxTyped[_])) =>
            if (isContinue(syntax.concept)) {
              if (visitedTypes.nonEmpty) {
                if (syntax == MyDsl.is) {
                  this.visitedTypes = this.visitedTypes + MyDsl.value
                }
                if (syntax == MyDsl.concat) {
                  this.visitedTypes = this.visitedTypes + MyDsl.number
                }
              }
              true
            } else {
              false
            }
          case _: DslSyntax[_]                            => true
          case DslAxiom(_)                                => true
          case DslBody(element)                           => continueVisit(element)
        }
      }

      override def endVisit(candidates: Seq[CompletionProposal]): Seq[CompletionProposal] = {
        var mustAddX = false
        val res      = candidates
          .filter(c => c.element.exists(_.elementType.exists(t => isExpected(t.concept))))
          .filter(p =>
            p.element match {
              case Some(s @ DslSyntax(syntax)) =>
                if (syntax.userData.contains(MyDsl.variableId))
                  if (
                    s.elementType.exists(t => t.concept == MyDsl.number || t.concept == MyDsl.value)
                  ) {
                    mustAddX = true
                    false
                  } else
                    false
                else true
              case _                           =>
                true
            }
          )
        if (mustAddX) {
          res ++ Seq(CompletionProposal(None, "AVarRef(x)", None, None, None))
        } else
          res
      }
    }
  }

  test("assure simple number") {
    AstHelpers.selectAst(MyDsl)("12") { tree =>
      assertEquals(tree.markers.length, 0)
      assertEquals(tree.value, ANumberValue("12"))
    }
  }

  test("assure parse expression") {
    AstHelpers.selectAst(MyDsl)("\"foo\" is 0 + \"bar\"") { tree =>
      assertEquals(tree.markers.length, 0)
      assertEquals(
        tree.value,
        AIs(
          AStringValue("foo"),
          AConcat(ANumberValue("0"), AStringValue("bar"))
        )
      )
    }
  }

  test("assure parse expression 2") {
    AstHelpers.selectAst(MyDsl)("\"foo\" ?: 0 + \"bar\"") { tree =>
      assertEquals(tree.markers.length, 0)
      assertEquals(
        tree.value,
        AElvis(
          AStringValue("foo"),
          AConcat(ANumberValue("0"), AStringValue("bar"))
        )
      )
    }
  }

  test("assure parse variable") {
    AstHelpers.selectAst(MyDsl)("toto") { tree =>
      assertEquals(tree.markers.length, 0)
      assertEquals(tree.value, AVarRef("toto"))
    }
  }

  test("assure parse true") {
    AstHelpers.selectAst(MyDsl)("true") { tree =>
      assertEquals(tree.markers.length, 0)
      assertEquals(tree.value, ABooleanValue("true"))
    }
  }

  private def assertPredictions(
    expectedType: Concept[_],
    text: String,
    offset: Int,
    expected: Seq[Any]
  ): Unit = {
    val variables = Map("x" -> MyDsl.number)
    val config    = new CompletionConfiguration
    config.setComputeFilter(MyDsl.MyComputeFilter(expectedType, variables))
    val proposals = predict(MyDsl, text, offset, Some(config))
    assertEquals(proposals.map(_.text), expected)
  }

  //   1 "foo" is . <value>
  //   2 . <values>
  // * 3 . <number>
  //   4 . <exprs>
  //   5 . <number> + <string>

  // ( 1, 2, 3 )
  // ( 1, 4, 5, 3 )

  test("predict after 'is' with string") {
    val text = "\"foo\" is "
    assertPredictions(
      MyDsl.boolean,
      text,
      text.length,
      Seq(
        "ANumberValue(0)",
        "AStringValue()",
        "AVarRef(x)",
        "one of"
      )
    )
  }

  test("predict after is with keyword (not variable)") {
    val text = "true is "
    assertPredictions(
      MyDsl.boolean,
      text,
      text.length,
      Seq(
        "true",
        "false",
        "AVarRef(x)",
        "one of"
      )
    )
  }

  test("predict on empty, expecting string") {
    val text = ""
    assertPredictions(
      MyDsl.string,
      text,
      text.length,
      Seq(
        "ANumberValue(0)",
        "AStringValue()",
        "AVarRef(x)"
      )
    )
  }

  test("predict on empty, expecting boolean") {
    val text = ""
    assertPredictions(
      MyDsl.boolean,
      text,
      text.length,
      Seq(
        "ANumberValue(0)",
        "AStringValue()",
        "true",
        "false",
        "AVarRef(x)"
      )
    )
  }

  test("predict after elvis with string") {
    val text = "\"foo\" ?: "
    assertPredictions(
      MyDsl.string,
      text,
      text.length,
      Seq(
        "ANumberValue(0)",
        "AStringValue()",
        "AVarRef(x)"
      )
    )
  }

  test("predict after elvis with number") {
    val text = "1 ?: "
    assertPredictions(
      MyDsl.number,
      text,
      text.length,
      Seq(
        "ANumberValue(0)",
        "AVarRef(x)"
      )
    )
  }

  test("predict after keyword (not variable)") {
    val text = "true "
    assertPredictions(
      MyDsl.value,
      text,
      text.length,
      Seq(
        "&&",
        "?:",
        "is",
        "is one of",
        "?:"
      )
    )
  }

  test("predict after variable") {
    val text = "x "
    assertPredictions(
      MyDsl.value,
      text,
      text.length,
      Seq(
        "is",
        "is one of",
        "?:",
        "+",
        "?:"
      )
    )
  }

  test("predict open brace after is_one_of") {
    val text = "\"foo\" is one of "
    assertPredictions(
      MyDsl.value,
      text,
      text.length,
      Seq(
        "{"
      )
    )
  }

  test("predict after open brace in literal list") {
    val text = "\"foo\" is one of { "
    assertPredictions(
      MyDsl.value,
      text,
      text.length,
      Seq(
        "ANumberValue(0)",
        "AStringValue()",
        "AVarRef(x)"
      )
    )
  }

  test("predict after comma in literal list") {
    val text = "\"foo\" is one of { \"bar\",  "
    assertPredictions(
      MyDsl.value,
      text,
      text.length,
      Seq(
        "ANumberValue(0)",
        "AStringValue()",
        "AVarRef(x)"
      )
    )
  }

}
