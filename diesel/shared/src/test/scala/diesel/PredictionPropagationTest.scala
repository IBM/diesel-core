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
import diesel.Dsl.{Axiom, Concept, Instance, Syntax, SyntaxGeneric}
import munit.FunSuite
import diesel.Bnf.{DslAxiom, DslElement}
import diesel.Bnf.DslInstance
import diesel.Bnf.DslTarget
import diesel.Bnf.DslValue
import diesel.Bnf.DslSyntax
import diesel.Bnf.DslBody
import diesel.Dsl.SyntaxTyped
import diesel.Dsl.Identifiers

class PredictionPropagationTest extends FunSuite {

  trait AValue
  trait ANumber                           extends AValue
  trait ABoolean                          extends AValue
  trait AString                           extends AValue
  case class ANumberValue(value: String)  extends ANumber
  case class ABooleanValue(value: String) extends ABoolean
  case class AStringValue(value: String)  extends AString

  case class AIs(left: AValue, right: AValue)     extends ABoolean
  case class AConcat(left: AValue, right: AValue) extends AString
  case class AElvis(left: AValue, right: AValue)  extends AValue
  case class AVarRef(name: String)                extends AValue
  case class AAnd(left: AValue, right: AValue)    extends ABoolean

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

    def identRegex                           = "[a-zA-Z_\\\\$][a-zA-Z0-9_\\\\$]*".r
    override def identScanner: Lexer.Scanner = identRegex

    val variableRef: Syntax[AValue] = syntax(value)(
      idOrKeyword map {
        case (ctx, name) =>
          if (Set("true", "false").contains(name.text)) {
            // ctx.abort()
            ctx.addMarkers(createMarker(NotAVariable, ctx.offset, ctx.length))
          }
          AVarRef(name.text)
      }
    )

    object NotAVariable extends MarkerMessage {
      def format(locale: String): String = ???
    }

    private def createMarker(m: MarkerMessage, o: Int, l: Int): Marker =
      Marker(Errors.SemanticError, o, l, m)
  }

  object MyDsl extends BootVoc {

    val expr: Axiom[AValue] = axiom(value)
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
      // assertEquals(tree.markers.map(_.message), Seq(AmbiguousMsg))
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
    config.setComputeFilter(MyComputeFilter(expectedType, variables))
    val proposals = predict(MyDsl, text, offset, Some(config))
    assertEquals(proposals.map(_.text), expected)
  }

  case class MyComputeFilter(expectedType: Concept[_], variables: Map[String, Concept[_]])
      extends CompletionComputeFilter {

    var visitedTypes = Set.empty[Concept[_]]

    def beginVisit(predictionState: PredictionState): Boolean = {
      visitedTypes = Set.empty[Concept[_]]
      if (predictionState.isAxiom) {
        visitedTypes = Set(expectedType)
        true
      } else {
        println("FW begin state", predictionState)

        val accept =
          predictionState.leftSubIndex.map { i =>
            val elements        = predictionState.elementsAt(i)
            val allVariableRefs = elements.forall {
              case DslSyntax(syntax) => syntax == MyDsl.variableRef
              case _                 => false
            }
            if (allVariableRefs) {
              val text         = predictionState.textsAt(i).mkString("")
              val variableType = variables.get(text)
              // val valid        = for {
              //   actual   <- variableType
              //   element  <- predictionState.element
              //   expected <- element.elementType
              //   if MyDsl.isSubtypeOf(actual, expected.concept)
              // } yield actual
              // valid.map { variableType =>
              //   // visitedTypes = visitedTypes + variableType
              //   true
              // }.getOrElse(false)
              variableType.isDefined
            } else true
          }.getOrElse(true)
        if (accept) {
          val propagate = predictionState.element match {
            case Some(DslSyntax(syntax)) if syntax == MyDsl.is => true
            case Some(DslSyntax(syntax: SyntaxTyped[_]))       => syntax.name == "elvis"
            case _                                             => false
          }
          if (propagate) {
            val propagates = predictionState.subIndex.filter(_ > 0)
              .map(_ =>
                predictionState.elementsAt(0)
                  .filter {
                    case DslSyntax(syntax) => syntax != MyDsl.variableRef
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
        MyDsl.isSubtypeOf(concept, visited)
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
        case DslValue(concept)                          =>
          if (isContinue(concept)) {
            this.visitedTypes = this.visitedTypes + concept
          }
          true
        case DslBody(DslSyntax(syntax: SyntaxTyped[_])) =>
          if (isContinue(syntax.concept)) {
            if (this.visitedTypes.isEmpty) {
              this.visitedTypes = this.visitedTypes + syntax.concept
            }
            if (syntax == MyDsl.concat) {
              this.visitedTypes = this.visitedTypes + MyDsl.number
            } else if (syntax == MyDsl.variableRef) {
              // TODO infer variable type?
            }
            true
          } else {
            false
          }
//           if (isContinue(syntax.concept)) {
//             // TODO context is first 'hole'
//             if (syntax == MyDsl.concat) {
//               this.visitedTypes = this.visitedTypes + MyDsl.number
//               true
//             } else if (syntax == MyDsl.is) {
//               this.visitedTypes = this.visitedTypes + MyDsl.value
//               true
// // } else if (syntax == MyDsl.elvis) {
//               //   this.visitedTypes = this.visitedTypes + MyDsl.value
//               //   true
//             } else {
//               false
//             }
//           } else {
//             false
//           }
        case _: DslSyntax[_]                            => true
        case DslAxiom(_)                                => true
        case DslBody(element)                           => continueVisit(element)
      }
    }

    override def endVisit(candidates: Seq[CompletionProposal]): Seq[CompletionProposal] = {
//      val fw1 = candidates.map(_.element)
//      val fw2 = fw1.flatten.map(_.elementType)
//      val fw3 = fw2.flatten.map(_.concept.name)
//      println("FW3", fw3, this.visitedTypes.map(_.name))

      candidates
        .filter(c => c.element.exists(_.elementType.exists(t => isExpected(t.concept))))
    }
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
        "AStringValue()"
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
        "false"
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
        "AStringValue()"
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
        // TODO
        // "ANumberValue(0)",
        // "AStringValue()",
        "true",
        "false"
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
        "AStringValue()"
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
        "ANumberValue(0)"
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
        // "&&",
        // "?:",
        "is",
        "?:"
        // TODO
        // "+"
      )
    )
  }

}
