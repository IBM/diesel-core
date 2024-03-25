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
import diesel.Dsl.{Axiom, Concept, Instance, Syntax}
import munit.FunSuite
import diesel.Bnf.{DslAxiom, DslElement}

import scala.annotation.tailrec

class PredictionPropagationTest extends FunSuite {

  trait AValue
  trait ANumber                            extends AValue
  trait ABoolean                           extends AValue
  trait AString                            extends AValue
  case class ANumberValue(value: Double)   extends ANumber
  case class ABooleanValue(value: Boolean) extends ABoolean
  case class AStringValue(value: String)   extends AString

  case class AIs(left: AValue, right: AValue)     extends ABoolean
  case class AConcat(left: AValue, right: AValue) extends AString

  trait BootVoc extends Dsl {

    val value: Concept[AValue] = concept

    val number: Concept[ANumber] =
      concept[ANumber, AValue]("\\d+(\\.\\d+)?".r, ANumberValue(0.0), Some(value)) map {
        case (_, t) =>
          ANumberValue(t.text.toDoubleOption.getOrElse(0.0))
      }

    val boolean: Concept[ABoolean] = concept(value)

    val booleanTrue: Instance[ABoolean] = instance(boolean)("true") map { _ =>
      ABooleanValue(true)
    }

    val booleanFalse: Instance[ABoolean] = instance(boolean)("false") map { _ =>
      ABooleanValue(false)
    }

    val string: Concept[AString] =
      concept[AString, AValue]("\"([^\"\\\\]|\\\\.)*\"".r, AStringValue(""), Some(value)) map {
        case (_, t) =>
          AStringValue(t.text.drop(1).dropRight(1))
      }

    val concat: Syntax[AString] = syntax[AString](string)(
      number ~ "+".leftAssoc(2) ~ string map {
        case (_, (lhs, _, rhs)) =>
          AConcat(lhs, rhs)
      }
    )

    val is: Syntax[ABoolean] = syntax[ABoolean](boolean)(
      value ~ "is".leftAssoc(1) ~ value map {
        case (_, (lhs, _, rhs)) =>
          AIs(lhs, rhs)
      }
    )
  }

  object MyDsl extends BootVoc {

    val expr: Axiom[AValue] = axiom(value)
  }

  test("simple number") {
    AstHelpers.selectAst(MyDsl)("12") { tree =>
      assertEquals(tree.markers.length, 0)
      assertEquals(tree.value, ANumberValue(12.0))
    }
  }

  test("ddd") {
    AstHelpers.selectAst(MyDsl)("\"foo\" is 0 + \"bar\"") { tree =>
      assertEquals(tree.markers.length, 0)
    }
  }

  private def assertPredictions(
    expectedType: Concept[_],
    text: String,
    offset: Int,
    expected: Seq[Any]
  ): Unit = {
    val config    = new CompletionConfiguration
    config.setFilter(MyCompletionFilter(expectedType))
    config.setIncludePaths(true)
    val proposals = predict(MyDsl, text, offset, Some(config))
    assertEquals(proposals.map(_.text), expected)
  }

  case class MyCompletionFilter(expectedType: Concept[_]) extends CompletionFilter {
    def filterProposals(
      tree: GenericTree,
      offset: Int,
      node: Option[GenericNode],
      proposals: Seq[CompletionProposal]
    ): Seq[CompletionProposal] = proposals.filter { _.predictorPaths.exists(isInteresting) }

    def isInteresting(pathToPropsal: Seq[DslElement]): Boolean = {
      val r = pathToPropsal.filter(e => !e.isInstanceOf[DslAxiom[_]]).lastOption
        .flatMap(elementType)
        .exists(_.concept == expectedType)
      r
    }

    case class ElementType(concept: Concept[_], multiple: Boolean = false)

    @tailrec
    private def elementType(p: DslElement): Option[ElementType] = p match {
      case Bnf.DslAxiom(_)           =>
        None
      case Bnf.DslValue(concept)     =>
        Some(ElementType(concept))
      case Bnf.DslTarget(concept)    =>
        Some(ElementType(concept))
      case Bnf.DslInstance(instance) =>
        Some(ElementType(instance.concept))
      case Bnf.DslSyntax(syntax)     =>
        syntax match {
          case Dsl.SyntaxUntyped(_, _, _, _)              =>
            None
          case Dsl.SyntaxTyped(_, concept, _, _, _, _, _) =>
            Some(ElementType(concept))
          case Dsl.SyntaxMulti(_, concept, _, _, _, _)    =>
            Some(ElementType(concept, multiple = true))
        }
      case Bnf.DslBody(element)      =>
        elementType(element)
    }
  }

  //   1 "foo" is . <value>
  //   2 . <values>
  // * 3 . <number>
  //   4 . <exprs>
  //   5 . <number> + <string>

  // ( 1, 2, 3 )
  // ( 1, 4, 5, 3 )

  test("predict 1") {
    val text = "\"foo\" is "
    assertPredictions(
      MyDsl.string,
      text,
      text.length,
      Seq(
        "ANumberValue(0.0)",
        "AStringValue()"
      )
    )
  }

  test("predict 2") {
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

  test("predict 3") {
    val text = ""
    assertPredictions(
      MyDsl.string,
      text,
      text.length,
      Seq(
        "ANumberValue(0.0)",
        "AStringValue()"
      )
    )
  }

  test("predict 4") {
    val text = ""
    assertPredictions(
      MyDsl.boolean,
      text,
      text.length,
      Seq(
        "ANumberValue(0.0)",
        "AStringValue()",
        "true",
        "false"
      )
    )
  }
}
