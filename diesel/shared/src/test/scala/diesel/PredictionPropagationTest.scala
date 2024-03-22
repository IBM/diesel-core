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

  class BootVoc extends Dsl {

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

  private def assertPredictions(text: String, offset: Int, expected: Seq[Any]): Unit = {
    val proposals = predict(MyDsl, text, offset, None)
    assertEquals(proposals.map(_.text), expected)
  }

  //   1 "foo" is . <value>
  //   2 . <values>
  // * 3 . <number>
  //   4 . <exprs>
  //   5 . <number> + <string>

  // ( 1, 2, 3 )
  // ( 1, 4, 5, 3 )

  test("predict") {
    assertPredictions(
      "\"foo\" is ",
      10,
      Seq(
        ANumberValue(0.0),
        AStringValue("")
      )
    )
  }
}
