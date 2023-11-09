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

class PredictionQuotedVariablesTest extends FunSuite {

  object MyDsl extends Dsl {

    val cVar: Concept[String] = concept

    val sVar: Syntax[String] = syntax(cVar)(
      "'[a-z(]+'".r map {
        case (_, (name)) =>
          name.text
      }
    )

    val iVar: Instance[String] = instance(cVar)("foo") map (_ => "foo")

    val sPrint: Syntax[String] = syntax(
      "print" ~ "(" ~ cVar ~ ")" map {
        case (_, (_, _, name, _)) =>
          name.text
      }
    )

    val a: Axiom[String] = axiom(sPrint)

  }

  val predQuoted = "'[a-z(]+'"
  val predFoo    = "foo"
  val allPreds   = Seq(predFoo, predQuoted)

  private def assertPredictions(text: String, offset: Int, expected: Seq[String]) = {
    val proposals = predict(MyDsl, text, offset, None)
    assertEquals(proposals.map(_.text), expected)
  }

  test("empty text, no spaces, eol") {
    assertPredictions("", 0, Seq("print ("))
  }

  test("predict after print") {
    assertPredictions("print", 5, Seq("print ("))
  }

  test("predict after print with paren") {
    assertPredictions("print(", 6, allPreds)
  }

  test("predict inside variable") {
    assertPredictions("print('toto'", 9, allPreds)
  }

  test("predict inside variable with delimiter eos") {
    assertPredictions("print('toto')", 13, Nil)
  }

  test("predict inside variable with delimiter 1") {
    assertPredictions("print('to(to')", 10, allPreds)
  }

  test("predict inside variable with delimiter 2") {
    assertPredictions("print('to(to')", 11, allPreds)
  }

  test("predict inside variable with delimiter 3") {
    assertPredictions("print(foo)", 8, allPreds)
  }

  test("predict begin variable with ws 1") {
    assertPredictions("print( foo)", 7, allPreds)
  }

  test("predict begin variable with ws 2") {
    assertPredictions("print(foo)", 6, allPreds)
  }

  test("predict begin variable with ws 3") {
    assertPredictions("print(  foo)", 7, allPreds)
  }

  test("predict begin variable with ws 4") {
    assertPredictions("print( ", 7, allPreds)
  }

  test("with error") {
    assertPredictions("xxxx", 2, Seq("print ("))
  }

  test("with error 2") {
    assertPredictions("print(xxxx", 6, allPreds)
  }

  test("with error 3") {
    assertPredictions("print(xxxx", 10, allPreds)
  }

}
