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

class PredictionAndSpacesTest extends FunSuite {

  object MyDsl extends Dsl {

    val attr: Concept[String] = concept

    val foo: Instance[String] = instance(attr)("foo") map (_ => "foo")

    val bar: Instance[String] = instance(attr)("bar") map (_ => "bar")

    val obj: Syntax[String] = syntax(
      "{" ~ attr ~ "}" map {
        case (_, (_, s, _)) =>
          s
      }
    )

    val a: Axiom[String] = axiom(obj)

  }

  private val expectedPredictions = Seq("foo", "bar")

  private def assertPredictions(text: String, offset: Int, expected: Seq[String]) = {
    val proposals = predict(MyDsl, text, offset, None)
    assertEquals(proposals.map(_.text), expected)
  }

  test("empty text, no spaces, eol") {
    assertPredictions("", 0, Seq("{"))
  }

  test("empty text, one space") {
    assertPredictions(" ", 0, Seq("{"))
  }

  test("empty text, one space, eol") {
    assertPredictions(" ", 1, Seq("{"))
  }

  test("empty text, spaces, 0") {
    assertPredictions("  ", 0, Seq("{"))
  }

  test("empty text, spaces, 1") {
    assertPredictions("  ", 1, Seq("{"))
  }

  test("empty text, spaces, eol") {
    assertPredictions("  ", 2, Seq("{"))
  }

  test("no space, closed object") {
    assertPredictions("{}", 1, expectedPredictions)
  }

  test("one space, closed object") {
    assertPredictions("{ }", 1, expectedPredictions)
  }

  test("2 spaces, closed object 1") {
    assertPredictions("{  }", 1, expectedPredictions)
  }

  test("2 spaces, closed object 2") {
    assertPredictions("{  }", 2, expectedPredictions)
  }

  test("2 spaces, closed object 3") {
    assertPredictions("{  }", 3, expectedPredictions)
  }

  test("with space") {
    assertPredictions("{  ", 2, expectedPredictions)
  }

  test("with space, eol") {
    assertPredictions("{ ", 2, expectedPredictions)
  }

  test("without space") {
    assertPredictions("{ ", 1, expectedPredictions)
  }

  test("without space, eol") {
    assertPredictions("{", 1, expectedPredictions)
  }

}
