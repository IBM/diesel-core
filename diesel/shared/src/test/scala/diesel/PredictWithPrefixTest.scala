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

import diesel.Dsl.{Axiom, Concept, Instance}
import munit.FunSuite

class PredictWithPrefixTest extends FunSuite {

  object MyDsl extends Dsl {

    val x: Concept[String] = concept

    val foo: Instance[String] = instance(x)("foo") map { _ => "foo" }

    val bar: Instance[String] = instance(x)("bar") map { _ => "bar" }

    val a: Axiom[String] = axiom(x)

  }

  private def assertPredictions(
    text: String,
    offset: Int,
    expectedPredictions: Seq[String]
  ): Unit = {
    val proposals = AstHelpers.predict(MyDsl, text, offset)
    assert(
      proposals.map(_.text) == expectedPredictions
    )
  }

  test("empty") {
    assertPredictions(
      "",
      0,
      Seq("foo", "bar")
    )
  }

  test("f ba") {
    assertPredictions(
      "f ba",
      4,
      Seq("bar")
    )
  }

  test("f") {
    assertPredictions(
      "f",
      1,
      Seq("foo")
    )
  }

  test("fo") {
    assertPredictions(
      "fo",
      2,
      Seq("foo")
    )
  }

  test("b") {
    assertPredictions(
      "b",
      1,
      Seq("bar")
    )
  }

  test("x, offset 0") {
    assertPredictions(
      "x",
      0,
      Seq("foo", "bar")
    )
  }

  test("x, offset 1") {
    assertPredictions(
      "x",
      1,
      Seq.empty
    )
  }

  test("replace") {
    val proposals = AstHelpers.predict(MyDsl, "fo", 2)
    assert(proposals.size == 1)
    val p         = proposals.head
    assert(p.text == "foo")
    assert(p.userData.isEmpty)
    assert(p.replace.isDefined)
    assert(p.replace.get._1 == 0)
    assert(p.replace.get._2 == 2)
  }

}
