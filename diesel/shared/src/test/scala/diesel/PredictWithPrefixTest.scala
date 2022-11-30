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
    assertEquals(proposals.map(_.text), expectedPredictions)
  }

  private val expectedAll = Seq("foo", "bar")

  test("empty") {
    assertPredictions(
      "",
      0,
      expectedAll
    )
  }

  test("f ba") {
    assertPredictions(
      "f ba",
      4,
      expectedAll
    )
  }

  test("f") {
    assertPredictions(
      "f",
      1,
      expectedAll
    )
  }

  test("fo") {
    assertPredictions(
      "fo",
      2,
      expectedAll
    )
  }

  test("b") {
    assertPredictions(
      "b",
      1,
      expectedAll
    )
  }

  test("x, offset 0") {
    assertPredictions(
      "x",
      0,
      expectedAll
    )
  }

  test("x, offset 1") {
    assertPredictions(
      "x",
      1,
      expectedAll
    )
  }

  test("replace 1") {
    val proposals = AstHelpers.predict(MyDsl, "fo", 2)
    assert(proposals.size == 2)
    val p0        = proposals.toArray.apply(0)
    assert(p0.text == "foo")
    assert(p0.userData.isEmpty)
    assert(p0.replace.isDefined)
    assert(p0.replace.get._1 == 0)
    assert(p0.replace.get._2 == 2)
    val p1        = proposals.toArray.apply(1)
    assert(p1.text == "bar")
    assert(p1.userData.isEmpty)
    assert(p1.replace.isDefined)
    assert(p1.replace.get._1 == 0)
    assert(p1.replace.get._2 == 2)
  }

  test("replace 2") {
    val proposals = AstHelpers.predict(MyDsl, "foo", 2)
    assert(proposals.size == 2)
    val p0        = proposals.toArray.apply(0)
    assert(p0.text == "foo")
    assert(p0.userData.isEmpty)
    assert(p0.replace.isDefined)
    assert(p0.replace.get._1 == 0)
    assert(p0.replace.get._2 == 3)
    val p1        = proposals.toArray.apply(1)
    assert(p1.text == "bar")
    assert(p1.userData.isEmpty)
    assert(p1.replace.isDefined)
    assert(p1.replace.get._1 == 0)
    assert(p1.replace.get._2 == 3)
  }

  test("replace 3") {
    val proposals = AstHelpers.predict(MyDsl, "foo", 0)
    assert(proposals.size == 2)
    val p0        = proposals.toArray.apply(0)
    assert(p0.text == "foo")
    assert(p0.userData.isEmpty)
    assert(p0.replace.isDefined)
    assert(p0.replace.get._1 == 0)
    assert(p0.replace.get._2 == 3)
    val p1        = proposals.toArray.apply(1)
    assert(p1.text == "bar")
    assert(p1.userData.isEmpty)
    assert(p1.replace.isDefined)
    assert(p1.replace.get._1 == 0)
    assert(p1.replace.get._2 == 3)
  }

  test("replace 4") {
    val proposals = AstHelpers.predict(MyDsl, "foo", 3)
    assert(proposals.size == 2)
    val p0        = proposals.toArray.apply(0)
    assert(p0.text == "foo")
    assert(p0.userData.isEmpty)
    assert(p0.replace.isDefined)
    assert(p0.replace.get._1 == 0)
    assert(p0.replace.get._2 == 3)
    val p1        = proposals.toArray.apply(1)
    assert(p1.text == "bar")
    assert(p1.userData.isEmpty)
    assert(p1.replace.isDefined)
    assert(p1.replace.get._1 == 0)
    assert(p1.replace.get._2 == 3)
  }

//  test("tmp") {
//    val proposals = AstHelpers.predict(MyDsl, "fo", 2)
//    assert(proposals.size == 2)
//  }

}
