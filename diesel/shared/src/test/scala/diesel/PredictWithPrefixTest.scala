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

import diesel.Dsl.{Axiom, Concept, Instance, Syntax}
import munit.FunSuite

class PredictWithPrefixTest extends FunSuite {

  object MyDsl extends Dsl {

    val x: Concept[String] = concept

    val foo: Instance[String] = instance(x)("foo") map { _ => "foo" }

    val bar: Instance[String] = instance(x)("bar") map { _ => "bar" }

    val sHelloWorld: Syntax[String] = syntax(x)(
      "hello" ~ "world" map {
        case _ =>
          "helloworld"
      }
    )

    val sHelloFriend: Syntax[String] = syntax(x)(
      "hello" ~ "friend" map {
        case _ =>
          "hellofriend"
      }
    )

    val y: Concept[String] = concept

    val iJob: Instance[String] = instance(y)("job") map { _ => "job" }

    val iDay: Instance[String] = instance(y)("day") map { _ => "day" }

    val sGoodJobOrDay: Syntax[String] = syntax(x)(
      "good" ~ y map {
        case (_, (_, s)) =>
          "good" + s
      }
    )

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

  private val expectedAll = Seq("foo", "bar", "hello world", "hello friend", "good")

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

  test("go") {
    assertPredictions(
      "go",
      2,
      expectedAll
    )
  }

  test("good") {
    assertPredictions(
      "good ",
      5,
      Seq("job", "day")
    )
  }

  test("good 2") {
    assertPredictions(
      "good",
      2,
      expectedAll
    )
  }

  test("good 3") {
    assertPredictions(
      "good xxx",
      7,
      Seq("job", "day")
    )
  }

  test("good 4") {
    assertPredictions(
      "good jo",
      7,
      Seq("job", "day")
    )
  }

  test("good 5") {
    assertPredictions(
      "good job",
      4,
      expectedAll
    )
  }

  def assertProposal(p: CompletionProposal, text: String, replace: (Int, Int)): Unit = {
    assertEquals(p.text, text)
    assertEquals(p.replace.get._1, replace._1)
    assertEquals(p.replace.get._2, replace._2)
  }

  test("replace 1") {
    val proposals = AstHelpers.predict(MyDsl, "fo", 2)
    assertEquals(proposals.size, 5)
    proposals.foreach(p => {
      assertEquals(p.replace.get, (0, 2))
    })
  }

  test("replace 2") {
    val proposals = AstHelpers.predict(MyDsl, "foo", 2)
    assertEquals(proposals.size, 5)
    proposals.foreach(p => {
      assertEquals(p.replace.get, (0, 2))
    })
  }

  test("replace 3") {
    val proposals = AstHelpers.predict(MyDsl, "foo", 0)
    assertEquals(proposals.size, 5)
    proposals.foreach(p => {
      assert(p.replace.isEmpty)
    })
  }

  test("replace 4") {
    assertPredictions("foo", 3, expectedAll)
//    val proposals = AstHelpers.predict(MyDsl, "foo", 3)
//    assertEquals(proposals.size, 0)
  }

  test("replace 5") {
    val proposals = AstHelpers.predict(MyDsl, "good ", 5)
    assertEquals(proposals.size, 2)
    proposals.foreach(p => {
      assert(p.replace.isEmpty)
    })
  }

  test("replace 6") {
    val proposals = AstHelpers.predict(MyDsl, "good xx", 7)
    assertEquals(proposals.size, 2)
    proposals.foreach(p => {
      assertEquals(p.replace.get, (5, 2))
    })
  }

  test("replace 7") {
    val proposals = AstHelpers.predict(MyDsl, "good job", 8)
    assertEquals(proposals.map(_.text), Seq("job", "day"))
  }
}
