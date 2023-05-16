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

package diesel.samples.jsmodeldsl

import diesel.AstHelpers._
import diesel.CompletionProposal
import munit.FunSuite

class BmdDslPredictionTest extends FunSuite {

  private def assertPredictions(
    text: String,
    offset: Int,
    expectedPredictions: Seq[String]
  ): Unit = {
    assertPredictionsWith(text, offset) { proposals =>
      assert(
        proposals.map(_.text) == expectedPredictions
      )
    }
  }

  private def assertPredictionsWith(
    text: String,
    offset: Int
  )(
    fun: Seq[CompletionProposal] => Unit
  ): Unit = {
    val proposals =
      predict(
        BmdDsl,
        text,
        offset,
        Some(BmdDsl.completionConfiguration),
        axiom = Some(BmdDsl.aCompileUnit)
      )
    fun(proposals)
  }

  test("empty") {
    assertPredictions(
      "",
      0,
      Seq("start with")
    )
  }

  test("start") {
    assertPredictions(
      "start",
      5,
      Seq("with")
    )
  }

  test("start with") {
    assertPredictions(
      "start with",
      10,
      Seq("text", "numeric", "a")
    )
  }

  test("declared classes show up in predictions on empty") {
    assertPredictions(
      """start with a 
        |a Foo is a concept.
        |a Bar is a concept.
        |""".stripMargin,
      12,
      // TODO should be?
//      Seq("Foo", "Bar")
      Seq("Bar")
    )
  }

  test("declared classes show up in predictions") {
    assertPredictions(
      """start with a x.
        |a Foo is a concept.
        |a Bar is a concept.
        |""".stripMargin,
      13,
      Seq("Foo", "Bar")
    )
  }

  test("is a concept") {
    assertPredictions(
      "a shopping cart",
      16,
      Seq("has", "can be", "is a", "can be one of", ".")
    )
  }

  test("optionals") {
    assertPredictions(
      """a Foo is a concept.
        |a Foo has a bar (text)
        |""".stripMargin,
      19 + 22 + 1,
      Seq("[ optional ]", ".")
    )
  }

  test("classes for fields") {
    assertPredictions(
      """a Foo is a concept.
        |a Foo has a bar (a  ).
        |a Bar is a concept.
        |a Gnu is a concept.
        |""".stripMargin,
      19 + 19 + 1,
      Seq("Bar", "Gnu") // TODO previous: Seq("Foo", "Bar", "Gnu")
    )
  }

  test("types for fields") {
    assertPredictions(
      """a Foo is a concept.
        |a Foo has a bar (  ).
        |a Bar is a concept.
        |a Gnu is a concept.
        |""".stripMargin,
      19 + 18 + 1,
      Seq("text", "numeric", "a")
    )
  }

  test("concepts for is a") {
    assertPredictions(
      """a Foo is a .
        |a Bar is a concept.
        |a Gnu is a concept.
        |""".stripMargin,
      11,
      Seq(
        "concept",
        "Bar",
        "Gnu",
        "has",
        "can be",
        "is a",
        "can be one of",
        "."
      )
    )
  }

  // TODO prediction on unclosed production
  test("concepts for unclosed is a".ignore) {
    assertPredictions(
      """a Foo is a
        |a Bar is a concept.
        |a Gnu is a concept.
        |""".stripMargin,
      11,
      Seq("concept", "Bar", "Gnu")
    )
  }

  test("exclude self from is a") {
    assertPredictions(
      """start with a Foo.
        |a Foo is a concept.
        |a Gnu is a xx.""".stripMargin,
      17 + 19 + 2 + 11,
      Seq(
        "concept",
        "Foo",
        "has",
        "can be",
        "is a",
        "can be one of"
      )
    )
  }

  test("predict with replacement") {
    assertPredictionsWith(
      """start with a Foo.
        |a Foo is a concept.
        |a Gnu is a xx.""".stripMargin,
      15
    ) { predictions =>
      assert(predictions.map(_.text) == Seq("Foo", "Gnu"))
      assert(predictions.flatMap(_.replace) == Seq((13, 3), (13, 3)))
    }
  }

  test("predict with documentation") {
    assertPredictionsWith(
      """start with a Foo.
        |a Foo is a concept.
        |a Gnu is a Foo.
        |a Gnu has a bar (text) [optional].
        |""".stripMargin,
      15
    ) { predictions =>
      assertEquals(
        predictions.flatMap(_.documentation),
        Seq(
          "<b>Foo</b>\n",
          """|<b>Gnu</b><br/>
             |extends Foo<br/>
             |bar?: string
             |""".stripMargin
        )
      )
    }
  }
}
