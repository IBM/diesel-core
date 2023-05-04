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
import munit.FunSuite

class JsModelDslPredictionTest extends FunSuite {

  private def assertPredictions(
    text: String,
    offset: Int,
    expectedPredictions: Seq[String],
    expectedReplace: Option[Seq[Option[(Int, Int)]]] = None
  ): Unit = {
    val proposals = predict(JsModelDsl, text, offset, Some(JsModelDsl.completionConfiguration))
    assertEquals(proposals.map(_.text), expectedPredictions)
    expectedReplace.foreach { expectedReplace =>
      assertEquals(proposals.map(_.replace), expectedReplace)
    }
  }

  private def assertPredictionsNoCompletionConfig(
    text: String,
    offset: Int,
    expectedPredictions: Seq[String]
  ): Unit = {
    val proposals = predict(JsModelDsl, text, offset, None)
    assertEquals(proposals.map(_.text), expectedPredictions)
  }

  test("empty") {
    assertPredictions(
      "",
      0,
      Seq("root :")
    )
  }

  test("root with prefix") {
    assertPredictions(
      "ro",
      2,
      Seq("root :")
    )
  }

  test("root with prefix no config") {
    assertPredictionsNoCompletionConfig(
      "ro",
      2,
      Seq("root :")
    )
  }

  test("root") {
    assertPredictions(
      "root",
      4,
      Seq(":")
    )
  }

  test("root:") {
    assertPredictions(
      "root:",
      5,
      Seq("string", "boolean", "number")
    )
  }

  test("declared classes show up in predictions") {
    assertPredictions(
      """root:
        |class Foo { }
        |class Bar { }
        |""".stripMargin,
      5,
      Seq("string", "boolean", "number", "Foo", "Bar")
    )
  }

  test("array or class or domain") {
    assertPredictions(
      "root: number",
      12,
      Seq("[]", "class", "domain")
    )
  }

  test("opening brace after class declaration") {
    assertPredictions(
      """root: MyClass
        |class MyClass """.stripMargin,
      28,
      Seq("extends", "{", "discriminator")
    )
  }

  test("closing brace after class declaration") {
    assertPredictions(
      """root: MyClass
        |class MyClass {""".stripMargin,
      29,
      Seq("}")
    )
  }

  test("optionals") {
    assertPredictions(
      """root: MyClass
        |class MyClass {
        |  foo
        |}""".stripMargin,
      35,
      Seq("?", ":")
    )
  }

  test("class name") {
    assertPredictions(
      """root: MyClass
        |class """.stripMargin,
      20,
      Seq()
    )
  }

  test("domain name") {
    assertPredictions(
      """root: MyDomain
        |domain """.stripMargin,
      22,
      Seq()
    )
  }

  test("super class") {
    assertPredictions(
      """root: A
        |class A {}
        |class B extends """.stripMargin,
      35,
      Seq("A")
    )
  }

  test("super class, with replace") {
    assertPredictions(
      """root: A
        |class A {}
        |class B extends xxx""".stripMargin,
      36,
      Seq("A"),
      Some(Seq(Some(35, 1)))
    )
  }

}
