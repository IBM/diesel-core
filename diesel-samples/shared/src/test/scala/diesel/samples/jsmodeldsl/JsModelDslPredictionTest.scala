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

  test("root with prefix 2") {
    assertPredictions(
      "root : number",
      2,
      Seq("root :")
    )
  }

  test("root with prefix 3") {
    assertPredictions(
      "root : ",
      2,
      Seq("root :")
    )
  }

  test("root with prefix 4") {
    assertPredictions(
      "roo",
      2,
      Seq("root :")
    )
  }

  test("root with prefix 5") {
    assertPredictions(
      "roo",
      3,
      Seq("root :")
    )
  }

  test("root with prefix 6") {
    assertPredictions(
      "ro :",
      2,
      Seq("root :")
    )
  }

  test("root") {
    assertPredictions(
      "root",
      4,
      Seq("root :")
    )
  }

  test("root: 1") {
    assertPredictions(
      "root:",
      4,
      Seq("root :")
    )
  }

  test("root: 2") {
    assertPredictions(
      "root:",
      5,
      Seq("string", "boolean", "number")
    )
  }

  test("root non existing class") {
    assertPredictions(
      "root: MyClass",
      6,
      Seq("string", "boolean", "number")
    )
  }

  test("root non existing class") {
    assertPredictions(
      "root: MyClass",
      13,
      Seq("string", "boolean", "number")
    )
  }

  test("root non existing class") {
    assertPredictions(
      "root: MyClass",
      13,
      Seq("string", "boolean", "number")
    )
  }

  test("declared classes show up in predictions 1") {
    assertPredictions(
      """root:
        |class Foo { }
        |class Bar { }
        |""".stripMargin,
      4,
      Seq("root :")
    )
  }

  test("declared classes show up in predictions 2") {
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
      Seq("string", "boolean", "number")
    )
  }

  test("array or class or domain 2") {
    assertPredictions(
      "root: number ",
      13,
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

  test("closing brace after class declaration 1") {
    assertPredictions(
      """root: MyClass
        |class MyClass {""".stripMargin,
      29,
      Seq("}")
    )
  }

  test("closing brace after class declaration 2") {
    assertPredictions(
      """root: MyClass
        |class MyClass { }""".stripMargin,
      29,
      Seq("}")
    )
  }

  test("closing brace after class declaration 3") {
    assertPredictions(
      """root: MyClass
        |class MyClass { }""".stripMargin,
      29,
      Seq("}")
    )
  }

  test("optionals 1") {
    assertPredictions(
      """root: MyClass
        |class MyClass {
        |  foo
        |}""".stripMargin,
      35,
      Seq("}")
    )
  }

  test("optionals 2") {
    assertPredictions(
      """root: MyClass
        |class MyClass {
        |  foo
        |}""".stripMargin,
      36,
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
