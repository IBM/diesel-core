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

import diesel.Marker.{Descriptor, Kind, Severity}
import diesel.{DslTestFunSuite, Marker}

class JsModelDslValidationTest extends DslTestFunSuite[JsModelDsl.type] {

  override def dsl = JsModelDsl

  private val semanticError = Descriptor(Kind.Semantic, Severity.Error)

  import diesel.MarkerMessage.Implicits.strToMsg

  test("duplicate attribute") {
    assertMarkers(
      """root: MyClass
        |class MyClass {
        |  foo: string
        |  foo: number
        |  bar: boolean
        |}
        |""".stripMargin
    ) {
      Seq(
        Marker(semanticError, 32, 3, "Attribute foo declared more than once"),
        Marker(semanticError, 46, 3, "Attribute foo declared more than once")
      )
    }
  }

  test("duplicate class") {
    assertMarkers(
      """root: MyClass
        |class MyClass { }
        |class MyClass { }
        |""".stripMargin
    ) {
      Seq(
        Marker(semanticError, 20, 7, "Class MyClass declared more than once"),
        Marker(semanticError, 38, 7, "Class MyClass declared more than once")
      )
    }
  }

  test("missing class") {
    assertMarkers("root: MyClass") {
      Seq(
        Marker(semanticError, 6, 7, "Undeclared type MyClass")
      )
    }
  }

  test("extend self") {
    assertMarkers(
      """root: Super
        |class Super extends Super {}""".stripMargin
    ) {
      Seq(
        Marker(semanticError, 32, 5, "A class cannot extend itself")
      )
    }
  }

  test("cyclic extend") {
    assertMarkers(
      """root: A
        |class A extends B {}
        |class B extends A {}""".stripMargin
    ) {
      Seq(
        Marker(semanticError, 45, 1, "Cyclic inheritance"),
        Marker(semanticError, 24, 1, "Cyclic inheritance")
      )
    }
  }

  test("missing type") {
    assertMarkers(
      """root: A
        |class A extends B {}""".stripMargin
    ) {
      Seq(
        Marker(semanticError, 24, 1, "Undeclared type B")
      )
    }
  }

  test("discriminator misuse") {
    assertMarkers(
      """root: A
        |class A discriminator field foo {
        |  foo: string
        |  bar: number
        |}""".stripMargin
    ) {
      Seq(
        Marker(semanticError, 44, 3, "Attribute is used as discriminator"),
        Marker(semanticError, 36, 3, "Discriminator cannot be used in attributes")
      )
    }
  }

  test("discriminator incomplete") {
    assertMarkers(
      """root: A
        |class A discriminator value "a" {
        |  bar: number
        |}""".stripMargin
    ) {
      Seq(
        Marker(
          Descriptor(Kind.Semantic, Severity.Warning),
          36,
          3,
          "Discriminator value used without 'extends'. Please specify the super class."
        )
      )
    }
  }

}
