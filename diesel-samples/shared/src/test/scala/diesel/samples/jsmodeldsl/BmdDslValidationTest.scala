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
import diesel.Dsl

class BmdDslValidationTest extends DslTestFunSuite[BmdDsl.type] {

  override def dsl                                 = BmdDsl
  override def axiom: Some[Dsl.Axiom[JsModelDecl]] = Some(BmdDsl.aCompileUnit)

  private val semanticError = Descriptor(Kind.Semantic, Severity.Error)

  import diesel.MarkerMessage.Implicits.strToMsg

  test("duplicate attribute") {
    assertMarkers(
      """start with a MyClass.
        |a MyClass is a concept.
        |a MyClass has a foo (numeric).
        |a MyClass has a foo (numeric).
        |a MyClass has a bar (text).
        |""".stripMargin
    ) {
      Seq(
        Marker(semanticError, 62, 3, "Attribute foo declared more than once"),
        Marker(semanticError, 93, 3, "Attribute foo declared more than once")
      )
    }
  }

  test("duplicate class") {
    assertMarkers(
      """start with a my class.
        |a my class is a concept.
        |a my class is a concept.
        |""".stripMargin
    ) {
      Seq(
        Marker(semanticError, 25, 8, "Class my class declared more than once"),
        Marker(semanticError, 50, 8, "Class my class declared more than once")
      )
    }
  }

  test("missing class") {
    assertMarkers("start with a MyClass.") {
      Seq(
        Marker(semanticError, 13, 7, "Undeclared type MyClass")
      )
    }
  }

}
