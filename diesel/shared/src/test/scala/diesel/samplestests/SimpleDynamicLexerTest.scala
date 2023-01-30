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

package diesel.samplestests

import diesel.Dsl.{Axiom, Concept, DynamicLexer, Syntax}
import diesel.{Dsl, DslTestFunSuite}

object MyDsl extends Dsl with DynamicLexer {

  val id: Concept[String] = concept("[a-z]+".r, "foo") map {
    case (_, t) =>
      t.text
  }

  val classDecl: Syntax[String] = syntax(
    "class" ~ id ~ "{" ~ "}" map {
      case (_, (_, i, _, _)) =>
        i
    }
  )

  val a: Axiom[String] = axiom(classDecl)

}

class MyTest extends DslTestFunSuite {

  type Ast = String
  override def dsl = MyDsl

  test("keyword as identifier") {
    assertAst("class class { }") {
      "class"
    }
  }

  test("infinite loop unrecognized char") {
    assertMarkers("class class€")(Seq.empty)
  }

}
