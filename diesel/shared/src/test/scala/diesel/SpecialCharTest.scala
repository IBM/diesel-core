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

import diesel.Dsl.{Axiom, Concept, Identifiers, Syntax}
import diesel.Marker.{Descriptor, Severity}
import diesel.Marker.Kind.Lexical
import munit.FunSuite

class SpecialCharTest extends FunSuite {

  object MyDsl extends Dsl with Identifiers {

    override def identScanner: Lexer.Scanner = "[a-zA-Z_][a-zA-Z0-9_]*".r

    val c: Concept[String] = concept

    val s1: Syntax[String] = syntax(c)(id map { case (_, t) => t.text })

    val s2: Syntax[String] = syntax(c)(id map { case (_, t) => t.text })

    val t: Syntax[String] = syntax(c)(s1 ~ "+" ~ s2 map {
      case (_, (t1, _, t2)) =>
        t1.text.concat(t2.text)
    })

    val a: Axiom[String] = axiom(c)
  }

  test("concat".only) {
    AstHelpers.selectAst(MyDsl)("yal + la") { tree =>
      assertEquals(tree.value, "yalla")
    }
  }

  test("no special char") {
    AstHelpers.selectAst(MyDsl)("yalla") { tree =>
      assertEquals(tree.value, "yalla")
    }
  }

  test("special char") {
    AstHelpers.selectAst(MyDsl)("yalla€") { tree =>
      assertEquals(
        tree.markers,
        List(
          Marker(
            descriptor = Descriptor(
              kind = Lexical,
              severity = Severity.Error
            ),
            offset = 5,
            length = 1,
            message = UnknownTokenMsg(token =
              "\u20ac"
            )
          )
        )
      )
    }
  }

}
