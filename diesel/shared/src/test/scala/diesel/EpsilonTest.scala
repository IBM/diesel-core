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

import diesel.Dsl.{Axiom, Syntax}
import diesel.Errors.Ambiguous
import munit.FunSuite

class EpsilonTest extends FunSuite {

  object MyDsl extends Dsl {

    val b: Syntax[String] = syntax("b".? map {
      case (_, b) => b match {
          case Some(value) => value.text
          case None        => ""
        }
    })

    val s: Syntax[String] = syntax(b ~ b ~ b ~ b ~ b map {
      case (_, (b1, b2, b3, b4, b5)) =>
        b1 + b2 + b3 + b4 + b5
    })

    val a: Axiom[String] = axiom(s)
  }

  test("BNF") {
    def dsl = MyDsl
    def bnf = Bnf(dsl)
    assertEquals(
      bnf.emptyRules.map(r => r.name),
      Set(
        "a[_,_,_,_,_]",
        "syntax[_,_,_,_,_].b",
        "syntax[_,_,_,_,_].b.map.1",
        "syntax[_,_,_,_,_].s.map.0",
        "syntax[_,_,_,_,_].b.map.1.opt.2",
        "syntax[_,_,_,_,_].s"
      )
    )
    // "syntax[_,_,_,_,_].b.map.1.opt.2.item" -> "b"
  }

  test("none") {
    AstHelpers.selectAst(MyDsl)("") { tree =>
      AstHelpers.assertNoMarkers(tree)
      assertEquals(tree.value, "")
    }
  }

  test("one") {
    AstHelpers.selectAst(MyDsl)("b") { tree =>
      assertEquals(tree.markers.length, 1)
      assertEquals(tree.markers.head, Ambiguous.apply(0, 1))
      assertEquals(tree.value, "b")
    }
  }

  test("two") {
    AstHelpers.selectAst(MyDsl)("bb") { tree =>
      assertEquals(tree.markers.length, 1)
      assertEquals(tree.markers.head, Ambiguous.apply(0, 2))
      assertEquals(tree.value, "bb")
    }
  }

  test("three") {
    AstHelpers.selectAst(MyDsl)("bbb") { tree =>
      assertEquals(tree.markers.length, 1)
      assertEquals(tree.markers.head, Ambiguous.apply(0, 3))
      assertEquals(tree.value, "bbb")
    }
  }

  test("four") {
    AstHelpers.selectAst(MyDsl)("bbbb") { tree =>
      assertEquals(tree.value, "bbbb")
    }
  }

  test("five") {
    AstHelpers.selectAst(MyDsl)("bbbbb") { tree =>
      AstHelpers.assertNoMarkers(tree)
      assertEquals(tree.value, "bbbbb")
    }
  }
}
