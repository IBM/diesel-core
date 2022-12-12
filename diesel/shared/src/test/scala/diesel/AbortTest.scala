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

import diesel.AstHelpers._
import diesel.samples.Abort
import diesel.samples.Abort.{Add, Constant, Mul}

class AbortTest extends DslTestFunSuite[Dsl] {

  type Ast = Abort.Expr
  override def dsl = Abort.MyDsl

  test("constant") {
    assertAst("12") {
      Constant(12)
    }
  }

  test("expression") {
    assertAst("1 + 2") {
      Add(Constant(1), Constant(2))
    }
  }

  test("left assoc expression") {
    withAsts("1 + 2 + 3") { (nav: Navigator) =>
      val first = nav.next()
      assert(first.value == Add(Add(Constant(1), Constant(2)), Constant(3)))
      assert(first.markers.isEmpty)
      assert(!nav.hasNext)
    }
    withSelect("1 + 2 + 3") { tree =>
      assertNoMarkers(tree, assertNoAmbiguity = false)
      assert(tree.value == Add(Add(Constant(1), Constant(2)), Constant(3)))
    }
  }

  test("precedence expression") {
    withAsts("1 + 2 * 3 + 4") { (nav: Navigator) =>
      val first = nav.next()
      assert(first.value == Add(
        Add(Constant(1.0), Mul(Constant(2.0), Constant(3.0))),
        Constant(4.0)
      ))
      assert(first.markers.isEmpty)
      assert(!nav.hasNext)
    }
    withSelect("1 + 2 * 3 + 4") { tree =>
      assertNoMarkers(tree, assertNoAmbiguity = false)
      assert(tree.value == Add(
        Add(Constant(1.0), Mul(Constant(2.0), Constant(3.0))),
        Constant(4.0)
      ))
    }
  }

}
