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
import diesel.samples.Ambiguity
import diesel.samples.Ambiguity._

class AmbiguityTest extends DslTestFunSuite {

  type Ast = Expr
  override def dsl = Ambiguity.MyDsl

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

  test("left assoc") {
    withAsts("1 + 2 + 3") { nav: Navigator =>
      val first  = nav.next()
      assert(first.value == Add(Add(Constant(1), Constant(2)), Constant(3)))
      assert(first.toSeq.exists(n => n.hasAmbiguity))
      assert(first.markers.isEmpty)
      val second = nav.next()
      assert(second.value == Add(Constant(1), Add(Constant(2), Constant(3))))
      assert(second.toSeq.exists(n => n.hasAmbiguity))
      assert(second.markers.size == 1)
      assert(second.markers.head.message == SimpleMarkerMessage("Invalid precedence"))
      assert(!nav.hasNext)
    }
    withSelect("1 + 2 + 3") { tree =>
      assertNoMarkers(tree, assertNoAmbiguity = false)
      assert(tree.value == Add(Add(Constant(1), Constant(2)), Constant(3)))
    }
  }

  test("precedence") {
    withAsts("1 + 2 * 3 + 4") { nav: Navigator =>
      val first  = nav.next()
      assert(first.value == Add(
        Mul(Add(Constant(1.0), Constant(2.0)), Constant(3.0)),
        Constant(4.0)
      ))
      assert(first.markers.size == 1)
      assert(first.toSeq.exists(n => n.hasAmbiguity))
      val second = nav.next()
      assert(second.value == Add(
        Add(Constant(1.0), Mul(Constant(2.0), Constant(3.0))),
        Constant(4.0)
      ))
      assert(second.markers.isEmpty)
      assert(second.toSeq.exists(n => n.hasAmbiguity))
      val third  = nav.next()
      assert(third.value == Add(
        Constant(1.0),
        Add(Mul(Constant(2.0), Constant(3.0)), Constant(4.0))
      ))
      assert(third.markers.size == 1)
      assert(third.toSeq.exists(n => n.hasAmbiguity))
      val fourth = nav.next()
      assert(fourth.value == Add(
        Constant(1.0),
        Mul(Constant(2.0), Add(Constant(3.0), Constant(4.0)))
      ))
      assert(fourth.markers.size == 1)
      assert(fourth.toSeq.exists(n => n.hasAmbiguity))
      val fifth  = nav.next()
      assert(fifth.value == Mul(
        Add(Constant(1.0), Constant(2.0)),
        Add(Constant(3.0), Constant(4.0))
      ))
      assert(fifth.markers.size == 2)
      assert(fifth.toSeq.exists(n => n.hasAmbiguity))
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
