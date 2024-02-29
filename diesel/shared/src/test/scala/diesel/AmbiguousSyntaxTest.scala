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

import diesel.Errors.Ambiguous
import diesel.samples.AmbiguousSyntax
import diesel.samples.AmbiguousSyntax.{
  AgeOf,
  Brother,
  Constant,
  Expr,
  House,
  NamedPerson,
  Owner,
  Pair
}

class AmbiguousSyntaxTest extends DslTestFunSuite {

  type Ast = Expr

  override def dsl = AmbiguousSyntax.MyDsl

  test("constant") {
    assertAst("12") {
      Constant(12)
    }
  }

  test("age of") {
    assertAst("the age of john") {
      AgeOf(NamedPerson("john"))
    }
  }

  test("brother of") {
    assertAst("john 's brother") {
      Brother(NamedPerson("john"))
    }
  }

  test("age of brother") {
    assertAst("the age of john 's brother") {
      AgeOf(Brother(NamedPerson("john")))
    }
  }

  test("age of brother of brother") {
    assertAst("the age of john 's brother 's brother") {
      AgeOf(Brother(Brother(NamedPerson("john"))))
    }
  }

  test("owner of house of john") {
    assertAst("the owner of the house of john") {
      Owner(House(NamedPerson("john")))
    }
  }

  test("pair") {
    assertAst("cons(the age of john, 12)") {
      Pair(AgeOf(NamedPerson("john")), Constant(12))
    }
  }

  test("owner of house of brother of john") {
    withTree("the owner of the house of john 's brother") { tree =>
      assert(tree.markers.length == 1)
      assert(
        tree.markers.head == Ambiguous.apply(0, 41)
      )
      assertEquals(tree.value, Brother(Owner(House(NamedPerson("john")))))
    }
  }

  test("age of owner of house of brother of john") {
    withTree("the age of the owner of the house of john 's brother") { tree =>
      assert(tree.markers.length == 1)
      assert(
        tree.markers.head == Ambiguous.apply(0, 52)
      )
      assertEquals(tree.value, AgeOf(Brother(Owner(House(NamedPerson("john"))))))
    }
  }

  test("cons(owner of house of brother of john, 12)") {
    withTree("cons( the age of the owner of the house of john 's brother, 12)") { tree =>
      assert(tree.markers.length == 1)
      assert(
        tree.markers.head == Ambiguous.apply(6, 52)
      )
      assertEquals(
        tree.value,
        Pair(AgeOf(Brother(Owner(House(NamedPerson("john"))))), Constant(12))
      )
    }
  }

  test(
    "cons(age of owner of house of brother of john, age of owner of house of brother of john)"
  ) {
    withTree(
      "cons(the age of the owner of the house of john 's brother, the age of the owner of the house of john 's brother)"
    ) { tree =>
      assert(tree.markers.length == 2)
      assert(tree.markers.head == Ambiguous.apply(5, 52))
      assert(tree.markers.tail.head == Ambiguous.apply(59, 52))
      assertEquals(
        tree.value,
        Pair(
          AgeOf(Brother(Owner(House(NamedPerson("john"))))),
          AgeOf(Brother(Owner(House(NamedPerson("john")))))
        )
      )
    }
  }
}
