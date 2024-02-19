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

import diesel.samples.InheritanceSample.Ast._
import diesel.samples.InheritanceSample._

class InheritanceTest extends DslTestFunSuite {

  type Ast = Ast.Value
  override def dsl = MyDsl

  test("concept inheritance") {
    assert(MyDsl.getParent(MyDsl.intValue).contains(MyDsl.hiddenValue))
    assert(MyDsl.getParent(MyDsl.hiddenValue).contains(MyDsl.value))
  }

  test("123") {
    assertAst("123") {
      IntValue(123)
    }
  }

  test("value type ref") {
    assertAst("value") {
      TypeRef("value")
    }
  }

  test("intValue type ref") {
    assertAst("intValue") {
      TypeRef("intValue")
    }
  }

  test("hiddenValue type ref") {
    withTree("hiddenValue") { tree =>
      assert(tree.markers.length == 2)
      assertEquals(
        tree.markers.head.message.format("en"),
        "The word 'IntValue(0)' is missing."
      )
      assertEquals(
        tree.markers.tail.head.message.format("en"),
        "The word 'hiddenValue' is unknown."
      )
      assert(tree.value == IntValue(0))
    }
  }
}
