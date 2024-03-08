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

import diesel.samples.TernaryOperatorSample
import diesel.samples.TernaryOperatorSample.Ast.{BoolValue, IntValue, Ternary}
import diesel.samples.TernaryOperatorSample.{Ast, MyDsl}

class TernaryOperatorTest extends DslTestFunSuite {

  type Ast = Ast.Value

  override def dsl: TernaryOperatorSample.MyDsl.type = MyDsl

  test("int value") {
    assertAst("12") {
      Ast.IntValue(12)
    }
  }

  test("bool value") {
    assertAst("true") {
      Ast.BoolValue(true)
    }
  }

  test("ternary") {
    assertAst("true ? 1 : 2") {
      Ast.Ternary(BoolValue(true), IntValue(1), IntValue(2))
    }
  }

  test("left nested ternary") {
    assertAst("true ? false : true ? 1 : 2") {
      Ternary(Ternary(BoolValue(true), BoolValue(false), BoolValue(true)), IntValue(1), IntValue(2))
    }
  }

  test("middle nested ternary".only) {
    assertAst("true ? false ? 1 : 2 : 3") {
      Ternary(Ternary(BoolValue(true), BoolValue(false), BoolValue(true)), IntValue(1), IntValue(2))
    }
  }
}
