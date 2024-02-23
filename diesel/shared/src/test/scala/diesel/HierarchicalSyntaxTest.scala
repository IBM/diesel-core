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

import diesel.samples.HierarchicalSyntaxSample
import diesel.samples.HierarchicalSyntaxSample.Ast.{Add, FloatValue, IntValue, Round, Sub, SubExpr}
import diesel.samples.HierarchicalSyntaxSample.{Ast, MyDsl}

class HierarchicalSyntaxTest extends DslTestFunSuite {

  type Ast = Ast.Value
  override def dsl: HierarchicalSyntaxSample.MyDsl.type = MyDsl

  test("int value") {
    assertAst("12") {
      IntValue(12)
    }
  }

  test("float value") {
    assertAst("12.34") {
      FloatValue(12.34)
    }
  }

  test("add") {
    assertAst("12 + 12.34") {
      Add(IntValue(12), FloatValue(12.34))
    }
  }

  test("add sub") {
    assertAst("12 + 12.34 - 2") {
      Sub(Add(IntValue(12), FloatValue(12.34)), IntValue(2))
    }
  }

  test("round") {
    assertAst("round(12 + 12.34)") {
      Round(Add(IntValue(12), FloatValue(12.34)))
    }
  }

  test("sub expression") {
    assertAst("(12 + 12.34)") {
      SubExpr(Add(IntValue(12), FloatValue(12.34)))
    }
  }

  test("sub expression bis") {
    // System.setProperty("diesel.dumpbnf.html", "dump.html")
    assertAst("1 + (12 + 12.34)") {
      Add(IntValue(1), SubExpr(Add(IntValue(12), FloatValue(12.34))))
    }
  }
}
