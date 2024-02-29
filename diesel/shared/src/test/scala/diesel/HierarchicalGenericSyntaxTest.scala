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

import diesel.samples.HierarchicalGenericSyntaxSample
import diesel.samples.HierarchicalGenericSyntaxSample.Ast.{
  Count,
  IntValue,
  Length,
  ListLiteral,
  StringValue,
  SubExpr,
  Sum,
  Target,
  Value
}
import diesel.samples.HierarchicalGenericSyntaxSample.{Ast, MyDsl}

class HierarchicalGenericSyntaxTest extends DslTestFunSuite {

  type Ast = Ast.Value
  override def dsl: HierarchicalGenericSyntaxSample.MyDsl.type = MyDsl

  test("int subexpression") {
    assertAst("( 12 )") {
      SubExpr(IntValue(12))
    }
  }

  test("string subexpression") {
    assertAst("( \"foo\" )") {
      SubExpr(StringValue("foo"))
    }
  }

  test("sum") {
    assertAst("sum({1, 2, 3})") {
      Sum(ListLiteral(Seq(IntValue(1), IntValue(2), IntValue(3))))
    }
  }

  test("count") {
    assertAst("""count({1, "foo", 3, "bar"})""") {
      Count(ListLiteral(Seq[Value](
        IntValue(1),
        StringValue("foo"),
        IntValue(3),
        StringValue("bar")
      )))
    }
  }

  test("count bis") {
    assertAst("""count({1, 3})""") {
      Count(ListLiteral(Seq[Value](
        IntValue(1),
        IntValue(3)
      )))
    }
  }

  test("simple target") {
    assertAst("discreteConcept") {
      Target("discreteConcept")
    }
  }

  test("value target") {
    assertAst("stringConcept") {
      Target("stringConcept")
    }
  }

  test("target sub expression") {
    assertAst("( discreteConcept )") {
      SubExpr(Target("discreteConcept"))
    }
  }

  test("length") {
    // System.setProperty("diesel.dumpbnf.html", "dump.html")
    assertAst("""length("foo")""") {
      Length(StringValue("foo"))
    }
  }
}
