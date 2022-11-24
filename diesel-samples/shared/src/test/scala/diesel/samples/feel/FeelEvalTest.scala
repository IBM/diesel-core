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

package diesel.samples.feel

import diesel.{DslTestFunSuite, GenericTree}
import diesel.samples.feel.Ast.{Expression, _}
import diesel.samples.feel.Evaluator._
import diesel.Dsl

class FeelEvalTest extends DslTestFunSuite[Feel] {

  override def dsl                                = new Feel
  override def axiom: Some[Dsl.Axiom[Expression]] = Some(dsl.a_expression)

  type Ast = Evaluator.EValue
  override def ast(tree: GenericTree): Evaluator.EValue =
    Evaluator.eval(tree.root.value.asInstanceOf[Expression], variables)

  private val variables = Map[String, EValue](
    "foo" -> ENum(1),
    "bar" -> EStr("baz")
  )

  test("123") {
    assertAst("123") {
      ENum(123)
    }
  }

  test("1 + 2") {
    assertAst("1 + 2") {
      ENum(3)
    }
  }

  test("foo") {
    assertAst("foo") {
      ENum(1)
    }
  }

  test("if true then 1 else 2") {
    assertAst("if true then 1 else 2") {
      ENum(1)
    }
  }

  test("if false then 1 else 2") {
    assertAst("if false then 1 else 2") {
      ENum(2)
    }
  }

  test("if 1 then 1 else 2") {
    assertAst("if 1 then 1 else 2") {
      ENum(1)
    }
  }

  test("if 0 then 1 else 2") {
    assertAst("if 0 then 1 else 2") {
      ENum(2)
    }
  }

  test("true") {
    assertAst("true") {
      ETrue
    }
  }

  test("false") {
    assertAst("false") {
      EFalse
    }
  }

  test("null") {
    assertAst("null") {
      ENull
    }
  }

  test("true and true") {
    assertAst("true and true") {
      ETrue
    }
  }

  test("true or true") {
    assertAst("true or true") {
      ETrue
    }
  }

  test("true and false") {
    assertAst("true and false") {
      EFalse
    }
  }

  test("true or false") {
    assertAst("true or false") {
      ETrue
    }
  }

  test("true and 1") {
    assertAst("true and 1") {
      ENull
    }
  }

  test("true or 1") {
    assertAst("true or 1") {
      ETrue
    }
  }

  test("false and true") {
    assertAst("false and true") {
      EFalse
    }
  }

  test("false or true") {
    assertAst("false or true") {
      ETrue
    }
  }

  test("false and false") {
    assertAst("false and false") {
      EFalse
    }
  }

  test("false or false") {
    assertAst("false or false") {
      EFalse
    }
  }

  test("false and 1") {
    assertAst("false and 1") {
      EFalse
    }
  }

  test("false or 1") {
    assertAst("false or 1") {
      ENull
    }
  }

  test("1 and true") {
    assertAst("1 and true") {
      ENull
    }
  }

  test("1 or true") {
    assertAst("1 or true") {
      ETrue
    }
  }

  test("1 or false") {
    assertAst("1 or false") {
      ENull
    }
  }

  test("1 and false") {
    assertAst("1 and false") {
      EFalse
    }
  }

  test("1 and 1") {
    assertAst("1 and 1") {
      ENull
    }
  }

  test("1 or 1") {
    assertAst("1 or 1") {
      ENull
    }
  }

  test("1 = 1") {
    assertAst("1 = 1") {
      ETrue
    }
  }

  test("1 != 1") {
    assertAst("1 != 1") {
      EFalse
    }
  }

  test("1 = 2") {
    assertAst("1 = 2") {
      EFalse
    }
  }

  test("1 != 2") {
    assertAst("1 != 2") {
      ETrue
    }
  }

  test("1 < 2") {
    assertAst("1 < 2") {
      ETrue
    }
  }

  test("1 > 2") {
    assertAst("1 > 2") {
      EFalse
    }
  }

  test("1 <= 2") {
    assertAst("1 <= 2") {
      ETrue
    }
  }

  test("1 >= 2") {
    assertAst("1 >= 2") {
      EFalse
    }
  }

  test("{ foo: 1 }") {
    assertAst("{ foo: 1 }") {
      EContext(Map("foo" -> ENum(1)))
    }
  }

  test("list") {
    assertAst(
      """{
        | foo: {
        |   "bar": 2,
        |   baz: "blah"
        | },
        | yalla: true
        |}""".stripMargin
    ) {
      EContext(Map(
        "foo"   -> EContext(Map("bar" -> ENum(2.0), "baz" -> EStr("blah"))),
        "yalla" -> ETrue
      ))
    }
  }

  test("[1, 2, 3]") {
    assertAst("[1, 2, 3]") {
      EList(Seq(1, 2, 3).map(i => ENum(i)))
    }
  }

  test("[1, 2, [3, 4]]") {
    assertAst("[1, 2, [3, 4]]") {
      EList(
        Seq(
          ENum(1),
          ENum(2),
          EList(
            Seq(
              ENum(3),
              ENum(4)
            )
          )
        )
      )
    }
  }

  test("[5..10]") {
    assertAst("[5..10]") {
      ERange(ENum(5), ENum(10))
    }
  }

  test("sum([1,2])") {
    assertAst("sum([1,2])") {
      ENum(3)
    }
  }

  test("min([1,2])") {
    assertAst("min([1,2])") {
      ENum(1)
    }
  }

  test("append([1,2], 3)") {
    assertAst("append([1,2], 3)") {
      EList(Seq(ENum(1), ENum(2), ENum(3)))
    }
  }

  test("append([1,2], 3, 4)") {
    assertAst("append([1,2], 3, 4)") {
      EList(Seq(ENum(1), ENum(2), ENum(3), ENum(4)))
    }
  }

  test("function(foo) 1") {
    assertAst("function(foo) 1") {
      EDeclaredFunc(FunctionDefinition(
        List(FormalParameter(Name("foo"), None)),
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1)))))
      ))
    }
  }

  test("(function(foo) 1)(2)") {
    assertAst("(function(foo) 1)(2)") {
      ENum(1)
    }
  }

  test("(function(foo) (foo + 1))(2)") {
    assertAst("(function(foo) (foo + 1))(2)") {
      ENum(3)
    }
  }

}
