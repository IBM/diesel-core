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
import diesel.Bnf.DslSyntax
import diesel.samples.calc.Ast._
import diesel.samples.calc.MyDsl
import diesel.Bnf.DslValue
import diesel.Bnf.DslInstance

class CalcTest extends DslTestFunSuite {

  type Ast = Expr
  override def dsl: MyDsl.type = MyDsl

  test("constant") {
    assertAst("1") {
      Value(1)
    }
  }

  test("expression") {
    assertAst("1 + 2") {
      Add(
        Value(1),
        Value(2)
      )
    }
  }

  test("precedence") {
    assertAst("1 + 2 * 3") {
      Add(
        Value(1),
        Mul(
          Value(2),
          Value(3)
        )
      )
    }
  }

  test("parentheses") {
    assertAst("(1 + 2) * 3") {
      Mul(
        Add(
          Value(1),
          Value(2)
        ),
        Value(3)
      )
    }
  }

  test("fun call") {
    assertAst("cos(1+2)") {
      Cos(
        Add(
          Value(1),
          Value(2)
        )
      )
    }
  }

  test("fun call arg list") {
    assertAst("sum(1, 2)") {
      Sum(
        Args(
          Seq(Value(1), Value(2))
        )
      )
    }
  }

  test("fun call arg exp") {
    assertAst("sum(1, 2*3, 45)") {
      Sum(
        Args(
          Seq(
            Value(1),
            Mul(
              Value(2),
              Value(3)
            ),
            Value(45)
          )
        )
      )
    }
  }

  test("assert number style") {
    val res               = parse(MyDsl, "123")
    assert(res.success)
    val navigator         = Navigator(res)
    assert(navigator.hasNext)
    val root: GenericTree = navigator.next()
    assert(root != null)
    assert(root.value == Value(123))
    val styles            = new Styles(root)
    val ranges            = styles.styledRanges
    assert(ranges.size == 1)
    val range             = ranges.head
    assert(range.length == 3)
    assert(range.offset == 0)
    assert(range.style == ValueStyle)
  }

  test("assert value propagation") {
    val res               = parse(MyDsl, "(123)")
    assert(res.success)
    val navigator         = Navigator(res)
    assert(navigator.hasNext)
    val root: GenericTree = navigator.next()
    assert(root != null)
    assert(root.value == Value(123))
    val styles            = new Styles(root)
    val ranges            = styles.styledRanges
    assert(ranges.size == 3)
    assert(ranges(2).offset == 1)
    assert(ranges(2).length == 3)
    assert(ranges(2).style == ValueStyle)
    assert(ranges(0).offset == 0)
    assert(ranges(0).length == 1)
    assert(ranges(0).style == ParenStyle)
    assert(ranges(1).offset == 4)
    assert(ranges(1).length == 1)
    assert(ranges(1).style == ParenStyle)
  }

  test("predict") {
    val res1 = predict(MyDsl, "", 0)
    // println(res1.map(_.text).mkString("\n"))
    assertEquals(
      res1,
      Seq(
        CompletionProposal(Some(DslValue(MyDsl.number)), "0"),
        CompletionProposal(Some(DslInstance(MyDsl.pi)), "pi"),
        CompletionProposal(Some(DslSyntax(MyDsl.cos)), "cos ("),
        CompletionProposal(Some(DslSyntax(MyDsl.subExpr)), "("),
        CompletionProposal(Some(DslSyntax(MyDsl.sumExpr)), "sum (")
      )
    )
  }

  test("predict operator") {
    val res2 = predict(MyDsl, "10 ", 3)
//    println(res2)
    assertEquals(
      res2,
      Seq(
        CompletionProposal(Some(DslSyntax(MyDsl.add)), "+"),
        CompletionProposal(Some(DslSyntax(MyDsl.mul)), "*")
      )
    )
  }
}
