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

package diesel.samples.sfeel

import diesel.DslTestFunSuite
import diesel.samples.sfeel.Ast._
import diesel.Dsl

class SFeelTest extends DslTestFunSuite {

  override def dsl: SFeel.type                    = SFeel
  override def axiom: Some[Dsl.Axiom[Expression]] = Some(dsl.a_expression)

  type Ast = Expression

  test("true") {
    assertAst("true") {
      Expression(SESimpleValue(SVSimpleLiteral(SLBool(BooleanLiteral(true)))))
    }
  }

  test("123") {
    assertAst("123") {
      Expression(SESimpleValue(SVSimpleLiteral(SLNumeric(NumericLiteral(123)))))
    }
  }

  test("\"foo\"") {
    assertAst("\"foo\"") {
      Expression(SESimpleValue(SVSimpleLiteral(SLString(StringLiteral("foo")))))
    }
  }

  test("""date("foo")""") {
    assertAst("""date("foo")""") {
      Expression(SESimpleValue(SVSimpleLiteral(SLDateTime(DateTimeLiteral(StringLiteral("foo"))))))
    }
  }

  test("yalla") {
    assertAst("yalla") {
      Expression(SESimpleValue(SVQualifiedName(QualifiedName(Seq(Name("yalla"))))))
    }
  }

  test("yalla.again") {
    assertAst("yalla.again") {
      Expression(SESimpleValue(SVQualifiedName(QualifiedName(List(Name("yalla"), Name("again"))))))
    }
  }

  test("1 > 2") {
    assertAst("1 > 2") {
      Expression(SEComparison(Comparison(
        Expression(SESimpleValue(SVSimpleLiteral(SLNumeric(NumericLiteral(1))))),
        "TODO",
        Expression(SESimpleValue(SVSimpleLiteral(SLNumeric(NumericLiteral(2)))))
      )))
    }
  }

  test("1 + 2") {
    assertAst("1 + 2") {
      Expression(SEArithmeticExpression(Addition(
        Expression(SESimpleValue(SVSimpleLiteral(SLNumeric(NumericLiteral(1))))),
        Expression(SESimpleValue(SVSimpleLiteral(SLNumeric(NumericLiteral(2)))))
      )))
    }
  }

  test("1 + 2 + 3") {
    assertAst("1 + 2 + 3") {
      Expression(SEArithmeticExpression(Addition(
        Expression(SEArithmeticExpression(Addition(
          Expression(SESimpleValue(SVSimpleLiteral(SLNumeric(NumericLiteral(1))))),
          Expression(SESimpleValue(SVSimpleLiteral(SLNumeric(NumericLiteral(2)))))
        ))),
        Expression(SESimpleValue(SVSimpleLiteral(SLNumeric(NumericLiteral(3)))))
      )))
    }
  }

  test("1 + 2 > 3") {
    assertAst("1 + 2 > 3") {
      Expression(SEArithmeticExpression(Addition(
        Expression(SESimpleValue(SVSimpleLiteral(SLNumeric(NumericLiteral(1))))),
        Expression(SEComparison(Comparison(
          Expression(SESimpleValue(SVSimpleLiteral(SLNumeric(NumericLiteral(2))))),
          "TODO",
          Expression(SESimpleValue(SVSimpleLiteral(SLNumeric(NumericLiteral(3)))))
        )))
      )))
    }
  }

}

class SFeelSimpleUnaryTestsTest extends DslTestFunSuite {

  override def dsl: SFeel.type = SFeel
  type Ast = SimpleUnaryTests

  test("[1..2]") {
    assertAst("[1..2]") {
      SUTPositive(List(SPUTInterval(Interval(
        "[",
        Endpoint(SVSimpleLiteral(SLNumeric(NumericLiteral(1)))),
        Endpoint(SVSimpleLiteral(SLNumeric(NumericLiteral(2)))),
        "]"
      ))))
    }
  }

  test("[1..bar]") {
    assertAst("[1..bar]") {
      SUTPositive(List(SPUTInterval(Interval(
        "[",
        Endpoint(SVSimpleLiteral(SLNumeric(NumericLiteral(1)))),
        Endpoint(SVQualifiedName(QualifiedName(List(Name("bar"))))),
        "]"
      ))))
    }
  }

}
