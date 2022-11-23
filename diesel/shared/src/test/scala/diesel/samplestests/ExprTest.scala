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

package diesel.samplestests

import diesel.DslTestFunSuite
import diesel.samples.Expr
import diesel.samples.Expr._

class ExprTest extends DslTestFunSuite {

  type Ast = Expression
  override def dsl = Expr

  test("binary expression") {
    assertAst("1 + 2") {
      Expression(SEArithmeticExpression(Addition(
        Expression(SESimpleValue(SVSimpleLiteral(SLNumeric(NumericLiteral(1))))),
        Expression(SESimpleValue(SVSimpleLiteral(SLNumeric(NumericLiteral(2)))))
      )))
    }
  }

  test("left assoc") {
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
}
