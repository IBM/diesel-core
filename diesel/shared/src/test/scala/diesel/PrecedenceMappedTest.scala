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

import diesel.Dsl.{Axiom, Syntax}
import diesel.samples.calc.Ast._
import diesel.samples.calc.MathBase

object MyDslWithMappedPrededence extends MathBase {
  case class Div(d1: Expr, d2: Expr) extends Expr

  val div: Syntax[Expr] = syntax(number)(
    number ~ ("/".leftAssoc(13) map { case (_, t) => t }) ~ number map {
      case (c, (n1, plus, n2)) =>
        c.setTokenStyle(plus, KeywordStyle)
        Div(n1, n2)
    }
  )

  val expr: Axiom[Expr] = axiom(number)

}

class PrecedenceMappedTest extends DslTestFunSuite {
  import MyDslWithMappedPrededence.Div

  type Ast = Expr
  override def dsl = MyDslWithMappedPrededence

  test("unmapped precedence") {
    assertAst("1 + 2 + 3") {
      Add(
        Add(
          Value(1),
          Value(2)
        ),
        Value(3)
      )
    }
  }

  test("mapped precedence") {
    assertAst("1 / 2 / 3") {
      Div(
        Div(
          Value(1),
          Value(2)
        ),
        Value(3)
      )
    }
  }

}
