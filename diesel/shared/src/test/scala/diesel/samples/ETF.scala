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

package diesel.samples

import diesel.Dsl
import diesel.Dsl.{Axiom, Concept, Syntax}

object ETF extends Dsl {

  trait Expression

  trait ArithmeticExpression                                  extends Expression
  case class Addition(lhs: Expression, rhs: Expression)       extends ArithmeticExpression
  case class Multiplication(lhs: Expression, rhs: Expression) extends ArithmeticExpression

  case class NumericLiteral(v: Int) extends Expression

  val numeric_literal: Concept[NumericLiteral] =
    concept("[0-9]+".r, NumericLiteral(0)) map ((_, t) => NumericLiteral(t.text.toInt))

  val factor: Syntax[Expression] = syntax(
    numeric_literal map {
      case (_, l) => l
    }
  )

  val multiplication: Syntax[ArithmeticExpression] = syntax(
    term ~ "*" ~ factor map {
      case (_, (l, _, r)) =>
        Multiplication(l, r)
    }
  )

  def term: Syntax[Expression] = syntax(
    (multiplication | factor) map {
      case (_, Left(m))  => m
      case (_, Right(f)) => f
    }
  )

  val addition: Syntax[ArithmeticExpression] = syntax(
    expression ~ "+" ~ term map {
      case (_, (l, _, r)) =>
        Addition(l, r)
    }
  )

  def expression: Syntax[Expression] = syntax(
    (addition | term) map {
      case (_, Left(a))  => a
      case (_, Right(t)) => t
    }
  )

  val a_expression: Axiom[Expression] = axiom(expression)
}
