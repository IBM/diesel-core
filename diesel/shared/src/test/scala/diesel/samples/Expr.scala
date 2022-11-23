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

object Expr extends Dsl {

  trait SimpleLiteral
  case class SLNumeric(l: NumericLiteral) extends SimpleLiteral

  trait SimpleValue
  case class SVSimpleLiteral(l: SimpleLiteral) extends SimpleValue

  trait ArithmeticExpression
  case class Addition(lhs: Expression, rhs: Expression) extends ArithmeticExpression

  trait SimpleExpression
  case class SEArithmeticExpression(e: ArithmeticExpression) extends SimpleExpression
  case class SESimpleValue(sv: SimpleValue)                  extends SimpleExpression

  case class NumericLiteral(v: Int)

  case class Expression(e: SimpleExpression)

  val expression: Concept[Expression]                      = concept
  val simple_expression: Concept[SimpleExpression]         = concept
  val arithmetic_expression: Concept[ArithmeticExpression] = concept
  val simple_value: Concept[SimpleValue]                   = concept
  val simple_literal: Concept[SimpleLiteral]               = concept
  val numeric_literal: Concept[NumericLiteral]             =
    concept("[0-9]+".r, NumericLiteral(0)) map ((_, t) => NumericLiteral(t.text.toInt))

  val s_expression: Syntax[Expression] = syntax(expression)(
    simple_expression map {
      case (_, e) =>
        Expression(e)
    }
  )

  val s_simple_expr_1: Syntax[SimpleExpression] = syntax(simple_expression)(
    arithmetic_expression map {
      case (_, a) =>
        SEArithmeticExpression(a)
    }
  )

  val s_simple_expr_2: Syntax[SimpleExpression] = syntax(simple_expression)(
    simple_value map {
      case (_, sv) =>
        SESimpleValue(sv)
    }
  )

  val s_simple_value: Syntax[SimpleValue] = syntax(simple_value)(
    simple_literal map {
      case (_, sl) =>
        SVSimpleLiteral(sl)
    }
  )

  val s_addition: Syntax[ArithmeticExpression] = syntax(arithmetic_expression)(
    expression ~ "+".leftAssoc(10) ~ expression map {
      case (_, (l, _, r)) =>
        Addition(l, r)
    }
  )

  val s_simple_literal_1: Syntax[SimpleLiteral] = syntax(simple_literal)(
    numeric_literal map {
      case (_, l) =>
        SLNumeric(l)
    }
  )

  val a_expression: Axiom[Expression] = axiom(expression)
}
