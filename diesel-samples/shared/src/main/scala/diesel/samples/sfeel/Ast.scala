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

object Ast {

  case class Expression(e: SimpleExpression)

  trait ArithmeticExpression
  case class Addition(lhs: Expression, rhs: Expression)       extends ArithmeticExpression
  case class Subtraction(lhs: Expression, rhs: Expression)    extends ArithmeticExpression
  case class Multiplication(lhs: Expression, rhs: Expression) extends ArithmeticExpression
  case class Division(lhs: Expression, rhs: Expression)       extends ArithmeticExpression
  case class Exponentiation(lhs: Expression, rhs: Expression) extends ArithmeticExpression
  case class ArithmeticNegation(e: Expression)                extends ArithmeticExpression

  trait SimpleExpression
  case class SEArithmeticExpression(e: ArithmeticExpression) extends SimpleExpression
  case class SESimpleValue(sv: SimpleValue)                  extends SimpleExpression
  case class SEComparison(c: Comparison)                     extends SimpleExpression

  trait SimplePositiveUnaryTest
  case class SPUTEndpoint(c: Option[String], e: Endpoint) extends SimplePositiveUnaryTest
  case class SPUTInterval(i: Interval)                    extends SimplePositiveUnaryTest

  case class Interval(start: String, e1: Endpoint, e2: Endpoint, end: String)

  trait SimpleUnaryTests
  case class SUTPositive(ts: Seq[SimplePositiveUnaryTest])    extends SimpleUnaryTests
  case class SUTPositiveNot(ts: Seq[SimplePositiveUnaryTest]) extends SimpleUnaryTests
  case object SUTMinus                                        extends SimpleUnaryTests

  trait SimpleValue
  case class SVQualifiedName(n: QualifiedName) extends SimpleValue
  case class SVSimpleLiteral(l: SimpleLiteral) extends SimpleValue

  case class Endpoint(v: SimpleValue)

  case class QualifiedName(vs: Seq[Name])

  case class Comparison(e1: Expression, s: String, e2: Expression)

  case class Name(s: String)

  trait SimpleLiteral
  case class SLNumeric(l: NumericLiteral)   extends SimpleLiteral
  case class SLString(l: StringLiteral)     extends SimpleLiteral
  case class SLBool(l: BooleanLiteral)      extends SimpleLiteral
  case class SLDateTime(l: DateTimeLiteral) extends SimpleLiteral

  case class NumericLiteral(v: Int)
  case class StringLiteral(v: String)
  case class BooleanLiteral(v: Boolean)
  case class DateTimeLiteral(v: StringLiteral)
}
