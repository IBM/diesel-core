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

object Ast {

  sealed trait Expression
  case class EBoxed(e: BoxedExpression)     extends Expression
  case class ETextual(e: TextualExpression) extends Expression

  sealed trait TextualExpression
  case class TEFor(e: ForExpression)            extends TextualExpression
  case class TEIf(e: IfExpression)              extends TextualExpression
  case class TEQuant(e: QuantifiedExpression)   extends TextualExpression
  case class TEDisj(e: Disjunction)             extends TextualExpression
  case class TEConj(e: Conjunction)             extends TextualExpression
  case class TEComp(e: Comparison)              extends TextualExpression
  case class TEArith(e: ArithmeticExpression)   extends TextualExpression
  case class TEInstanceOf(e: InstanceOf)        extends TextualExpression
  case class TEPath(e: PathExpression)          extends TextualExpression
  case class TEFilter(e: FilterExpression)      extends TextualExpression
  case class TEFuncInv(e: FunctionInvocation)   extends TextualExpression
  case class TELiteral(e: Literal)              extends TextualExpression
  case class TESPUT(e: SimplePositiveUnaryTest) extends TextualExpression
  case class TEName(e: Name)                    extends TextualExpression
  case class TEParens(e: Expression)            extends TextualExpression

  sealed trait SimpleExpression
  case class SEArith(e: ArithmeticExpression) extends SimpleExpression
  case class SEVal(e: SimpleValue)            extends SimpleExpression

  sealed trait PutOp
  case object Gt  extends PutOp
  case object Gte extends PutOp
  case object Lt  extends PutOp
  case object Lte extends PutOp

  sealed trait SimplePositiveUnaryTest
  case class SPUTEndpoint(op: PutOp, e: Endpoint) extends SimplePositiveUnaryTest
  case class SPUTInterval(i: Interval)            extends SimplePositiveUnaryTest

  case class Interval(start: String, e1: Endpoint, e2: Endpoint, end: String)

  case class Endpoint(v: SimpleValue)

  case class PositiveUnaryTest(e: Expression)

  sealed trait UnaryTests
  case class UTPUT(ps: Seq[PositiveUnaryTest], not: Boolean = false) extends UnaryTests
  case object UTMinus                                                extends UnaryTests

  case class QualifiedName(vs: Seq[Name])

  case class Name(s: String)

  sealed trait SimpleLiteral
  case class SLNumeric(l: NumericLiteral)   extends SimpleLiteral
  case class SLString(l: StringLiteral)     extends SimpleLiteral
  case class SLBool(l: BooleanLiteral)      extends SimpleLiteral
  case class SLDateTime(l: DateTimeLiteral) extends SimpleLiteral
  case class NumericLiteral(v: Double)
  case class StringLiteral(v: String)
  case class BooleanLiteral(v: Boolean)
  sealed trait DateTimeLiteral
  case class DTAt(s: StringLiteral)         extends DateTimeLiteral
  case class DTFun(f: FunctionInvocation)   extends DateTimeLiteral

  sealed trait SimpleValue
  case class SVQualifiedName(n: QualifiedName) extends SimpleValue
  case class SVSimpleLiteral(l: SimpleLiteral) extends SimpleValue

  sealed trait Literal
  case class LSimple(l: SimpleLiteral) extends Literal
  case object LNull                    extends Literal

  sealed trait Parameters
  case class PNamed(ps: Seq[(Name, Expression)]) extends Parameters
  case class PPositional(es: Seq[Expression])    extends Parameters

  case class FunctionInvocation(e: Expression, p: Parameters)

  case class PathExpression(e: Expression, n: Name)

  case class ForExpression(nis: Seq[(Name, IterationContext)], retVal: Expression)

  case class IfExpression(cond: Expression, thenExpr: Expression, elseExpr: Expression)

  case class QuantifiedExpression(
    some: Boolean,
    nes: Seq[(Name, Expression)],
    satisfies: Expression
  )

  case class Disjunction(lhs: Expression, rhs: Expression)

  case class Conjunction(lhs: Expression, rhs: Expression)

  sealed trait CompareOp
  case object CEq  extends CompareOp
  case object CNeq extends CompareOp
  case object CLt  extends CompareOp
  case object CLte extends CompareOp
  case object CGt  extends CompareOp
  case object CGte extends CompareOp

  sealed trait Comparison
  case class CCompare(op: CompareOp, lhs: Expression, rhs: Expression) extends Comparison
  case class CBetween(e1: Expression, e2: Expression, e3: Expression)  extends Comparison
  case class CIn(e: Expression, ps: Seq[PositiveUnaryTest])            extends Comparison

  case class FilterExpression(e1: Expression, e2: Expression)

  case class IterationContext(e1: Expression, e2: Option[Expression])

  case class InstanceOf(e: Expression, t: Type)

  trait ParenthesizedExpression

  sealed trait ArithmeticExpression
  case class Addition(lhs: Expression, rhs: Expression)       extends ArithmeticExpression
  case class Subtraction(lhs: Expression, rhs: Expression)    extends ArithmeticExpression
  case class Multiplication(lhs: Expression, rhs: Expression) extends ArithmeticExpression
  case class Division(lhs: Expression, rhs: Expression)       extends ArithmeticExpression
  case class Exponentiation(lhs: Expression, rhs: Expression) extends ArithmeticExpression
  case class ArithmeticNegation(e: Expression)                extends ArithmeticExpression

  sealed trait BoxedExpression
  case class BEList(es: Seq[Expression])     extends BoxedExpression
  case class BEFunDef(f: FunctionDefinition) extends BoxedExpression
  case class BEContext(c: Context)           extends BoxedExpression

  case class FormalParameter(n: Name, t: Option[Type])
  case class FunctionDefinition(
    params: Seq[FormalParameter],
    e: Expression,
    external: Boolean = false
  )

  case class Context(entries: Seq[ContextEntry])
  case class ContextEntry(key: Either[Name, StringLiteral], e: Expression)

  sealed trait Type
  case class TQualified(n: QualifiedName)                  extends Type
  case class TList(t: Type)                                extends Type
  case class TContext(fields: Seq[(Name, Type)])           extends Type
  case class TFunction(argTypes: Seq[Type], retType: Type) extends Type

}
