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

package diesel.samples.glsl

object Ast {

  case class VariableIdentifier(name: String)

  sealed trait PrimaryExpression
  case class PrimVariableIdentifier(v: VariableIdentifier) extends PrimaryExpression
  case class PrimNumberConstant(value: String)             extends PrimaryExpression
  case class PrimBoolConstant(value: Boolean)              extends PrimaryExpression
  case class PrimParens(e: Expression)                     extends PrimaryExpression

  sealed trait PostfixExpression
  case class PostPrimaryExpression(p: PrimaryExpression)                   extends PostfixExpression
  case class PostIndexedAccess(e: PostfixExpression, i: IntegerExpression) extends PostfixExpression
  case class PostFunctionCall(f: FunctionCall)                             extends PostfixExpression
  case class PostDotSelect(e: PostfixExpression, f: String)                extends PostfixExpression
  case class PostInc(e: PostfixExpression)                                 extends PostfixExpression
  case class PostDec(e: PostfixExpression)                                 extends PostfixExpression

  case class IntegerExpression(e: Expression)

  case class FunctionCall(f: FunctionCallOrMethod)

  sealed trait FunctionCallOrMethod
  case class FCMGeneric(f: FunctionCallGeneric)                     extends FunctionCallOrMethod
  case class FCMethod(e: PostfixExpression, f: FunctionCallGeneric) extends FunctionCallOrMethod

  sealed trait FunctionCallGeneric
  case class FCGWithParams(f: FunctionCallHeaderWithParameters) extends FunctionCallGeneric
  case class FCGNoParams(f: FunctionCallHeaderNoParameters)     extends FunctionCallGeneric

  case class FunctionCallHeaderNoParameters(h: FunctionCallHeader, isVoid: Boolean)

  sealed trait FunctionCallHeaderWithParameters
  case class FCHWP1(h: FunctionCallHeader, a: AssignmentExpression)
      extends FunctionCallHeaderWithParameters
  case class FCHWP2(h: FunctionCallHeaderWithParameters, a: AssignmentExpression)
      extends FunctionCallHeaderWithParameters

  case class FunctionCallHeader(i: FunctionIdentifier)

  sealed trait FunctionIdentifier
  case class FITypeSpecifier(t: TypeSpecifier) extends FunctionIdentifier
  case class FITIdentifier(i: String)          extends FunctionIdentifier
  case class FTFieldSelection(s: String)       extends FunctionIdentifier

  sealed trait UnaryExpression
  case class UEPostfixExpr(e: PostfixExpression)             extends UnaryExpression
  case class UEInc(e: UnaryExpression)                       extends UnaryExpression
  case class UEDec(e: UnaryExpression)                       extends UnaryExpression
  case class UEUnaryOp(o: UnaryOperator, e: UnaryExpression) extends UnaryExpression

  sealed trait UnaryOperator
  case object UOPlus  extends UnaryOperator
  case object UODash  extends UnaryOperator
  case object UOBang  extends UnaryOperator
  case object UOTilde extends UnaryOperator

  sealed trait MultiplicativeExpression
  case class MEUnaryExpression(e: UnaryExpression) extends MultiplicativeExpression
  case class MEMul(e: MultiplicativeExpression, e2: UnaryExpression)
      extends MultiplicativeExpression
  case class MEDiv(e: MultiplicativeExpression, e2: UnaryExpression)
      extends MultiplicativeExpression
  case class MEMod(e: MultiplicativeExpression, e2: UnaryExpression)
      extends MultiplicativeExpression

  sealed trait AdditiveExpression
  case class AEMultiplicativeExpression(e: MultiplicativeExpression)    extends AdditiveExpression
  case class AEAdd(e: AdditiveExpression, e2: MultiplicativeExpression) extends AdditiveExpression
  case class AESub(e: AdditiveExpression, e2: MultiplicativeExpression) extends AdditiveExpression

  sealed trait ShiftExpression
  case class SEAdditiveExpression(e: AdditiveExpression)         extends ShiftExpression
  case class SELeft(e: ShiftExpression, e2: AdditiveExpression)  extends ShiftExpression
  case class SERight(e: ShiftExpression, e2: AdditiveExpression) extends ShiftExpression

  sealed trait RelationalExpression
  case class REShiftExpression(e: ShiftExpression)               extends RelationalExpression
  case class RELt(e: RelationalExpression, e2: ShiftExpression)  extends RelationalExpression
  case class REGt(e: RelationalExpression, e2: ShiftExpression)  extends RelationalExpression
  case class RELte(e: RelationalExpression, e2: ShiftExpression) extends RelationalExpression
  case class REGte(e: RelationalExpression, e2: ShiftExpression) extends RelationalExpression

  sealed trait EqualityExpression
  case class EERelationalExpression(e: RelationalExpression)        extends EqualityExpression
  case class EEEq(e: EqualityExpression, e2: RelationalExpression)  extends EqualityExpression
  case class EENeq(e: EqualityExpression, e2: RelationalExpression) extends EqualityExpression

  sealed trait AndExpression
  case class AEEqualityExpression(e: EqualityExpression)     extends AndExpression
  case class AEAmp(e: AndExpression, e2: EqualityExpression) extends AndExpression

  sealed trait ExclusiveOrExpression
  case class EOEAndExpression(e: AndExpression)                 extends ExclusiveOrExpression
  case class EOXor(e: ExclusiveOrExpression, e2: AndExpression) extends ExclusiveOrExpression

  sealed trait InclusiveOrExpression
  case class IOEExclusiveOrExpression(e: ExclusiveOrExpression) extends InclusiveOrExpression
  case class IOEIor(e: InclusiveOrExpression, e2: ExclusiveOrExpression)
      extends InclusiveOrExpression

  sealed trait LogicalAndExpression
  case class LAEInclusiveOrExpression(e: InclusiveOrExpression)         extends LogicalAndExpression
  case class LAEAnd(e: LogicalAndExpression, e2: InclusiveOrExpression) extends LogicalAndExpression

  sealed trait LogicalXorExpression
  case class LXELogicalAndExpression(e: LogicalAndExpression)          extends LogicalXorExpression
  case class LXEXor(e: LogicalXorExpression, e2: LogicalAndExpression) extends LogicalXorExpression

  sealed trait LogicalOrExpression
  case class LOELogicalXorExpression(e: LogicalXorExpression)        extends LogicalOrExpression
  case class LOEOr(e: LogicalOrExpression, e2: LogicalXorExpression) extends LogicalOrExpression

  case class ConditionalExpression(
    e: LogicalOrExpression,
    rest: Option[Tuple2[Expression, AssignmentExpression]]
  )

  sealed trait AssignmentExpression
  case class AEConditionalExpression(e: ConditionalExpression) extends AssignmentExpression
  case class AEAssign(e: UnaryExpression, o: AssignmentOperator, e2: AssignmentExpression)
      extends AssignmentExpression

  sealed trait AssignmentOperator
  case object AEEqual     extends AssignmentOperator
  case object AEMulAssign extends AssignmentOperator
  case object AEDivAssign extends AssignmentOperator
  case object AEModAssign extends AssignmentOperator
  case object AEAddAssign extends AssignmentOperator
  case object AESubAssign extends AssignmentOperator

  sealed trait Expression
  case class EAssignmentExpression(e: AssignmentExpression)   extends Expression
  case class EAComma(e: Expression, e2: AssignmentExpression) extends Expression

  case class ConstantExpression(e: ConditionalExpression)

  sealed trait Declaration
  case class DFProto(f: FunctionPrototype)       extends Declaration
  case class DInitDecList(f: InitDeclaratorList) extends Declaration

  case class FunctionPrototype(f: FunctionDeclarator)

  sealed trait FunctionDeclarator
  case class FDHeader(f: FunctionHeader)                         extends FunctionDeclarator
  case class FDHeaderWithParams(f: FunctionHeaderWithParameters) extends FunctionDeclarator

  sealed trait FunctionHeaderWithParameters
  case class FHWP1(f: FunctionHeader, p: ParameterDeclaration) extends FunctionHeaderWithParameters
  case class FHWP2(f: FunctionHeaderWithParameters, p: ParameterDeclaration)
      extends FunctionHeaderWithParameters

  case class FunctionHeader(f: FullySpecifiedType, id: String)

  case class ParameterDeclarator(t: TypeSpecifier, id: String, e: Option[ConstantExpression])

  sealed trait ParameterDeclaration
  case class PD1(t: TypeQualifier, pq: Option[ParameterQualifier], pd: ParameterDeclarator)
      extends ParameterDeclaration
  case class PD2(pq: Option[ParameterQualifier], pd: ParameterDeclarator)
      extends ParameterDeclaration
  case class PD3(t: TypeQualifier, pq: Option[ParameterQualifier], pts: ParameterTypeSpecifier)
      extends ParameterDeclaration
  case class PD4(ps: Option[ParameterQualifier], pts: ParameterTypeSpecifier)
      extends ParameterDeclaration

  sealed trait ParameterQualifier
  case object PQIn    extends ParameterQualifier
  case object PQOut   extends ParameterQualifier
  case object PQInOut extends ParameterQualifier

  case class ParameterTypeSpecifier(t: TypeSpecifier)

  sealed trait InitDeclaratorList
  case class IDL1(s: SingleDeclaration) extends InitDeclaratorList
  case class IDL2(
    s: InitDeclaratorList,
    id: String,
    c: Option[ConstantExpression],
    i: Option[Initializer]
  ) extends InitDeclaratorList

  case class SingleDeclaration(
    t: FullySpecifiedType,
    id: Option[String],
    c: Option[ConstantExpression],
    i: Option[Initializer]
  )

  case class FullySpecifiedType(tq: Option[TypeQualifier], ts: TypeSpecifier)

  sealed trait TypeQualifier
  case object TQConst     extends TypeQualifier
  case object TQAttribute extends TypeQualifier
  case object TQVarying   extends TypeQualifier
  case object TQCentroid  extends TypeQualifier
  case object TQUniform   extends TypeQualifier

  case class TypeSpecifier(t: TypeSpecifierNonArray, c: Option[ConstantExpression])

  sealed trait TypeSpecifierNonArray
  case object TSNAVoid                      extends TypeSpecifierNonArray
  case object TSNAFloat                     extends TypeSpecifierNonArray
  case object TSNAInt                       extends TypeSpecifierNonArray
  case object TSNABool                      extends TypeSpecifierNonArray
  case object TSNAVec2                      extends TypeSpecifierNonArray
  case object TSNAVec3                      extends TypeSpecifierNonArray
  case object TSNAVec4                      extends TypeSpecifierNonArray
  case object TSNABVec2                     extends TypeSpecifierNonArray
  case object TSNABVec3                     extends TypeSpecifierNonArray
  case object TSNABVec4                     extends TypeSpecifierNonArray
  case object TSNAIVec2                     extends TypeSpecifierNonArray
  case object TSNAIVec3                     extends TypeSpecifierNonArray
  case object TSNAIVec4                     extends TypeSpecifierNonArray
  case object TSNAMat2                      extends TypeSpecifierNonArray
  case object TSNAMat3                      extends TypeSpecifierNonArray
  case object TSNAMat4                      extends TypeSpecifierNonArray
  case object TSNAMat22                     extends TypeSpecifierNonArray
  case object TSNAMat23                     extends TypeSpecifierNonArray
  case object TSNAMat24                     extends TypeSpecifierNonArray
  case object TSNAMat32                     extends TypeSpecifierNonArray
  case object TSNAMat33                     extends TypeSpecifierNonArray
  case object TSNAMat34                     extends TypeSpecifierNonArray
  case object TSNAMat42                     extends TypeSpecifierNonArray
  case object TSNAMat43                     extends TypeSpecifierNonArray
  case object TSNAMat44                     extends TypeSpecifierNonArray
  case object TSNASampler1D                 extends TypeSpecifierNonArray
  case object TSNASampler2D                 extends TypeSpecifierNonArray
  case object TSNASampler3D                 extends TypeSpecifierNonArray
  case object TSNASamplerCube               extends TypeSpecifierNonArray
  case object TSNASampler1DShadow           extends TypeSpecifierNonArray
  case object TSNASampler2DShadow           extends TypeSpecifierNonArray
  case class TSNAStruct(s: StructSpecifier) extends TypeSpecifierNonArray
  case class TSNATypeName(t: String)        extends TypeSpecifierNonArray

  case class StructSpecifier(name: Option[String], s: StructDeclarationList)

  case class StructDeclarationList(sl: Option[StructDeclarationList], sd: StructDeclaration)

  case class StructDeclaration(t: TypeSpecifier, s: StructDeclaratorList)

  case class StructDeclaratorList(sl: Option[StructDeclaratorList], sd: StructDeclarator)

  case class StructDeclarator(name: String, e: Option[ConstantExpression])

  case class Initializer(a: AssignmentExpression)

  case class DeclarationStatement(d: Declaration)

  sealed trait Statement
  case class SCompound(c: CompoundStatement) extends Statement
  case class SSimple(s: SimpleStatement)     extends Statement

  sealed trait SimpleStatement
  case class SSDecl(d: DeclarationStatement) extends SimpleStatement
  case class SSExp(e: ExpressionStatement)   extends SimpleStatement
  case class SSSel(s: SelectionStatement)    extends SimpleStatement
  case class SSIter(i: IterationStatement)   extends SimpleStatement
  case class SSJump(j: JumpStatement)        extends SimpleStatement

  case class CompoundStatement(s: Option[StatementList])

  sealed trait StatementNoNewScope
  case class SNNSCompound(d: CompoundStatementNoNewScope) extends StatementNoNewScope
  case class SNSSSimple(s: SimpleStatement)               extends StatementNoNewScope

  case class CompoundStatementNoNewScope(s: Option[StatementList])

  case class StatementList(s: Statement, l: Option[StatementList])

  case class ExpressionStatement(e: Option[Expression])

  case class SelectionStatement(e: Expression, r: SelectionRestStatement)

  case class SelectionRestStatement(t: Statement, e: Option[Statement])

  sealed trait Condition
  case class CExpression(e: Expression)                                   extends Condition
  case class CTypeInit(t: FullySpecifiedType, id: String, i: Initializer) extends Condition

  sealed trait IterationStatement
  case class ISWhile(c: Condition, s: StatementNoNewScope) extends IterationStatement
  case class ISDoWhile(s: Statement, e: Expression)        extends IterationStatement
  case class ISFor(i: ForInitStatement, r: ForRestStatement, s: StatementNoNewScope)
      extends IterationStatement

  sealed trait ForInitStatement
  case class FIExpr(e: ExpressionStatement)  extends ForInitStatement
  case class FIDecl(e: DeclarationStatement) extends ForInitStatement

  case class ForRestStatement(c: Option[Condition], e: Expression)

  sealed trait JumpStatement
  case object JSContinue                     extends JumpStatement
  case object JSBreak                        extends JumpStatement
  case class JSReturn(e: Option[Expression]) extends JumpStatement
  case object JSDiscard                      extends JumpStatement

  case class TranslationUnit(t: Option[TranslationUnit], e: ExternalDeclaration)

  sealed trait ExternalDeclaration
  case class EDFunc(f: FunctionDefinition) extends ExternalDeclaration
  case class EDDec(d: Declaration)         extends ExternalDeclaration

  case class FunctionDefinition(p: FunctionPrototype, s: CompoundStatementNoNewScope)

}
