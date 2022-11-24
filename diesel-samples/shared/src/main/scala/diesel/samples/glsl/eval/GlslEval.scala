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

package diesel.samples.glsl.eval

import diesel.samples.glsl.Ast
import diesel.samples.glsl.Ast._
import diesel.samples.glsl.eval.Expr._
import scala.collection.mutable.ArrayBuffer

class GlslEval(var scope: Scope) {

  case class VarPart(name: String, ce: Option[ConstantExpression], i: Option[Initializer])

  private val variables = ArrayBuffer[VarPart]()

  private def pushVariable(
    name: String,
    ce: Option[ConstantExpression],
    i: Option[Initializer]
  ): Unit = {
    variables.append(
      VarPart(name, ce, i)
    )
  }

  private def flushVariables(t: FullySpecifiedType): Unit = {

    def doDeclare(name: String, e: Expr): Unit = {
      e match {
        case ERef(prevName) =>
          scope.find(prevName)
            .foreach(v => scope.declare(name, v))
        case value: EValue  =>
          scope.declare(name, value)
      }
    }

    variables.reverse.foreach(v => {
      v.ce.foreach(x => doDeclare(v.name, eval(x)))
      v.i.foreach(x => {
        doDeclare(v.name, eval(x))
      })
    })
    variables.clear()
  }

  private val funArgs                         = ArrayBuffer[(ParameterDeclarator, Option[ParameterQualifier])]()
  private var funHead: Option[FunctionHeader] = None

  def pushFunctionHeader(fh: FunctionHeader): Unit = {
    funHead = Some(fh)
  }

  def pushFunctionArg(pd: ParameterDeclarator, pq: Option[ParameterQualifier]): Unit = {
    funArgs.append((pd, pq))
  }

  def flushFunctionDeclaration(statements: Option[StatementList]): Unit = {
    funHead.foreach(h =>
      scope.declare(
        h.id,
        GlslFunc(
          funArgs.map { a =>
            GlslArg(
              a._1.id,
              a._2 match {
                case Some(Ast.PQOut)   =>
                  true
                case Some(Ast.PQInOut) =>
                  true
                case _                 =>
                  false
              }
            )
          }.toSeq,
          statements
        )
      )
    )
    funArgs.clear()
  }

  val callArgs: ArrayBuffer[ArrayBuffer[AssignmentExpression]] =
    ArrayBuffer[ArrayBuffer[AssignmentExpression]]()

  def pushCallArgs(): Unit = {
    callArgs.append(ArrayBuffer())
  }

  def popCallArgs(): Unit = {
    callArgs.remove(callArgs.length - 1)
  }

  def pushCallArg(a: AssignmentExpression): Unit = {
    callArgs(callArgs.length - 1).append(a)
  }

  def flushCallArgs(): Seq[Expr] = {
    val args = callArgs(callArgs.length - 1)
    args.reverse.map(eval).toSeq
  }

  // eval AST below

  def eval(e: PrimaryExpression): Expr = {
    e match {
      case PrimVariableIdentifier(v) =>
        ref(v.name)
      case PrimNumberConstant(value) =>
        num(value.toDouble)
      case PrimBoolConstant(value)   =>
        bool(value)
      case PrimParens(e)             =>
        eval(e)
    }
  }

  def eval(e: Expression): Expr = {
    e match {
      case EAssignmentExpression(e) =>
        eval(e)
      case EAComma(e, e2)           =>
        ???
    }
  }

  def eval(e: AssignmentExpression): Expr = {
    e match {
      case AEConditionalExpression(e) =>
        eval(e)
      case AEAssign(e, o, e2)         =>
        val left = eval(e)
        left match {
          case ERef(leftName) =>
            o match {
              case Ast.AEEqual =>
                val right = eval(e2)
                right match {
                  case ERef(rightName) =>
                    scope.find(rightName)
                      .foreach(rightVal => {
                        scope.setValue(leftName, rightVal)
                      })
                  case value: EValue   =>
                    scope.setValue(leftName, value)
                }
                voidExpr
              case _           =>
                ???
            }
          case value: EValue  =>
            ???
        }
    }
  }

  def eval(e: UnaryExpression): Expr = {
    e match {
      case UEPostfixExpr(e) =>
        eval(e)
      case UEInc(e)         =>
        ???
      case UEDec(e)         =>
        ???
      case UEUnaryOp(o, e)  =>
        val value = eval(e) match {
          case ERef(name) =>
            scope
              .find(name)
              .getOrElse(voidExpr)
          case v: EValue  =>
            v
        }
        o match {
          case Ast.UOPlus  =>
            ???
          case Ast.UODash  =>
            value.dash()
          case Ast.UOBang  =>
            ???
          case Ast.UOTilde =>
            ???
        }
    }
  }

  def eval(e: PostfixExpression): Expr = {
    e match {
      case PostPrimaryExpression(p) =>
        eval(p)
      case PostIndexedAccess(e, i)  =>
        ???
      case PostFunctionCall(f)      =>
        eval(f)
      case PostDotSelect(e, f)      =>
        val left = eval(e)
        left match {
          case ERef(name)    =>
            dotSelect(left, f)
          case value: EValue =>
            dotSelect(value, f)
        }
      case PostInc(e)               =>
        ???
      case PostDec(e)               =>
        ???
    }
  }

  private def dotSelect(e: Expr, s: String): Expr = {
    e match {
      case ERef(name)    =>
        scope
          .find(name)
          .map(v => dotSelect(v, s))
          .getOrElse(voidExpr)
      case value: EValue =>
        value.dotSelect(s)
    }
  }

  def eval(f: FunctionCall): Expr = {
    f.f match {
      case FCMGeneric(f)  =>
        eval(f)
      case FCMethod(e, f) =>
        ???
    }
  }

  def eval(fc: FunctionCallGeneric): Expr = {
    pushCallArgs()
    val res = fc match {
      case FCGWithParams(f) =>
        eval(f)
      case FCGNoParams(f)   =>
        eval(f)
    }
    popCallArgs()
    res
  }

  def eval(f: FunctionCallHeaderNoParameters): Expr = {
    eval(f.h)
  }

  def eval(f: FunctionCallHeaderWithParameters): Expr = {
    f match {
      case FCHWP1(h, a) =>
        pushCallArg(a)
        val args = flushCallArgs()
        val fId  = h.i match {
          case FITypeSpecifier(t)  =>
            t.c match {
              case Some(x) =>
                ???
              case None    =>
                eval(t.t)
            }
          case FITIdentifier(i)    =>
            i
          case FTFieldSelection(s) =>
            s
        }
        scope.findFunc(fId) match {
          case Some(f) =>
            f.call(scope, args)
          case None    =>
            throw new IllegalStateException(s"function not found ${fId}")
        }

      case FCHWP2(h, a) =>
        pushCallArg(a)
        eval(h)
    }
  }

  def eval(h: FunctionCallHeader): Expr = {
    val fId = h.i match {
      case FITypeSpecifier(t)  =>
        t.c match {
          case Some(x) =>
            ???
          case None    =>
            eval(t.t)
        }
      case FITIdentifier(i)    =>
        i
      case FTFieldSelection(s) =>
        s
    }
    scope.findFunc(fId) match {
      case Some(f) =>
        f.call(scope, Seq())
      case None    =>
        throw new IllegalStateException(s"function not found ${fId}")
    }
  }

  def eval(o: UnaryOperator): Unit = {
    ???
//    out(
//      o match {
//        case Ast.UOPlus =>
//          "+"
//        case Ast.UODash =>
//          "-"
//        case Ast.UOBang =>
//          "!"
//        case Ast.UOTilde =>
//          "~"
//      }
//    )
  }

  def eval(t: TranslationUnit): Unit = {
    t.t.foreach(x => eval(x))
    eval(t.e)
  }

  def eval(ed: ExternalDeclaration): Unit = {
    ed match {
      case EDFunc(f) =>
        eval(f)
      case EDDec(d)  =>
        eval(d)
    }
  }

  def eval(f: FunctionDefinition): Unit = {
    eval(f.p.f)
    flushFunctionDeclaration(f.s.s)
  }

  def eval(fd: FunctionDeclarator): Unit = {
    fd match {
      case FDHeader(f)           =>
        eval(f)
      case FDHeaderWithParams(f) =>
        eval(f)
    }
  }

  def eval(fh: FunctionHeader): Unit = {
    pushFunctionHeader(fh)
  }

  def eval(f: FunctionHeaderWithParameters): Unit = {
    f match {
      case FHWP1(f, p) =>
        eval(f)
        eval(p)
      case FHWP2(f, p) =>
        eval(f)
        eval(p)
    }
  }

  def eval(p: ParameterDeclaration): Unit = {
    p match {
      case PD1(t, pq, pd)  =>
        pushFunctionArg(pd, pq)
      case PD2(pq, pd)     =>
        pushFunctionArg(pd, pq)
      case PD3(t, pq, pts) =>
        ???
      case PD4(ps, pts)    =>
        ???
    }
  }

  def eval(s: CompoundStatementNoNewScope): Unit = {
    s.s.foreach(eval)
  }

  def eval(sl: StatementList): Expr = {
    sl.l.foreach(eval)
    eval(sl.s)
  }

  def eval(s: Statement): Expr = {
    s match {
      case SCompound(c) =>
        eval(c)
      case SSimple(s)   =>
        eval(s)
    }
  }

  def eval(s: CompoundStatement): Expr = {
    s.s.map(eval).getOrElse(voidExpr)
  }

  def eval(s: SimpleStatement): Expr = {
    s match {
      case SSDecl(d) =>
        eval(d.d)
        voidExpr
      case SSExp(e)  =>
        e.e.map(eval).getOrElse(voidExpr)
      case SSSel(s)  =>
        ???
      case SSIter(i) =>
        i match {
          case ISWhile(c, s)   =>
            ???
          case ISDoWhile(s, e) =>
            ???
          case ISFor(i, r, s)  =>
            // eval(i)
            println(s"$i${r.toString}$s")
            ???
        }
      case SSJump(j) =>
        eval(j)
    }
  }

  def eval(j: JumpStatement): Expr = {
    j match {
      case Ast.JSContinue =>
        ???
      case Ast.JSBreak    =>
        ???
      case JSReturn(e)    =>
        e.map(eval).getOrElse(voidExpr)
      case Ast.JSDiscard  =>
        ???
    }
  }

  def eval(f: Declaration): Unit = {
    f match {
      case DFProto(f)      =>
        ???
      case DInitDecList(f) =>
        eval(f)
    }
  }

  def eval(i: InitDeclaratorList): Unit = {
    i match {
      case IDL1(s)            =>
        s.id.foreach(varName => {
          pushVariable(varName, s.c, s.i)
          flushVariables(s.t)
        })
      case IDL2(s, id, ce, i) =>
        pushVariable(id, ce, i)
        eval(s)
    }
  }

  def eval(e: ConditionalExpression): Expr = {
    val r = eval(e.e)
    if e.rest.isDefined then {
      throw new RuntimeException("TODO")
    }
    r
//    e.rest.foreach(x => {
//      eval(x._1)
//      eval(x._2)
//    })
  }

  def eval(i: Initializer): Expr = {
    eval(i.a)
  }

  def eval(e: LogicalOrExpression): Expr = {
    e match {
      case LOELogicalXorExpression(e) =>
        eval(e)
      case LOEOr(e, e2)               =>
        ???
    }
  }

  def eval(e: LogicalXorExpression): Expr = {
    e match {
      case LXELogicalAndExpression(e) =>
        eval(e)
      case LXEXor(e, e2)              =>
        ???
    }
  }

  def eval(e: LogicalAndExpression): Expr = {
    e match {
      case LAEInclusiveOrExpression(e) =>
        eval(e)
      case LAEAnd(e, e2)               =>
        ???
    }
  }

  def eval(e: InclusiveOrExpression): Expr = {
    e match {
      case IOEExclusiveOrExpression(e) =>
        eval(e)
      case IOEIor(e, e2)               =>
        ???
    }
  }

  def eval(e: ExclusiveOrExpression): Expr = {
    e match {
      case EOEAndExpression(e) =>
        eval(e)
      case EOXor(e, e2)        =>
        ???
    }
  }

  def eval(e: AndExpression): Expr = {
    e match {
      case AEEqualityExpression(e) =>
        eval(e)
      case AEAmp(e, e2)            =>
        ???
    }
  }

  def eval(e: EqualityExpression): Expr = {
    e match {
      case EERelationalExpression(e) =>
        eval(e)
      case EEEq(e, e2)               =>
        ???
      case EENeq(e, e2)              =>
        ???
    }
  }

  def eval(e: RelationalExpression): Expr = {
    e match {
      case REShiftExpression(e) =>
        eval(e)
      case RELt(e, e2)          =>
        ???
      case REGt(e, e2)          =>
        ???
      case RELte(e, e2)         =>
        ???
      case REGte(e, e2)         =>
        ???
    }
  }

  def eval(e: ShiftExpression): Expr = {
    e match {
      case SEAdditiveExpression(e) =>
        eval(e)
      case SELeft(e, e2)           =>
        ???
      case SERight(e, e2)          =>
        ???
    }
  }

  private def binOp(e1: Expr, e2: Expr, o: (EValue, EValue) => EValue): EValue = {
    val left  = scope.toValue(e1)
    val right = scope.toValue(e2)
    o(left, right)
  }

  def eval(e: AdditiveExpression): Expr = {
    e match {
      case AEMultiplicativeExpression(e) =>
        eval(e)
      case AEAdd(e, e2)                  =>
        binOp(eval(e), eval(e2), (v1, v2) => v1.add(v2))
      case AESub(e, e2)                  =>
        binOp(eval(e), eval(e2), (v1, v2) => v1.sub(v2))
    }
  }

  def eval(m: MultiplicativeExpression): Expr = {
    m match {
      case MEUnaryExpression(e) =>
        eval(e)
      case MEMul(e, e2)         =>
        binOp(eval(e), eval(e2), (v1, v2) => v1.mul(v2))
      case MEDiv(e, e2)         =>
        binOp(eval(e), eval(e2), (v1, v2) => v1.div(v2))
      case MEMod(e, e2)         =>
        binOp(eval(e), eval(e2), (v1, v2) => v1.mod(v2))
    }
  }

  def eval(t: FullySpecifiedType): Unit = {
    t.tq.foreach(eval)
    eval(t.ts)
  }

  def eval(t: TypeQualifier): Unit = {
    ???
  }

  def eval(t: TypeSpecifier): Unit = {
    eval(t.t)
    t.c.foreach(ce => eval(ce))
  }

  def eval(t: TypeSpecifierNonArray): String = {
    t match {
      case TSNABool  =>
        "bool"
      case TSNAFloat =>
        "float"
      case TSNAInt   =>
        "float"
      case TSNAVec2  =>
        "vec2"
      case TSNAVec3  =>
        "vec3"
      case TSNAVec4  =>
        "vec4"
      case _         =>
        ???
    }
  }

  def eval(e: ConstantExpression): Expr = {
    ???
  }
//
//  def eval( i: Initializer): Unit = {
//    out("= ")
//    eval(i.a)
//  }
//
//  def compile(a: AssignmentExpression): Unit = {
//    a match {
//      case AEConditionalExpression(e) =>
//        eval(e)
//      case AEAssign(e, o, e2) =>
//        eval(e)
//        eval(o)
//        eval(e2)
//    }
//  }
//
//  def compile(expression: ConditionalExpression): Unit = {
//    eval(expression.e)
//    expression.rest.foreach((t:(Expression, AssignmentExpression)) => {
//      compile(t._1)
//      eval(t._2)
//    })
//  }
//
//  def eval(e: Expression): Unit = {
//    out("expr")
//  }
//
//
//
//
//  def eval(f: FunctionCall): Unit = {
//    eval(f.f)
//  }
//
//  def eval(f: FunctionCallOrMethod): Unit = {
//    out("funcall_TODO()")
//  }
//
//  def eval(e: IntegerExpression): Unit = {
//    eval(e.e)
//  }
//
//  def eval(e: PrimaryExpression): Unit = {
//    e match {
//      case PrimVariableIdentifier(v) =>
//        eval(v)
//      case PrimNumberConstant(value) =>
//        out(value)
//      case PrimBoolConstant(value) =>
//        out(value)
//      case PrimParens(e) =>
//        out("(")
//        eval(e)
//        out(")")
//    }
//  }
//
//  def eval(v: VariableIdentifier): Unit = {
//    out(v.name)
//  }
//
//  def eval(expression: AssignmentOperator): Unit = {
//    out("assOp")
//  }
//
//

}
