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

import Ast._

object Evaluator {

  type ValueSupplier = () => EValue

  sealed trait EValue {
    def +(o: EValue): EValue = ENull
    def isTruthy: Boolean    = false
    def or(s: ValueSupplier): EValue = {
      s() match {
        case ETrue =>
          ETrue
        case _     =>
          ENull
      }
    }

    def and(s: ValueSupplier): EValue = {
      s() match {
        case ETrue  =>
          ENull
        case EFalse =>
          EFalse
        case _      =>
          ENull
      }
    }

  }

  case object ENull extends EValue

  case class ENum(v: Double) extends EValue {
    override def +(o: EValue): EValue = o match {
      case ENum(d) =>
        ENum(v + d)
      case _       =>
        ENull
    }

    override def isTruthy: Boolean = v != 0

  }
  case class EStr(v: String) extends EValue

  sealed trait EBool extends EValue

  object EBool {
    def apply(b: Boolean): EBool =
      if (b)
        ETrue
      else
        EFalse
  }

  case object ETrue extends EBool {
    override def isTruthy: Boolean             = true
    override def or(o: ValueSupplier): EValue  = ETrue
    override def and(o: ValueSupplier): EValue =
      o() match {
        case ETrue  =>
          ETrue
        case EFalse =>
          EFalse
        case _      =>
          ENull
      }

  }

  case object EFalse extends EBool {
    override def or(o: ValueSupplier): EValue = {
      o() match {
        case bool: EBool =>
          bool
        case _           =>
          ENull
      }
    }
    override def and(o: ValueSupplier): EValue = EFalse
  }

  // TODO dates/times/durations

  case class EList(es: Seq[EValue])                extends EValue
  case class EContext(fields: Map[String, EValue]) extends EValue
  case class ERange(from: EValue, to: EValue)      extends EValue

  sealed trait EFunc extends EValue {
    def apply(scope: Scope, args: Either[Seq[EValue], Map[String, EValue]]): EValue
  }

  sealed trait EBuiltInFunc extends EFunc {
    val name: String

    override def apply(scope: Scope, args: Either[Seq[EValue], Map[String, EValue]]): EValue =
      args match {
        case Left(pos)    =>
          applyPositional(scope, pos)
        case Right(named) =>
          throw new UnsupportedOperationException("named argd not supported for built-in functions")
      }

    protected def applyPositional(scope: Evaluator.Scope, values: Seq[Evaluator.EValue]): EValue
  }

  case class EDeclaredFunc(definition: FunctionDefinition) extends EFunc {
    override def apply(scope: Scope, args: Either[Seq[EValue], Map[String, EValue]]): EValue = {
      // put arguments into scope depending on the type (positional or named)
      val variablesToDeclare = args match {
        case Left(positional) =>
          // arg count must match
          if (positional.size != definition.params.size) {
            throw new UnsupportedOperationException("invalid arg list")
          } else {
            definition.params.map(_.n.s).zip(positional)
          }

        case Right(named) =>
          named
      }
      variablesToDeclare.foreach(np => scope.declareVariable(np._1, np._2))
      eval(definition.e, scope.allVariables)

    }
  }

  def eval(e: Expression, variables: Map[String, EValue]): EValue = {
    new EvalContext(variables).eval(e)
  }

  class Scope(val parent: Option[Scope], private var variables: Map[String, EValue] = Map.empty) {

    def find(name: String): Option[EValue] =
      variables.get(name) match {
        case Some(x) =>
          Some(x)
        case None    =>
          parent match {
            case Some(x) =>
              x.find(name)
            case None    =>
              None
          }
      }

    def declareVariable(name: String, value: EValue): Scope = {
      variables = variables + (name -> value)
      this
    }

    def allVariables: Map[String, EValue] = {
      parent match {
        case Some(p) =>
          p.allVariables ++ variables
        case None    =>
          variables
      }
    }
  }

  class EvalContext(variables: Map[String, EValue] = Map.empty) {

    private var scope: Scope = BuiltInFunctions.registerBuiltins(new Scope(None, variables))

    private def pushScope(): Unit = {
      this.scope = new Scope(Some(this.scope))
    }

    private def popScope(): Unit = {
      this.scope.parent match {
        case Some(parent) =>
          this.scope = parent
        case None         =>
          throw new IllegalArgumentException("trying to pop root scope !")
      }
    }

    def eval(e: Expression): EValue = e match {
      case EBoxed(e)   =>
        eval(e)
      case ETextual(e) =>
        eval(e)
    }

    def eval(e: TextualExpression): EValue = e match {
      case TEFor(e)                                     =>
        ???
      case TEIf(IfExpression(cond, thenExpr, elseExpr)) =>
        val c = eval(cond)
        if (c.isTruthy)
          eval(thenExpr)
        else
          eval(elseExpr)
      case TEQuant(e)                                   =>
        ???
      case TEDisj(Disjunction(l, r))                    =>
        eval(l).or(() => eval(r))
      case TEConj(Conjunction(l, r))                    =>
        eval(l).and(() => eval(r))
      case TEComp(e)                                    =>
        eval(e)
      case TEArith(e)                                   =>
        e match {
          case Addition(lhs, rhs) =>
            eval(lhs) + eval(rhs)
          case _                  =>
            ???
        }
      case TEInstanceOf(e)                              =>
        ???
      case TEPath(e)                                    =>
        ???
      case TEFilter(e)                                  =>
        ???
      case TEFuncInv(FunctionInvocation(e, params))     =>
        val f = eval(e)
        f match {
          case func: EFunc =>
            try {
              pushScope()
              val args = params match {
                case PNamed(ps)      =>
                  Right(ps.map(x => (x._1.s, eval(x._2))).toMap)
                case PPositional(es) =>
                  Left(es.map(eval))
              }
              func(scope, args)
            } finally {
              popScope()
            }

          case _ =>
            throw new IllegalStateException("expression doesn't evaluate to a function")
        }

      case TELiteral(e) =>
        eval(e)
      case TESPUT(e)    =>
        e match {
          case SPUTEndpoint(op, e) =>
            ???
//            val v = eval(e)
//            op match {
//              case Ast.Gt =>
//
//              case Ast.Gte =>
//              case Ast.Lt =>
//              case Ast.Lte =>
//            }
          case SPUTInterval(i)     =>
            ERange(eval(i.e1), eval(i.e2))
        }
      case TEName(e)    =>
        scope.find(e.s) match {
          case Some(x) =>
            x
          case None    =>
            throw new IllegalStateException(s"'${e.s}' is not defined in scope $scope")
        }
      case TEParens(e)  =>
        eval(e)
    }

    def eval(e: Literal): EValue = e match {
      case LSimple(l) =>
        eval(l)
      case Ast.LNull  =>
        ENull
    }

    def eval(e: Comparison): EBool = e match {
      case CCompare(op, lhs, rhs) =>
        val v1 = eval(lhs)
        val v2 = eval(rhs)
        op match {
          case Ast.CEq  =>
            EBool(v1 == v2)
          case Ast.CNeq =>
            EBool(v1 != v2)
          case Ast.CLt  =>
            compareNumbers(v1, v2) { (d1, d2) =>
              d1 < d2
            }
          case Ast.CLte =>
            compareNumbers(v1, v2) { (d1, d2) =>
              d1 <= d2
            }
          case Ast.CGt  =>
            compareNumbers(v1, v2) { (d1, d2) =>
              d1 > d2
            }
          case Ast.CGte =>
            compareNumbers(v1, v2) { (d1, d2) =>
              d1 >= d2
            }
        }
      case CBetween(e1, e2, e3)   =>
        ???
      case CIn(e, ps)             =>
        ???
    }

    private def compareNumbers(v1: EValue, v2: EValue)(f: (Double, Double) => Boolean): EBool =
      (v1, v2) match {
        case (ENum(d1), ENum(d2)) =>
          EBool(f(d1, d2))
        case _                    =>
          throw new IllegalStateException(s"cannot compare, not numbers\n$v1\n$v2")
      }

    def eval(b: BoxedExpression): EValue = b match {
      case BEList(es)   =>
        EList(es.map(eval))
      case BEFunDef(f)  =>
        EDeclaredFunc(f)
      case BEContext(c) =>
        EContext(
          c.entries.map(e =>
            (e.key match {
              case Left(name)     =>
                name.s
              case Right(literal) =>
                literal.v
            }) -> eval(e.e)
          ).toMap
        )
    }

    def eval(e: Endpoint): EValue = eval(e.v)

    def eval(s: SimpleValue): EValue = s match {
      case SVQualifiedName(n) =>
        ???
      case SVSimpleLiteral(l) =>
        eval(l)
    }

    def eval(sl: SimpleLiteral): EValue = sl match {
      case SLNumeric(l)  =>
        ENum(l.v)
      case SLString(l)   =>
        EStr(l.v)
      case SLBool(l)     =>
        if (l.v) ETrue else EFalse
      case SLDateTime(l) =>
        ???
    }

  }

  // built-in functions
  object BuiltInFunctions {

    private def reduceNumbers(
      initialValue: Double,
      es: Seq[EValue]
    )(f: (Double, Double) => Double): ENum = ENum(
      es.foldLeft(initialValue) { (acc, v) =>
        v match {
          case ENum(i) =>
            f(acc, i)
          case _       =>
            throw new IllegalArgumentException(s"element is not a number : '$v'")
        }
      }
    )

    object Sum extends EBuiltInFunc {

      override val name: String = "sum"

      override def applyPositional(scope: Scope, args: Seq[EValue]): EValue = args.toList match {
        case ::(EList(es), Nil) =>
          reduceNumbers(0, es)((a, b) => a + b)
        case _                  =>
          throw new IllegalArgumentException("invalid args")
      }
    }

    object Min extends EBuiltInFunc {

      override val name: String = "min"

      override def applyPositional(scope: Scope, args: Seq[EValue]): EValue = args.toList match {
        case ::(EList(es), Nil) =>
          es match {
            case Nil             =>
              throw new IllegalArgumentException("Calling min() on empty list")
            case ::(ENum(i), tl) =>
              reduceNumbers(i, tl)(Math.min)
          }
        case _                  =>
          throw new IllegalArgumentException("invalid args")
      }

    }

    object Append extends EBuiltInFunc {
      override val name: String = "append"

      override def applyPositional(scope: Scope, args: Seq[EValue]): EValue = args.toList match {
        case ::(EList(es), ::(hd, tl)) =>
          EList(es ++ Seq(hd) ++ tl)
        case _                         =>
          throw new IllegalArgumentException("invalid args")
      }
    }

    def registerBuiltins(scope: Scope): Scope = {
      Seq(
        Sum,
        Min,
        Append
      ).foreach(f => scope.declareVariable(f.name, f))
      scope
    }

  }

}
