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

import diesel.samples.glsl.Ast.StatementList
import diesel.samples.glsl.eval.Expr._

sealed trait Func {
  def call(scope: Scope, arguments: Seq[Expr]): EValue
}

case class GlslArg(name: String, isOut: Boolean)

case class GlslFunc(args: Seq[GlslArg], body: Option[StatementList]) extends Func {

  override def call(scope: Scope, arguments: Seq[Expr]): EValue =
    body
      .map(statements => {
        // create a new scope and add all arguments into it
        val s   = scope.push()
        for ((arg, argIndex) <- args.zipWithIndex) {
          val argVal = arguments(argIndex) match {
            case ERef(name)    =>
              scope.find(name).get
            case value: EValue =>
              value
          }
          s.declare(arg.name, argVal)
        }
        // invoke function body
        val e   = new GlslEval(s)
        val res = e.eval(statements)

        // post-process out args by setting their
        // values in the parent scope
        for ((arg, argIndex) <- args.zipWithIndex) {
          if (arg.isOut) {
            arguments(argIndex) match {
              case ERef(parentScopeVarName) =>
                // it's a ref : update in the parent scope...
                val v = s.find(arg.name).get
                scope.setValue(parentScopeVarName, v)
              case value: EValue            =>
                ???
            }

          }
        }
//
//          }
//
//          .foreach {
//            case ArgOut(name, parentScopeVarName) =>
//            case _ =>
//              // noop
//          }

        res match {
          case ERef(name)    =>
            ???
          case value: EValue =>
            value
        }
      })
      .getOrElse(voidExpr)

}

object BuiltInFunction {

  private def vecN(n: Int): BuiltInFunction =
    BuiltInFunction((scope, exprs) => Vec.fromExpressions(scope, n, exprs))

  def myCos(v: EValue): EValue = {
    v match {
      case ENum(v)  =>
        ENum(Math.cos(v))
      case vec: Vec =>
        vec.mapComponents(Math.cos)
      case _        =>
        ???
    }
  }

  def myCos: BuiltInFunction = BuiltInFunction((scope, exprs) => {
    if (exprs.length == 1) {
      exprs(0) match {
        case ERef(name)    =>
          val v = scope.find(name).get
          myCos(v)
        case value: EValue =>
          myCos(value)
      }
    } else {
      throw new IllegalArgumentException("too many exprs")
    }

  })

  def myDot(values: Array[Double], values2: Array[Double]): EValue = {
    if (values.length != values2.length) {
      throw new IllegalArgumentException("dot product must have arrays of same length")
    }
    var p: Double = 0
    for (i <- values.indices) {
      p = p + values(i) * values2(i)
    }
    num(p)
  }

  val builtInFunctions: Map[String, Func] = Map(
    "vec2" -> vecN(2),
    "vec3" -> vecN(3),
    "vec4" -> vecN(4),
    "cos"  -> myCos,
    "dot"  -> BuiltInFunction((scope, exprs) => {
      if (exprs.length != 2) {
        throw new IllegalArgumentException("dot() needs 2 args")
      }
      myDot(
        toDoubles(scope, exprs(0)),
        toDoubles(scope, exprs(1))
      )
    })
  )

  private def toDoubles(s: Scope, e: Expr): Array[Double] = {
    s.toValue(e) match {
      case ENum(v)  =>
        Array(v)
      case vec: Vec =>
        vec.values
      case _        =>
        ???
    }
  }

}

case class BuiltInFunction(body: (Scope, Array[Expr]) => EValue) extends Func {
  override def call(scope: Scope, arguments: Seq[Expr]): EValue = body(scope, arguments.toArray)
}
