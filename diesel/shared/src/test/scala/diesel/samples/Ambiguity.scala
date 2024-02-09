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

import diesel.{Context, Dsl, Errors, Lexer, Marker, SimpleMarkerMessage}
import diesel.Dsl.{Axiom, Concept, Identifiers, Instance, Syntax}
import diesel.Lexer.Token

object Ambiguity {

  sealed trait Expr
  case class Constant(value: Double) extends Expr {
    override def toString: String = s"${System.identityHashCode(this)}($value)"
  }

  case class Variable(name: String) extends Expr {
    override def toString: String = s"${System.identityHashCode(this)}($name)"
  }

  case class Add(left: Expr, right: Expr) extends Expr {
    override def toString: String = s"${System.identityHashCode(this)}($left, $right)"
  }

  case class Mul(left: Expr, right: Expr) extends Expr {
    override def toString: String = s"${System.identityHashCode(this)}($left, $right)"
  }

  case class SubExpr(e: Expr) extends Expr {
    override def toString: String = s"${System.identityHashCode(this)}($e)"
  }

  object MyDsl extends Dsl with Identifiers {

    override def identScanner: Lexer.Scanner = "[a-zA-Z]+".r

    object InvalidPrecedence {
      def apply(offset: Int, len: Int): Marker = {
        Marker(Errors.SemanticError, offset, len, SimpleMarkerMessage(s"Invalid precedence"))
      }
    }

    object UnknownVariable {
      def apply(name: String, offset: Int, len: Int): Marker = {
        Marker(Errors.SemanticError, offset, len, SimpleMarkerMessage(s"Unknown variable: $name"))
      }
    }

    val number: Concept[Expr] =
      concept[Expr]("\\d+".r, Constant(0)) map ((_: Context, s: Token) => Constant(s.text.toDouble))

    val pi: Instance[Expr] = instance(number)("pi") map { c =>
      Constant(3.14)
    }

    val variable: Syntax[Expr] = syntax(number)(
      idOrKeyword map {
        case (ctx, name) =>
          ctx.addMarkers(UnknownVariable(
            name.text,
            ctx.offset,
            ctx.length
          )) // Just add error for now
          Variable(name.text)
      }
    )

    val add: Syntax[Expr] = syntax(number)(
      number ~ "+" ~ number map {
        case (ctx, (n1: Expr, _, n2: Expr)) =>
          n2 match {
            case Add(_, _) =>
              ctx.addMarkers(InvalidPrecedence(ctx.offset, ctx.length))
            case _         =>
          }
          Add(n1, n2)
      }
    )

    val mul: Syntax[Expr] = syntax(number)(
      number ~ "*" ~ number map {
        case (ctx, (n1: Expr, _, n2: Expr)) =>
          n1 match {
            case Add(_, _) =>
              ctx.addMarkers(InvalidPrecedence(ctx.offset, ctx.length))
            case _         =>
          }
          n2 match {
            case Add(_, _) =>
              ctx.addMarkers(InvalidPrecedence(ctx.offset, ctx.length))
            case Mul(_, _) =>
              ctx.addMarkers(InvalidPrecedence(ctx.offset, ctx.length))
            case _         =>
          }
          Mul(n1, n2)
      }
    )

    val subexpr: Syntax[Expr] = syntax(number)(
      "(" ~ number ~ ")" map {
        case (_, (_, n: Expr, _)) =>
          SubExpr(n)
      }
    )

    val expr: Axiom[Expr] = axiom(number)
  }
}
