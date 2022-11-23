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

package diesel.styles

import diesel.Dsl
import diesel.Dsl.{Axiom, Instance, Syntax}
import diesel.Style
import diesel.BuiltinStyles

object MyStyledDsl extends Dsl {

  sealed trait Expr
  case class Value(i: Int)         extends Expr
  case object Pi                   extends Expr
  case class Add(l: Expr, r: Expr) extends Expr
  case class Mul(l: Expr, r: Expr) extends Expr

  sealed trait MyStyle extends Style
  case object Constant extends BuiltinStyles.WrappedStyle(BuiltinStyles.ConstantStyle) with MyStyle
  case object Literal  extends MyStyle { val name = "literal" }
  case object Keyword  extends BuiltinStyles.WrappedStyle(BuiltinStyles.KeywordStyle) with MyStyle

  val number: Dsl.Concept[Expr] = concept[Expr]("\\d+".r, Value(0))
    .tokenStyle(Literal)
    .map { (_, s) =>
      Value(s.text.toInt)
    }

  val pi: Instance[Expr] = instance(number)("pi")
    .tokenStyle(Constant)
    .map { _ =>
      Pi
    }

  val add: Syntax[Expr] = syntax(number)(
    number ~ "+".tokenStyle(Keyword).leftAssoc(10) ~ number map {
      case (_, (n1, _, n2)) =>
        Add(n1, n2)
    }
  )

  val mul: Syntax[Expr] = syntax(number)(
    number ~ "*".tokenStyle(Keyword).leftAssoc(20) ~ number map {
      case (c, (n1, minus, n2)) =>
        c.setTokenStyle(minus, Keyword)
        Mul(n1, n2)
    }
  )

  val expr: Axiom[Expr] = axiom(number)

}
