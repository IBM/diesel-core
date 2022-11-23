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

package diesel.samples.calc

import diesel.Dsl
import diesel.Dsl._
import diesel.Lexer.Scanner
import diesel.samples.calc.Ast._

trait Calc extends Dsl with MathBase with Identifiers {

  override def identScanner: Scanner = "[a-z]+".r

  val cos: Syntax[Expr] = syntax(number)(
    "cos" ~ "(" ~ number ~ ")" map {
      case (_, (_, _, n, _)) =>
        Cos(n)
    }
  )

  val subExpr: Syntax[Expr] = syntax(number)(
    "(" ~ number ~ ")" map {
      case (c, (pl, e, pr)) => {
        c.setTokenStyle(pl, ParenStyle)
        c.setTokenStyle(pr, ParenStyle)
        e
      }
    }
  )

  val args: Syntax[Args] = syntax(
    number ~ ("," ~ number).rep(true) map {
      case (_, (head, tail)) =>
        Args(Seq(head) ++ tail.map(_._2))
    }
  )

  val sumExpr: Syntax[Expr] = syntax(number)(
    "sum" ~ "(" ~ args ~ ")" map {
      case (_, (_, _, es, _)) =>
        Sum(es)
    }
  )

}
