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

import diesel.BuiltinStyles.{WrappedStyle, StringStyle}
import diesel.BuiltinStyles
import diesel.Style

object Ast {

  sealed trait CalcStyle    extends Style
  case object ValueStyle    extends WrappedStyle(StringStyle) with CalcStyle
  case object ParenStyle    extends CalcStyle { val name = "parent" }
  case object KeywordStyle  extends WrappedStyle(BuiltinStyles.KeywordStyle) with CalcStyle
  case object FunctStyle    extends CalcStyle { val name = "funct"  }
  case object ConstantStyle extends WrappedStyle(BuiltinStyles.ConstantStyle) with CalcStyle

  trait Expr

  case class Value(d: Int) extends Expr

  case object Pi extends Expr

  case class Add(d1: Expr, d2: Expr) extends Expr

  case class Mul(d1: Expr, d2: Expr) extends Expr
  case class Cos(n: Expr)            extends Expr
  case class Args(args: Seq[Expr])   extends Expr
  case class Sum(args: Args)         extends Expr

}
