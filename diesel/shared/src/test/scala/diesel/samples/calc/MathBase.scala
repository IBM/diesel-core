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

import diesel.Dsl._
import diesel.Lexer.Scanner
import diesel.samples.calc.Ast._
import diesel.{Context, Dsl}

trait MathBase extends Dsl {

  val numberScanner: Scanner = new Scanner {
    override def name: String = "integer"

    override def findPrefixOf(source: CharSequence): Option[String] = {
      val leadingDigits = (0 until source.length)
        .map(source.charAt)
        .takeWhile(Character.isDigit)
        .length
      Some(leadingDigits)
        .filter(_ > 0)
        .map(source.subSequence(0, _).toString)
    }
  }

  val number: Concept[Expr] = concept[Expr](numberScanner, Value(0))
    .valueToString((v: Expr) => v.asInstanceOf[Value].d.toString) map { (c: Context, s) =>
    c.setStyle(ValueStyle)
    Value(s.text.toInt)
  }

  val pi: Instance[Expr] = instance(number)("pi") map { c =>
    c.setStyle(ConstantStyle)
    Pi
  }

  val add: Syntax[Expr] = syntax(number)(
    number ~ "+".leftAssoc(10) ~ number map {
      case (c, (n1, plus, n2)) =>
        c.setTokenStyle(plus, KeywordStyle)
        Add(n1, n2)
    }
  )

  val mul: Syntax[Expr] = syntax(number)(
    number ~ "*".leftAssoc(20) ~ number map {
      case (c, (n1, mul, n2)) =>
        c.setTokenStyle(mul, KeywordStyle)
        Mul(n1, n2)
    }
  )

}
