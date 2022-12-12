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

import diesel.Lexer.Input
import diesel._
import diesel.styles.MyStyledDsl.{Add, Constant, Keyword, Literal, Pi, Value}

import scala.collection.mutable

class MyStyledDslTest extends DslTestFunSuite[Dsl] {

  type Ast = MyStyledDsl.Expr
  override def dsl = MyStyledDsl

  test("parse and get styles") {
    withTree("1 + pi") { tree =>
      assert(tree.markers.isEmpty)
      assertEquals(ast(tree), Add(Value(1), Pi))
      val styles = new Styles(tree)
      assertEquals(
        styles.styledRanges,
        Seq(
          StyledRange(2, 1, Keyword),
          StyledRange(0, 1, Literal),
          StyledRange(4, 2, Constant)
        )
      )
    }
  }

  test("lexer and get styles") {
    val bnf: Bnf                     = Bnf(dsl)
    val lexer                        = bnf.lexer
    val input                        = new Input("1 + pi")
    var res: (Lexer.Token, Seq[Any]) = lexer.nextWithStyles(input)
    val actualStyles                 = mutable.ArrayBuffer[StyledRange]()
    while res._1.id != Lexer.Eos do {
      if res._2.nonEmpty then
        actualStyles.append(StyledRange(
          res._1.offset,
          res._1.text.length,
          res._2.head.asInstanceOf[Style]
        ))
      res = lexer.nextWithStyles(input)
    }
    assertEquals(
      actualStyles.toSeq,
      Seq(StyledRange(0, 1, Literal), StyledRange(2, 1, Keyword), StyledRange(4, 2, Constant))
    )
  }

}
