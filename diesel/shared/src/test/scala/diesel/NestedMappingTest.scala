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

package diesel

import diesel.Dsl.Axiom
import munit.FunSuite

class NestedMappingTest extends FunSuite {

  private def assertAst[T](dsl: Dsl)(text: String, expected: T): Unit = {
    AstHelpers.assertAst(dsl)(text) { t =>
      AstHelpers.assertNoMarkers(t)
      assert(expected == t.value)
    }
  }

  object Dsl1 extends Dsl {

    val concept1: Dsl.Concept[Int] = concept[Int]

    val number2: Dsl.Concept[Int] = concept[Int](
      s"\\d{2}".r,
      0
    )
      .valueToString(v => s"%02d" format v)
      .map {
        case (_, t) => {
          t.text.toInt
        }
      }

    val syntax0: Dsl.Syntax[Int] = syntax(concept1)(
      ((number2 map { (_, v) =>
        v + 100
      })
        map { (_, v) => v + 1000 })
        map { (_, v) => v + 10000 }
    )

    val axiom1: Axiom[Int] = axiom(concept1) map { (_, v) => v + 100000 }
  }

  test("parse nested mapping with same type") {
    assertAst(Dsl1)(
      "99",
      111199
    )
  }

  object Dsl2 extends Dsl {

    sealed trait Ast
    case class AstInt(v: Int)            extends Ast
    case class AstWrap(v: Ast)           extends Ast
    case class AstInt2(v: Int)           extends Ast
    case class AstPlus(v1: Ast, v2: Ast) extends Ast

    val concept1: Dsl.Concept[Ast] = concept[Ast]

    val number2: Dsl.Concept[Int] = concept[Int](
      s"\\d{2}".r,
      0
    ).valueToString(v => s"%02d" format v)
      .map {
        case (_, t) => {
          t.text.toInt
        }
      }

    val numberAstInt: Dsl.SyntaxProduction[AstInt]   = number2 map { (_, v) => AstInt(v) }
    val numberAstInt2: Dsl.SyntaxProduction[AstInt2] = number2 map { (_, v) => AstInt2(v) }

    val syntax0: Dsl.Syntax[Ast] = syntax(concept1)(
      numberAstInt
        map { (_, v) => AstWrap(v) }
    )

    val syntax1: Dsl.Syntax[Ast] = syntax(concept1)(
      // TODO remove leading +
      "+" ~ numberAstInt ~ "+" ~ numberAstInt2
        map { case (_, (_, v1, _, v2)) => AstPlus(v1, v2) }
    )

    val axiom1: Axiom[Ast] = axiom(concept1)
  }

  test("parse nested mapping with same type") {
    assertAst(Dsl2)(
      "13",
      Dsl2.AstWrap(Dsl2.AstInt(13))
    )
  }

  test("parse nested mapping with same type and two mappings") {
    assertAst(Dsl2)(
      "+13+14",
      Dsl2.AstPlus(Dsl2.AstInt(13), Dsl2.AstInt2(14))
    )
  }

}
