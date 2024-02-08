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

import diesel.Dsl.{Axiom, Concept, Syntax}
import munit.FunSuite

class CardinalityInDslTest extends FunSuite {

  sealed trait AstNodeSingle
  case class AInt(value: Int)             extends AstNodeSingle
  case class ALength(value: AstNodeMulti) extends AstNodeSingle

  sealed trait AstNodeMulti
  case class APrime(value: AstNodeSingle)                        extends AstNodeMulti
  case class AFirstK(value: AstNodeSingle, values: AstNodeMulti) extends AstNodeMulti

  object MyDsl extends Dsl {

    val number: Concept[AstNodeSingle] = concept[AstNodeSingle]("\\d+".r, AInt(0)) map {
      case (_, t) =>
        AInt(t.text.toInt)
    }

    val primeNumbers: Syntax[AstNodeMulti] = syntax
      .typed(number)
      .multi[AstNodeMulti](
        "prime" ~ number map {
          case (_, (_, i)) =>
            APrime(i)
        }
      )

    val firstK: Syntax[AstNodeMulti] = syntax
      .typed(number)
      .multi[AstNodeMulti](
        "firstK" ~ number ~ "," ~ number.multiple[AstNodeMulti] map {
          case (_, (_, n, _, n2)) =>
            AFirstK(n, n2)
        }
      )

    val lengthOf: Syntax[AstNodeSingle] = syntax(number)(
      "length" ~ "of" ~ number /*.article(DefiniteArticle)*/ .multiple[AstNodeMulti] map {
        case (_, (_, _, m)) =>
          ALength(m)
      }
    )

    val a: Axiom[AstNodeSingle] = axiom(number)

  }

  test("simple") {
    val text =
      """
        |length of firstK 10 , prime 123
        |""".stripMargin
    AstHelpers.selectAst(MyDsl)(text) { tree =>
      assert(tree.markers.isEmpty)
      assert(
        tree.root.value == ALength(AFirstK(AInt(10), APrime(AInt(123))))
      )
    }
  }

}
