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

class DslOrTest extends FunSuite {

  sealed trait DateTime
  case class At(s: String)    extends DateTime
  case class Fun(f: Function) extends DateTime

  case class Function(name: String)

  object MyDsl extends Dsl {

    val date_time: Concept[DateTime] = concept

    val string: Concept[String]     = concept("\"[a-z]*\"".r, "") map ((_, t) => t.text)
    val function: Concept[Function] = concept

    val at: Syntax[DateTime] = syntax(date_time)(
      ("@" ~ string) | function map {
        case (_, Left((_, s))) =>
          At(s)
        case (_, Right(f))     =>
          Fun(f)
      }
    )

    val s_function: Syntax[Function] = syntax(function)(
      "[a-z]+".r ~ "(" ~ ")" map {
        case (_, (n, _, _)) =>
          Function(n.text)
      }
    )

    val a: Axiom[DateTime] = axiom(at)

  }

  test("should parse ok") {
    AstHelpers.assertAst(MyDsl)("@\"funk\"") { tree =>
      assert(tree.markers.isEmpty)
      assert(tree.root.value == At("\"funk\""))
    }
  }

}
