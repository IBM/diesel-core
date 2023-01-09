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

import diesel.Dsl.{Axiom, Concept}
import munit.FunSuite

class InfiniteLoopTest extends FunSuite {

  sealed trait Expression

  case class Function(name: String)

  object MyDsl extends Dsl {
    val value: Concept[Expression] = concept[Expression]
    val axiom: Axiom[Expression]   = axiom(value)
  }

  test("grammar cleanup removes empty axiom") {
    val bnf: Bnf = Bnf(MyDsl)
    assert(bnf.axioms.isEmpty)
  }

  test("no axiom throws from AstHelper") {
    val ex = intercept[IllegalArgumentException](AstHelpers.parse(MyDsl, "1313"))
    assert(ex.getMessage == "no axiom")
  }

  test("missing axiom throws from AstHelper") {
    val ex = intercept[IllegalArgumentException](AstHelpers.parse(MyDsl, "1313", Some(MyDsl.axiom)))
    assert(ex.getMessage == "missing axiom 'axiom'")
  }

}
