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

class EmptyRegexTest extends FunSuite {

  object MyDsl extends Dsl {

    val c1: Concept[String] = concept("".r, "") map { case (_, _) => "yalla" }

    val a1: Axiom[String] = axiom(c1)

  }

  test("empty regex throws") {
    val ex = intercept[IllegalArgumentException](AstHelpers.parse(MyDsl, "1313"))
    assertEquals(ex.getMessage, "found scanner matching empty string RegexScanner(), tokenId=c1")
  }

}
