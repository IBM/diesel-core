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
import diesel.Lexer.Scanner
import munit.FunSuite

class CustomScannerTest extends FunSuite {

  object MyScanner extends Scanner {

    val digits: Set[Char] = "0123456789".toSet

    override def name: String = "my-scanner"

    override def findPrefixOf(source: CharSequence): Option[String] = {
      var i      = 0
      var prefix = ""
      while i < source.length() && digits.contains(source.charAt(i)) do {
        prefix += source.charAt(i)
        i = i + 1
      }
      if prefix == "" then
        None
      else
        Some(prefix)
    }
  }

  case class MyInt(i: Int)
  case class MyString(s: String)
  case class MyRoot(i: MyInt, s: MyString)

  object MyDsl extends Dsl {

    val c1: Concept[MyInt] = concept(MyScanner, MyInt(0)) map ((_, t) => MyInt(t.text.toInt))

    val c2: Concept[MyString] =
      concept("[a-z]+".r, MyString("foo")) map ((_, t) => MyString(t.text))

    val s: Syntax[MyRoot] = syntax(
      c1 ~ ":" ~ c2 map {
        case (_, (a, _, b)) =>
          MyRoot(a, b)
      }
    )

    val a: Axiom[MyRoot] = axiom(s)
  }

  test("custom scanner works") {
    AstHelpers.assertAst(MyDsl)("666:bar") { tree =>
      assert(tree.value == MyRoot(MyInt(666), MyString("bar")))
    }
  }

}
