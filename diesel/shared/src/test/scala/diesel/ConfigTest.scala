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

import diesel.AstHelpers._
import diesel.samples.Config
import diesel.samples.Config.Ast
import diesel.samples.Config.Ast.{Name, Section, Value}
import munit.FunSuite

class ConfigTest extends FunSuite {

  test("One item") {
    doCheck(
      "[bla]\na=hello\n",
      Ast.Config(
        Seq(
          Section(Name("bla"), Seq(Ast.Item(Name("a"), Some(Value("hello")))))
        )
      )
    )
  }

  test("Several items") {
    doCheck(
      "[bla]\na=hello\nthis=\"that\",4\nempty=\n",
      Ast.Config(
        Seq(
          Section(
            Name("bla"),
            Seq(
              Ast.Item(Name("a"), Some(Value("hello"))),
              Ast.Item(Name("this"), Some(Value("\"that\",4"))),
              Ast.Item(Name("empty"), None)
            )
          )
        )
      )
    )
  }

  def doCheck(text: String, expected: Ast.Config): Unit = {
//    println(s">>>\n~input~\n$text")
    val result = parse(Config.Config, text)
    assert(result.success)
    val n      = Navigator(result)
    assert(n.hasNext)
    val tree   = n.next()
    assertNoMarkers(tree)
    val actual = tree.value
    assert(!n.hasNext)
//    println(s"~ast~\n$actual")
    assert(actual == expected)
  }
}
