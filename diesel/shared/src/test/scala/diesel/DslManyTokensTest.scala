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

import diesel.Dsl.{Axiom, Concept, SPAndN, SPStr, Syntax}
import munit.FunSuite

class DslManyTokensTest extends FunSuite {

  object MyDsl extends Dsl {

    val string: Concept[String] = concept("\"[a-z]*\"".r, "") map ((_, t) => t.text)

    val tokens = "this is a long phrase with many tokens before a string"
      .split(" ")
      .toSeq
      .map(word => SPStr(word))

    val sManyTokens: Syntax[String] = syntax(string)(
      SPAndN(tokens :+ syntaxExprRef(string)) map {
        case (_, ts) =>
          ts.last.asInstanceOf[String]
      }
    )

    val lessTokens = "this is a string"
      .split(" ")
      .toSeq
      .map(word => SPStr(word))

    val sLessTokens: Syntax[String] = syntax(string)(
      SPAndN(lessTokens :+ syntaxExprRef(string)) map {
        case (_, ts) =>
          ts.last.asInstanceOf[String]
      }
    )

    val a: Axiom[String] = axiom(string)

  }

  test("many tokens") {
    AstHelpers.selectAst(MyDsl)(
      "this is a long phrase with many tokens before a string \"hello\""
    ) { tree =>
      assertEquals(tree.markers, Seq())
      assertEquals(tree.root.value, "\"hello\"")
    }
  }

  test("less tokens") {
    AstHelpers.selectAst(MyDsl)(
      "this is a string \"hello\""
    ) { tree =>
      assertEquals(tree.markers, Seq())
      assertEquals(tree.root.value, "\"hello\"")
    }
  }

}
