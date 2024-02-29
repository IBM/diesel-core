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

import diesel.Dsl.{Axiom, CustomToken, CustomTokens, Identifiers, Syntax}
import diesel.Lexer.{IdentifierOrKeywordToken, IdentifierToken}
import munit.FunSuite

class CustomTokenTest extends FunSuite {

  object MyDsl extends Dsl with CustomTokens with Identifiers {

    override def identScanner: Lexer.Scanner = "[a-zA-Z_][a-zA-Z0-9_]*".r

    def placeHolders: CustomToken = CustomToken("<[^\"\n\r>]+>".r, Set("<foo>", "<bar>"))

    def hashTags: CustomToken =
      CustomToken("""#[a-zA-Z0-9]+""".r, Set("#One", "#Two", "#Three"), strict = false)

    override def customTokens: Seq[CustomToken] = {
      Seq(placeHolders, hashTags)
    }

    val s: Syntax[String] = syntax("#One" ~ "<foo>" ~ id ~ "<bar>" ~ hashTags.token map {
      case (_, (o, f, i, b, x)) =>
        o.text.substring(1) + " " + f.text.substring(
          1,
          f.text.length - 1
        ) + " " + i.text + " " + b.text.substring(
          1,
          b.text.length - 1
        ) + " " + x.text.substring(1)
    })

    val a: Axiom[String] = axiom(s)
  }

  test("BNF") {
    def dsl    = MyDsl
    def bnf    = Bnf(dsl)
    def tokens = bnf.lexer.tokenRules
    assertEquals(4, tokens.size)
    assert(tokens.contains(IdentifierToken(dsl.identScanner.name)))
    assert(tokens.contains(IdentifierOrKeywordToken(dsl.identScanner.name)))
    assert(tokens.contains(dsl.hashTags.tokenId))
    assert(tokens.contains(dsl.placeHolders.tokenId))
  }

  test("default") {
    AstHelpers.selectAst(MyDsl)("#One <foo> and <bar> #xxx") { tree =>
      assertEquals(tree.value, "One foo and bar xxx")
    }
  }

  test("error place holder") {
    AstHelpers.selectAst(MyDsl)("#One <bar> and <bar> #Two") { tree =>
      assert(tree.markers.length == 1)
      assert(
        tree.markers.head.message == TokenMutationMsg("<bar>", "<foo>")
      )
      assertEquals(tree.value, "One foo and bar Two")
    }
  }

  test("error hashtag") {
    AstHelpers.selectAst(MyDsl)("#Two <foo> and <bar> #yyy") { tree =>
      assert(tree.markers.length == 1)
      assert(
        tree.markers.head.message == TokenMutationMsg("#Two", "#One")
      )
      assertEquals(tree.value, "One foo and bar yyy")
    }
  }

  test("unknown place holder") {
    AstHelpers.selectAst(MyDsl)("#One <foo> and <baz> #xxx") { tree =>
      assert(tree.markers.length == 2)
      assert(tree.markers.head.message == MissingTokenMsg("<bar>"))
      assert(tree.markers.tail.head.message == UnknownTokenMsg("<baz>"))
      assertEquals(tree.value, "One foo and bar xxx")
    }
  }
}
