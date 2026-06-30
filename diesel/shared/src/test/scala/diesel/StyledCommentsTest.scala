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
import diesel.Dsl.Concept
import diesel.Dsl.Identifiers
import diesel.Dsl.Syntax
import diesel.Lexer.Scanner
import munit.FunSuite
import scala.collection.mutable.ArrayBuffer
import diesel.Lexer.Token

class StyledCommentsTest extends FunSuite {

  object MyDsl extends Dsl with Identifiers with Dsl.Comments {

    case object CommentStyle extends Style {
      val name = "myComment"
    }

    override def commentScanners: Seq[Scanner] = Seq(
      "//[^\n]*(\n|$)".r
    )
    override def commentStyle: Option[Style]   = Some(CommentStyle)

    override def identScanner: Lexer.Scanner = "[a-zA-Z_][a-zA-Z0-9_]*".r

    val c: Concept[String] = concept

    val s: Syntax[String] = syntax(c)(id map { case (_, t) => t.text })

    val a: Axiom[String] = axiom(c)
  }

  val commentTokens: ArrayBuffer[Token] = new ArrayBuffer()
  private def onComment(t: Token): Unit = {
    commentTokens.append(t)
  }

  test("no comment") {
    AstHelpers.assertAsts(MyDsl, onCommentToken = Some(onComment))("foo") { navigator =>
      assertEquals(navigator.next().value, "foo")
      assertEquals(
        commentTokens.toSeq.map(t => (t.offset, t.length)),
        Seq()
      )
    }
  }
  test("with comment") {
    AstHelpers.assertAsts(MyDsl, onCommentToken = Some(onComment))("""
                                                                     |// first comment
                                                                     |foo // more comment
                                                                     |// last comment
                                                                     |""".stripMargin) {
      navigator =>
        assertEquals(navigator.next().value, "foo")
        assertEquals(
          commentTokens.toSeq.map(t => (t.offset, t.length)),
          Seq((1, 17), (22, 16), (38, 16))
        )
    }
  }

}
