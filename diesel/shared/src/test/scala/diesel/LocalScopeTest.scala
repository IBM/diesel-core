/*
 * Copyright 2024 The Diesel Authors
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

import diesel.Dsl._
import diesel.Lexer.Scanner

object LocalScopeTestDsl {

  object MyDsl extends Dsl with Identifiers {

    override def identScanner: Scanner = "[a-z]+".r

    val cExpr: Concept[Expr] = concept[Expr]

    val sBlock: Syntax[Expr] = syntax(cExpr)(
      // TODO { pushes local scope
      "{" ~ cExpr.rep(true) ~ "}" map {
        case (c, (_, exprs, _)) =>
          Block(exprs)
      }
    )

    val sDeclare: Syntax[Expr] = syntax(cExpr)(
      "declare" ~ id map {
        case (c, (_, id)) =>
          // TODO add id to local scope
          Declare(id.text)
      }
    )

    val sUse: Syntax[Expr] = syntax(cExpr)(
      "use" ~ id map {
        case (c, (_, id)) =>
          // TODO raise marker, if id not in local scope
          Use(id.text)
      }
    )

    val aExpr: Axiom[Expr] = axiom(cExpr)

  }

  sealed trait Expr
  case class Block(exprs: Seq[Expr]) extends Expr
  case class Declare(name: String)   extends Expr
  case class Use(name: String)       extends Expr

  case class Undeclared(name: String) extends MarkerMessage {
    override def format(locale: String): String = s"Undeclared '${name}'"
  }
}

class LocalScopeTest extends DslTestFunSuite {
  import LocalScopeTestDsl._

  type Ast = Expr
  override def dsl: MyDsl.type = MyDsl

  test("empty") {
    assertAst("{ }") {
      Block(Seq())
    }
  }

  test("just declare ") {
    assertAst("{ declare foo }") {
      // TODO assert ast ok
      Block(Seq(Declare("foo")))
    }
  }

  test("use without declare") {
    assertMarkers("{ use foo }") {
      // TODO assert error: undeclared foo
      Seq(Marker(
        descriptor = Marker.Descriptor(
          kind = Marker.Kind.Syntactic,
          severity = Marker.Severity.Error
        ),
        offset = 0,
        length = 0,
        message = Undeclared("foo")
      ))
    }
  }

  test("use declared") {
    assertAst("{ declare foo use foo }") {
      // TODO assert ast ok
      Block(Seq(Declare("foo"), Use("foo")))
    }
  }

  test("use nested") {
    assertAst("{ declare foo { use foo } }") {
      // TODO assert ast ok
      Block(Seq(Declare("foo"), Block(Seq(Use("foo")))))
    }
  }

  test("use before declare ") {
    assertMarkers("{ use foo declare foo }") {
      Seq(Marker(
        descriptor = Marker.Descriptor(
          kind = Marker.Kind.Syntactic,
          severity = Marker.Severity.Error
        ),
        offset = 0,
        length = 0,
        message = Undeclared("foo")
      ))
    }
  }

  test("use before nested declare ") {
    assertMarkers("{ use foo { declare foo } }") {
      Seq(Marker(
        descriptor = Marker.Descriptor(
          kind = Marker.Kind.Syntactic,
          severity = Marker.Severity.Error
        ),
        offset = 0,
        length = 0,
        message = Undeclared("foo")
      ))
    }
  }

}
