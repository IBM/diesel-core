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

import diesel.Dsl._
import diesel.Lexer.Scanner

object LocalScopeTestDsl {

  object MyDsl extends Dsl with Identifiers {

    object UnknownId {
      def apply(name: String, offset: Int, len: Int): Marker = {
        Marker(Errors.SyntacticError, offset, len, SimpleMarkerMessage(s"Unknown variable: $name"))
      }
    }

    case class Variables(data: Map[String, Int] = Map()) {

      def getVar(key: String): Option[Int] = data.get(key)

      def setVar(key: String, value: Int): Variables = {
        Variables(data + (key -> value))
      }
    }

    override def identScanner: Scanner = "[a-z]+".r

    val cExpr: Concept[Expr] = concept[Expr]

    val sBlock: Syntax[Expr] = syntax(cExpr, contextual = true)(
      "{" ~ cExpr.rep(true) ~ "}" map {
        case (_, (_, exprs, _)) =>
          Block(exprs)
      }
    )

    val sDeclare: Syntax[Expr] = syntax(cExpr)(
      "declare" ~ id map {
        case (ctx, (_, id)) =>
          val variables = ctx.contextualUserData.get("variables") match {
            case Some(value: Variables) => value
            case _                      => Variables()
          }
          ctx.contextualUserData.set("variables", variables.setVar(id.text, 0))
          Declare(id.text)
      }
    )

    val sUse: Syntax[Expr] = syntax(cExpr)(
      "use" ~ id map {
        case (ctx, (_, id)) =>
          val exists = ctx.contextualUserData.get("variables") match {
            case Some(value: Variables) =>
              value.getVar(id.text).isDefined
            case Some(_)                => false
            case None                   => false
          }
          if (!exists) {
            ctx.addMarkers(UnknownId(id.text, id.offset, id.length))
          }
          Use(id.text)
      }
    )

    val aExpr: Axiom[Expr] = axiom(cExpr)

  }

  sealed trait Expr
  case class Block(exprs: Seq[Expr]) extends Expr
  case class Declare(name: String)   extends Expr
  case class Use(name: String)       extends Expr
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
      Block(Seq(Declare("foo")))
    }
  }

  test("use without declare") {
    assertMarkers("{ use foo }") {
      Seq(Marker(
        descriptor = Marker.Descriptor(
          kind = Marker.Kind.Syntactic,
          severity = Marker.Severity.Error
        ),
        offset = 6,
        length = 3,
        message = SimpleMarkerMessage("Unknown variable: foo")
      ))
    }
  }

  test("use declared") {
    assertAst("{ declare foo use foo }") {
      Block(Seq(Declare("foo"), Use("foo")))
    }
  }

  test("use nested") {
    assertAst("{ declare foo { use foo } }") {
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
        offset = 6,
        length = 3,
        message = SimpleMarkerMessage("Unknown variable: foo")
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
        offset = 6,
        length = 3,
        message = SimpleMarkerMessage("Unknown variable: foo")
      ))
    }
  }

  test("use after nested declare ") {
    assertMarkers("{ { declare foo use foo } use foo }") {
      Seq(Marker(
        descriptor = Marker.Descriptor(
          kind = Marker.Kind.Syntactic,
          severity = Marker.Severity.Error
        ),
        offset = 30,
        length = 3,
        message = SimpleMarkerMessage("Unknown variable: foo")
      ))
    }
  }
}
