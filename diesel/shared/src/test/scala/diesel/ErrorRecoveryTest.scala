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
import diesel.Marker.{Descriptor, Kind, Severity}

object ErrorRecoveryTestDsl {

  object Ast {
    sealed trait Value

    case class NumberValue(f: Int) extends Value {
      override def toString: String = f.toString
    }

    case class StringValue(s: String) extends Value

    case class Add(left: Value, right: Value) extends Value

    case class Sub(left: Value, right: Value) extends Value

    case class Concat(left: Value, right: Value) extends Value
  }

  import Ast._

  object MyDsl extends Dsl {

    val objectConcept: Concept[Value] = concept[Value]

    val numberConcept: Concept[Value] = concept[Value, Value](objectConcept)

    val stringConcept: Concept[Value] = concept[Value, Value](objectConcept)

    val intConcept: Concept[NumberValue] =
      concept("\\d+".r, NumberValue(0), parent = Some(numberConcept)) map {
        case (_, s) =>
          NumberValue(s.text.toInt)
      }

    val textConcept: Concept[StringValue] =
      concept("\"[^\"]*\"".r, StringValue(""), parent = Some(stringConcept)) map {
        case (_, s) =>
          StringValue(s.text.substring(1, s.text.length - 1))
      }

    val add: Syntax[Value] = syntax(numberConcept, hierarchical = true)(
      numberConcept ~ "+".leftAssoc(10) ~ numberConcept map {
        case (_, (n1, _, n2)) =>
          Add(n1, n2)
      }
    )

    val sub: Syntax[Value] = syntax(numberConcept, hierarchical = true)(
      numberConcept ~ "-".leftAssoc(10) ~ numberConcept map {
        case (_, (n1, _, n2)) =>
          Sub(n1, n2)
      }
    )

    val concat: Syntax[Value] = syntax(stringConcept, hierarchical = true)(
      stringConcept ~ "+".leftAssoc(10) ~ stringConcept map {
        case (_, (n1, _, n2)) =>
          Concat(n1, n2)
      }
    )

    val concat2: Syntax[Value] = syntax(stringConcept, hierarchical = true)(
      numberConcept ~ "+".leftAssoc(10) ~ stringConcept map {
        case (_, (n1, _, n2)) =>
          Concat(n1, n2)
      }
    )

    val concat3: Syntax[Value] = syntax(stringConcept, hierarchical = true)(
      stringConcept ~ "+".leftAssoc(10) ~ numberConcept map {
        case (_, (n1, _, n2)) =>
          Concat(n1, n2)
      }
    )

    val axiom: Axiom[Value] = axiom(objectConcept)
  }
}

class ErrorRecoveryTest extends DslTestFunSuite {

  import ErrorRecoveryTestDsl.Ast._

  type Ast = Value
  override def dsl: ErrorRecoveryTestDsl.MyDsl.type = ErrorRecoveryTestDsl.MyDsl

  private val syntacticError = Descriptor(Kind.Syntactic, Severity.Error)
  private val semanticError  = Descriptor(Kind.Semantic, Severity.Error)

  test("number") {
    assertAst("1") {
      NumberValue(1)
    }
  }

  test("string") {
    assertAst("\"foo\"") {
      StringValue("foo")
    }
  }

  test("add") {
    assertAst("1 + 2") {
      Add(NumberValue(1), NumberValue(2))
    }
  }

  test("add2") {
    assertAst("1 + 2 + 3") {
      Add(Add(NumberValue(1), NumberValue(2)), NumberValue(3))
    }
  }

  test("sub") {
    assertAst("1 - 2") {
      Sub(NumberValue(1), NumberValue(2))
    }
  }

  test("concat") {
    assertAst("\"foo\" + \"bar\"") {
      Concat(StringValue("foo"), StringValue("bar"))
    }
  }

  test("concat2") {
    assertAst("12 + \"bar\"") {
      Concat(NumberValue(12), StringValue("bar"))
    }
  }

  test("concat3") {
    assertAst("\"foo\" + 14") {
      Concat(StringValue("foo"), NumberValue(14))
    }
  }

  test("missing operator") {
    assertAst(
      "\"foo\" 12",
      Seq(Marker(syntacticError, 6, 2, InsertedTokenMsg("12")))
    ) {
      StringValue("foo")
    }
  }

  test("incompatible") {
    assertAst(
      "\"foo\" + 14 - 12",
      Seq(Marker(semanticError, 0, 15, IncompatibleMsg))
    ) {
      Concat(StringValue("foo"), Sub(NumberValue(14), NumberValue(12)))
    }
  }

  test("incompatible 2") {
    assertAst(
      "\"foo\" + 14 - 12 + 10",
      Seq(Marker(semanticError, 0, 20, IncompatibleMsg))
    ) {
      Concat(StringValue("foo"), Add(Sub(NumberValue(14), NumberValue(12)), NumberValue(10)))
    }
  }

  test("incompatible 3") {
    assertAst(
      "\"foo\" + 14 - 12 + \"bar\"",
      Seq(Marker(semanticError, 0, 23, IncompatibleMsg))
    ) {
      Concat(StringValue("foo"), Concat(Sub(NumberValue(14), NumberValue(12)), StringValue("bar")))
    }
  }

  test("incomplete") {
    assertAst(
      "\"foo\" + 14 - ",
      Seq(
        Marker(syntacticError, 11, 1, InsertedTokenMsg("-"))
      )
    ) {
      Concat(StringValue("foo"), NumberValue(14))
    }
  }

  test("wrong operator") {
    assertAst(
      "\"foo\" + 14 - \"bar\"",
      Seq(
        Marker(syntacticError, 11, 1, TokenMutationMsg("-", "+"))
      )
    ) {
      Concat(Concat(StringValue("foo"), NumberValue(14)), StringValue("bar"))
    }
  }

  test("empty") {
    assertAst(
      "",
      Seq(
        Marker(syntacticError, 0, 0, MissingTokenMsg("0"))
      )
    ) {
      NumberValue(0)
    }
  }
}
