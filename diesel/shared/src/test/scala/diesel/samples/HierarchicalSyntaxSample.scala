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

package diesel.samples

import diesel.Dsl
import diesel.Dsl.{Axiom, Concept, Syntax}

object HierarchicalSyntaxSample {

  object Ast {
    sealed trait Value

    trait Discrete extends Value

    case class IntValue(i: Int) extends Discrete

    case class FloatValue(f: Double) extends Value

    case class Add(left: Value, right: Value) extends Value

    case class Sub(left: Value, right: Value) extends Value

    case class Round(value: Value) extends Discrete

    case class SubExpr(value: Value) extends Value
  }

  import Ast._

  object MyDsl extends Dsl {

    val objectConcept: Concept[Value] = concept[Value]

    val numberConcept: Concept[Value] = concept[Value, Value](objectConcept)

    val discreteConcept: Concept[Discrete] = concept[Discrete, Value](numberConcept)

    val intConcept: Concept[IntValue] =
      concept("\\d+".r, IntValue(0), parent = Some(discreteConcept)) map {
        case (_, s) =>
          IntValue(s.text.toInt)
      }

    val floatConcept: Concept[FloatValue] =
      concept("\\d+\\.\\d+".r, FloatValue(0), parent = Some(numberConcept)) map {
        case (_, s) =>
          FloatValue(s.text.toDouble)
      }

    val add: Syntax[Value] = syntax(numberConcept, true, true)(
      numberConcept ~ "+".leftAssoc(10) ~ numberConcept map {
        case (_, (n1, _, n2)) =>
          Add(n1, n2)
      }
    )

    val sub: Syntax[Value] = syntax(numberConcept, true, true)(
      numberConcept ~ "-".leftAssoc(10) ~ numberConcept map {
        case (_, (n1, _, n2)) =>
          Sub(n1, n2)
      }
    )

    val round: Syntax[Discrete] = syntax(discreteConcept, true, true)(
      "round" ~ "(" ~ numberConcept ~ ")" map {
        case (_, (_, _, n, _)) =>
          Round(n)
      }
    )

    val subExpr: Syntax[Value] = syntax(numberConcept, true, hierarchical = false)(
      "(" ~ numberConcept ~ ")" map {
        case (_, (_, n, _)) =>
          SubExpr(n)
      }
    )

    val axiom: Axiom[Value] = axiom(objectConcept)
  }
}
