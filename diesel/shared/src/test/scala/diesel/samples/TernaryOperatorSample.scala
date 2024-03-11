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
import diesel.Dsl.{Axiom, Concept, Instance}

object TernaryOperatorSample {
  object Ast {
    sealed trait Value

    case class IntValue(i: Int) extends Value

    case class BoolValue(b: Boolean) extends Value

    case class Ternary(c: Value, t: Value, e: Value) extends Value

    case class List3(e1: Value, e2: Value, e3: Value) extends Value
  }

  import Ast._

  object MyDsl extends Dsl {

    val objectConcept: Concept[Value] = concept[Value]

    val booleanConcept: Concept[Value] = concept[Value, Value](objectConcept)

    val numberConcept: Concept[IntValue] =
      concept("\\d+".r, IntValue(0), parent = Some(objectConcept)) map {
        case (_, s) =>
          IntValue(s.text.toInt)
      }

    val trueValue: Instance[Value] = instance(booleanConcept)("true") map { _ => BoolValue(true) }

    val falseValue: Instance[Value] = instance(booleanConcept)("false") map { _ =>
      BoolValue(false)
    }

    val ternary: Dsl.SyntaxGeneric[Value] = {
      syntaxGeneric[Value]
        .accept(objectConcept) { builder =>
          builder.userData(13) {
            booleanConcept ~ ("?" ~ builder.concept ~ ":").leftAssoc(10) ~ builder.concept map {
              case (_, (c, (_, t, _), e)) =>
                Ternary(c, t, e)
            }
          }
        }
    }

    val list3: Dsl.SyntaxGeneric[Value] = {
      syntaxGeneric[Value]
        .accept(objectConcept) { builder =>
          builder.userData(13) {
            "{" ~ (builder.concept ~ "," ~ builder.concept ~ "," ~ builder.concept).group ~ "}" map {
              case (_, (_, (e1, _, e2, _, e3), _)) =>
                List3(e1, e2, e3)
            }
          }
        }
    }

    val axiom: Axiom[Value] = axiom(objectConcept)
  }
}
