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
import diesel.Dsl.{Axiom, Concept, SPStr, Syntax}

object HierarchicalGenericSyntaxSample {

  object Ast {
    sealed trait Value

    case class StringValue(value: String) extends Value

    trait Number extends Value

    trait Discrete extends Number

    trait List[T] extends Value

    case class IntValue(i: Int) extends Discrete

    case class FloatValue(f: Double) extends Number

    case class ListLiteral[T](values: Seq[T]) extends List[T]

    case class SubExpr(value: Value) extends Value

    case class Count(values: List[Value]) extends Number

    case class Sum(values: List[Number]) extends Number

    case class Length(text: StringValue) extends Number

    case class Target(typeId: String) extends Value
  }

  import Ast._

  object MyDsl extends Dsl {

    val objectConcept: Concept[Value] = concept[Value]

    val stringConcept: Concept[StringValue] =
      concept("\"[a-z]*\"".r, StringValue(""), parent = Some(objectConcept)) map {
        case (_, s) =>
          StringValue(s.text.substring(1, s.text.length - 1))
      }

    val numberConcept: Concept[Value] = concept[Value, Value](parent = objectConcept)

    val discreteConcept: Concept[Value] = concept[Value, Value](parent = numberConcept)

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

    val count: Syntax[Value] = syntax(numberConcept)(
      "count" ~ "(" ~ objectConcept.multiple[List[Value]] ~ ")" map {
        case (_, (_, _, n, _)) =>
          Count(n)
      }
    )

    val sum: Syntax[Value] = syntax(numberConcept)(
      "sum" ~ "(" ~ numberConcept.multiple[List[Number]] ~ ")" map {
        case (_, (_, _, n, _)) =>
          Sum(n)
      }
    )

    val subExpr: Dsl.SyntaxGeneric[Value] = syntaxGeneric[Value]
      .accept(objectConcept) { builder =>
        builder {
          ("(" ~ builder.concept ~ ")") map {
            case (_, (_, expr, _)) => SubExpr(expr)
          }
        }
      }

    val listLiteral: Dsl.SyntaxGenericMulti[Value, Value] = {
      syntaxGeneric[Value]
        .accept(objectConcept)
        .multi[Value] { builder =>
          builder.userData(13) {
            ("{" ~ builder.concept ~ ("," ~ builder.concept).rep(true) ~ "}") map {
              case (_, (_, e, es, _)) =>
                ListLiteral(Seq(e) ++ es.map(_._2))
            }
          }
        }
    }

    val length: Syntax[Value] = syntax(numberConcept)(
      "length" ~ "(" ~ stringConcept ~ ")" map {
        case (_, (_, _, s, _)) =>
          Length(s)
      }
    )

    val target: Dsl.SyntaxGeneric[Value] = {
      syntaxGeneric[Value]
        .accept(objectConcept).asHierarchical() { builder =>
          builder.userData(13) {
            SPStr(builder.concept.name) map {
              case (_, typeId) =>
                Target(typeId.text)
            }
          }
        }
    }

    val axiom: Axiom[Value] = axiom(objectConcept)
  }
}
