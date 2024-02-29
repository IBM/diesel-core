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

import diesel.{Context, Dsl}
import diesel.Dsl.{Axiom, Concept, Instance, Syntax}
import diesel.Lexer.Token;

object AmbiguousSyntax {

  sealed trait Expr

  case class Constant(value: Double) extends Expr {
    override def toString: String = s"${System.identityHashCode(this)}($value)"
  }

  sealed trait Person extends Expr

  case class NamedPerson(name: String) extends Person

  case class AgeOf(person: Person) extends Expr

  case class Brother(person: Person) extends Person

  case class House(owner: Person) extends Expr

  case class Owner(house: House) extends Person

  case class Pair(car: Expr, cdr: Expr) extends Expr

  object MyDsl extends Dsl {

    val value: Concept[Expr] = concept[Expr]

    val number: Concept[Expr] =
      concept[Expr, Expr]("\\d+".r, Constant(0), parent = Some(value)) map (
        (_: Context, s: Token) => Constant(s.text.toDouble)
      )

    val person: Concept[Person] = concept(value)

    val house: Concept[House] = concept(value)

    val john: Instance[Person] = instance(person)("john") map { _ =>
      NamedPerson("john")
    }

    val ageOf: Syntax[Expr] = syntax(number)(
      "the" ~ "age" ~ "of" ~ person map {
        case (_, (_, _, _, p)) =>
          AgeOf(p)
      }
    )

    val brotherOf: Syntax[Person] = syntax(person)(
      person ~ "'s" ~ "brother" map {
        case (_, (p, _, _)) =>
          Brother(p)
      }
    )

    val houseOf: Syntax[House] = syntax(house)(
      "the" ~ "house" ~ "of" ~ person map {
        case (_, (_, _, _, p)) =>
          House(p)
      }
    )

    val ownerOf: Syntax[Person] = syntax(person)(
      "the" ~ "owner" ~ "of" ~ house map {
        case (_, (_, _, _, h)) =>
          Owner(h)
      }
    )

    val cons: Syntax[Expr] = syntax(value)(
      "cons" ~ "(" ~ value ~ "," ~ value ~ ")" map {
        case (_, (_, _, car, _, cdr, _)) =>
          Pair(car, cdr)
      }
    )

    val expr: Axiom[Expr] = axiom(value)
  }

}
