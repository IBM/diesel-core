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
import diesel.Dsl._
import diesel.Lexer.{Scanner, Token}

object Sql {

  object Ast {
    sealed trait Node

    case class Name(s: String) extends Node

    trait Clause extends Node

    case class Select(projection: Projection, from: Name, where: Option[Where] = None)
        extends Clause

    case class Delete(from: Name) extends Clause

    sealed case class Where(fieldTest: FieldTest) extends Node

    sealed trait FieldCrit extends Node

    case object IsNull extends FieldCrit

    case object IsNotNull extends FieldCrit

    case class Equals(value: Value) extends FieldCrit

    sealed case class FieldTest(field: Name, crit: FieldCrit)

    trait Projection extends Node

    case object Star extends Projection

    case class Fields(fs: Seq[Name]) extends Projection

    sealed trait Value          extends Node
    case class IntValue(i: Int) extends Value

  }

  import Ast._

  object Sql extends Dsl with Identifiers {

    override def identScanner: Scanner = "[a-z]+".r

    val name: Concept[Name] = concept[Name]

    val idToName: Syntax[Name] = syntax(name)(
      id map { (_, s) => Name(s.text) }
    )

    val projection: Concept[Projection] = concept[Projection]

    val star: Instance[Projection] = instance(projection)("*") map { _ => Star }

    val fields: Syntax[Projection] = syntax(projection)(
      name ~ ("," ~ name).rep(true) map {
        case (_, (n, ns)) =>
          Fields(Seq(n) ++ ns.map(_._2))
      }
    )

    val clause: Concept[Clause] = concept[Clause]

    val where: Concept[Where] = concept[Where]

    val fieldCrit: Concept[FieldCrit] = concept[FieldCrit]

    val nullNotNullCrit: Syntax[FieldCrit] = syntax(fieldCrit)(
      "is" ~ "not".? ~ "null" map {
        case (_, (_, not, _)) =>
          not.map { _: Token => IsNotNull }.getOrElse(IsNull)
      }
    )

    val value: Concept[Value] = concept[Value]

    val intConcept: Concept[IntValue] = concept("\\d+".r, IntValue(0), parent = Some(value)) map {
      case (_, s) =>
        IntValue(s.text.toInt)
    }

    val binOpCrit: Syntax[FieldCrit] = syntax(fieldCrit)(
      "=" ~ value map {
        case (_, (_, v)) =>
          Equals(v)
      }
    )

    val whereS: Syntax[Where] = syntax(where)(
      "where" ~ name ~ fieldCrit map {
        case (_, (_, n, fc)) =>
          Where(FieldTest(n, fc))
      }
    )

    val select: Syntax[Clause] = syntax(clause)(
      "select" ~ projection ~ "from" ~ name ~ where.? map {
        case (_, (_, p: Projection, _, n: Name, w: Option[Where])) =>
          Select(p, n, w)
      }
    )

    val delete: Syntax[Clause] = syntax(clause)(
      "delete" ~ "from" ~ name map {
        case (_, (_, _, n: Name)) =>
          Delete(n)
      }
    )

    val a: Axiom[Clause] = axiom(clause)
  }

}
