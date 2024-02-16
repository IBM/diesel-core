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
import diesel.Dsl.{Axiom, Concept, Expressions, SPConceptRef, Syntax}

object InheritanceSample {

  object Ast {
    sealed trait Value

    case class IntValue(i: Int) extends Value

    case class TypeRef(identifier: String) extends Value
  }

  import Ast._

  object MyDsl extends Dsl {

    override def acceptExpr[T](
      exprType: Expressions.Type,
      concept: Concept[T],
      multiple: Boolean
    ): Boolean = concept != hiddenValue

    val value: Concept[Value] = concept[Value]

    val hiddenValue: Concept[Value] = concept[Value, Value](value)

    val intValue: Concept[IntValue] =
      concept[IntValue, Value](
        "\\d+".r,
        defaultValue = IntValue(0),
        parent = Some(hiddenValue)
      ) map {
        case (_, s) =>
          IntValue(try { s.text.toInt }
          catch { case _: Exception => 0 })
      }

    val typeRef: Syntax[Value] = syntax(value)(
      SPConceptRef(value, (_, c: Concept[Value]) => TypeRef(c.name))
    )

    val axiom: Axiom[Value] = axiom(value)
  }

}
