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
import diesel.Dsl.{Axiom, Syntax}

object UnrelatedTypes {

  sealed trait Root
  case class RFoo(foo: Foo) extends Root
  case class RBar(bar: Bar) extends Root
  case class Foo()
  case class Bar()

  object UnrelatedTypesDsl extends Dsl {

    val foo: Dsl.Concept[Foo] = concept("foo".r, Foo()) map {
      case (_, _) =>
        Foo()
    }

    val bar: Dsl.Concept[Bar] = concept("bar".r, Bar()) map {
      case (_, _) =>
        Bar()
    }

    val s: Syntax[Root] = syntax(
      (foo | bar) map {
        case (_, Left(f))  =>
          RFoo(f)
        case (_, Right(r)) =>
          RBar(r)
      }
    )

    val root: Axiom[Root] = axiom(s)

  }

}
