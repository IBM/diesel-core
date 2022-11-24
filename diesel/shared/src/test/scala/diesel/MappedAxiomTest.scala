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

import diesel.Dsl.{Axiom, Concept}

case class Num(value: Int)

object MappedAxiomDsl extends Dsl {

  import diesel.MarkerMessage.Implicits.strToMsg

  val number: Concept[Int] = concept("\\d+".r, 0) map { (c, t) =>
    val i = t.text.toInt
    if i > 999 then {
      c.addMarkers(Marker(Errors.SemanticError, c.offset, c.length, "Value is too high"))
    }
    i
  }

  val ax: Axiom[Num] = axiom(number) map { (c, i) =>
    if i == 111 then {
      c.addMarkers(Marker(Errors.SemanticError, c.offset, c.length, "Zorba le grec"))
    }
    Num(i)
  }

}

class MappedAxiomTest extends DslTestFunSuite[Dsl] {

  override def dsl = MappedAxiomDsl

  test("axiom is mapped") {
    withTree("123") { genTree =>
      genTree.root.value match {
        case Num(x) =>
          assert(x == 123)
        case _      =>
          fail(s"axiom has NOT been mapped : ${genTree.root.value}")
      }
    }
  }

  test("parsing error on mapped axiom") {
    withTree("zzz") { genTree =>
      assert(genTree.markers.size == 2)
    }
  }

  test("custom error on concept") {
    withTree("1000") { genTree =>
      assert(genTree.markers.size == 1)
      val marker = genTree.markers.head
      assert(marker.message == SimpleMarkerMessage("Value is too high"))
      assert(marker.offset == 0)
      assert(marker.length == 4)
    }
  }

  test("custom error on mapped axiom") {
    withTree("111") { genTree =>
      assert(genTree.markers.size == 1)
      val marker = genTree.markers.head
      assert(marker.message == SimpleMarkerMessage("Zorba le grec"))
      assert(marker.offset == 0)
      assert(marker.length == 3)
    }
  }

}
