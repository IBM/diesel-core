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

package diesel.facade

import diesel.{Errors, GenericTree, Marker, MarkerMessage}
import diesel.samples.calc.MyDsl
import scala.scalajs.js
import munit.FunSuite

class DieselFacadeTest extends FunSuite {

  def createParseRequest(text: String): ParseRequest = js.Dynamic.literal(
    "text" -> text
  ).asInstanceOf[ParseRequest]

  def createPredictRequest(text: String, offset: Int): PredictRequest = js.Dynamic.literal(
    "text"   -> text,
    "offset" -> offset
  ).asInstanceOf[PredictRequest]

  test("facade should create parse request") {
    val parseRequest = createParseRequest("hello")
    assertEquals(parseRequest.text, "hello")
    assert(parseRequest.axiom.isEmpty)
    parseRequest.axiom = "yalla"
    assertEquals(parseRequest.text, "hello")
    assertEquals(parseRequest.axiom.toOption, Some("yalla"))
  }

  test("facade should create predict request") {
    val predictRequest = createPredictRequest("hello", 2)
    assertEquals(predictRequest.text, "hello")
    assert(predictRequest.axiom.isEmpty)
    assertEquals(predictRequest.offset, 2)
  }

  test("facade should parse calc dsl") {
    val facade = new DieselParserFacade(MyDsl)
    val res    = facade.parse(createParseRequest("1 + pi"))
    assert(res.success)
    assert(res.error.isEmpty)
    assert(res.markers.isEmpty)
    assertEquals(res.styles.size, 3)
    val s0     = res.styles(0)
    assertEquals(s0.offset, 2)
    assertEquals(s0.length, 1)
    assertEquals(s0.name, "keyword")
    val s1     = res.styles(1)
    assertEquals(s1.offset, 0)
    assertEquals(s1.length, 1)
    assertEquals(s1.name, "string")
    val s2     = res.styles(2)
    assertEquals(s2.offset, 4)
    assertEquals(s2.length, 2)
    assertEquals(s2.name, "constant")
  }

  test("facade should predict calc dsl") {
    val facade = new DieselParserFacade(MyDsl)
    val res    = facade.predict(createPredictRequest("1 + ", 4))
    assert(res.success)
    assert(res.error.isEmpty)

    println(res.proposals)

    assertEquals(res.proposals.length, 5)
    val p0 = res.proposals(0)
    assertEquals(p0.text, "0")
    assert(p0.replace.isEmpty)
    val p1 = res.proposals(1)
    assertEquals(p1.text, "pi")
    assert(p1.replace.isEmpty)
  }

  object MyMarkerMessage extends MarkerMessage {
    override def format(locale: String): String = "yalla"
  }

  object MyMarkerPostProcessor extends MarkerPostProcessor {
    override def postProcessMarkers(tree: GenericTree): Seq[Marker] = {
      tree.markers ++ Seq(Marker(Errors.SyntacticError, 0, tree.length, MyMarkerMessage))
    }
  }

  test("facade should support marker post processing") {
    val facade = new DieselParserFacade(MyDsl, markerPostProcessor = Some(MyMarkerPostProcessor))
    val res    = facade.parse(createParseRequest("1+2"))
    assert(res.success)
    assert(res.error.isEmpty)
    assertEquals(res.markers.length, 1)
    val m0     = res.markers(0)
    assertEquals(m0.offset, 0)
    assertEquals(m0.length, 3)
    assertEquals(m0.getMessage("en"), "yalla")
    assertEquals(m0.severity, "error")
  }

}
