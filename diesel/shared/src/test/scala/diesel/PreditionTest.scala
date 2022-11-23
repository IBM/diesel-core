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

import diesel.AstHelpers.predict
import diesel.Bnf.{DslElement, DslValue}
import munit.FunSuite

class PredictTest extends FunSuite {

  implicit object MyPredictDsl extends Dsl {

    val foo: Dsl.Concept[String] = concept(
      "foo\\d*".r,
      "foo13"
    ).map { (_, t) => t.text }

    val root: Dsl.Axiom[String] = axiom(foo).build
  }

  implicit val element: DslValue[String] = DslValue(MyPredictDsl.foo)

  val nodeWithStringValue: GenericNode => Boolean = { n: GenericNode =>
    n.value.isInstanceOf[String]
  }

  private def assertPredictionsAt(offset: Int) = {
    assertPredictions(
      "foo",
      offset,
      nodeWithStringValue,
      { n => Seq(n.value.toString + "1313") },
      Seq("foo1313")
    )
  }

  test("predict at 0") {
    assertPredictionsAt(0)
  }

  test("predict at 1") {
    assertPredictionsAt(1)
  }

  test("predict at 2") {
    assertPredictionsAt(2)
  }

  def completionProvider(
    matcher: GenericNode => Boolean,
    proposer: GenericNode => Seq[String]
  ): CompletionProvider =
    (element: Option[Bnf.DslElement], tree: GenericTree, offset: Int, node: Option[GenericNode]) =>
      {
        tree.toIterable
          .filter(matcher)
          .flatMap(proposer)
          .map(target => CompletionProposal(element, target))
          .toSeq
      }

  private def assertPredictions(
    text: String,
    offset: Int,
    matcher: GenericNode => Boolean,
    proposer: GenericNode => Seq[String],
    expected: Seq[String]
  )(implicit dsl: Dsl, element: DslElement): Unit = {
    val config = new CompletionConfiguration()
    config.setProvider(element, completionProvider(matcher, proposer))

    assert(
      predict(dsl, text, offset, Some(config)).map(_.text)
        == expected
    )
  }

}
