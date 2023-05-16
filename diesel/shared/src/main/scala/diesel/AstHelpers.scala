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

import diesel.Dsl.Axiom

object AstHelpers {

  def parse(
    dsl: Dsl,
    text: String,
    axiom: Option[Axiom[_]] = None
  ): Result = {
    val bnf: Bnf       = Bnf(dsl)
    val parser: Earley = Earley(bnf, dsl.dynamicLexer)
    val a              = getBnfAxiomOrThrow(bnf, axiom)
    parser.parse(new Lexer.Input(text), a)
  }

  private def getBnfAxiomOrThrow(bnf: Bnf, axiom: Option[Axiom[_]]): Bnf.Axiom = {
    axiom match {
      case Some(x) =>
        bnf.axioms
          .find(ba => ba.name == s"${x.name}[_,_,_,_,_].axiom")
          .getOrElse(throw new IllegalArgumentException(s"missing axiom '${x.name}'"))
      case None    =>
        bnf.axioms
          .headOption
          .getOrElse(throw new IllegalArgumentException("no axiom"))
    }
  }

  def predict(
    dsl: Dsl,
    text: String,
    offset: Int,
    config: Option[CompletionConfiguration] = None,
    userDataProvider: Option[UserDataProvider] = None,
    axiom: Option[Axiom[_]] = None
  ): Seq[CompletionProposal] = {
    val bnf: Bnf       = Bnf(dsl)
    val parser: Earley = Earley(bnf, dsl.dynamicLexer)
    val a              = getBnfAxiomOrThrow(bnf, axiom)
    val res            = new CompletionProcessor(
      parser.parse(new Lexer.Input(text), a),
      text,
      config,
      userDataProvider
    ).computeCompletionProposal(offset)
//    println(s"predict '$text' at offset $offset : $res")
    res
  }

  def assertAsts(
    dsl: Dsl,
    axiom: Option[Axiom[_]] = None,
    navigatorFactory: Result => Navigator = Navigator(_)
  )(s: String)(f: Navigator => Unit): Unit = {
    val result    = parse(dsl, s, axiom)
    assert(result.success)
    val navigator = navigatorFactory(result)
    f(navigator)
  }

  def assertAst(
    dsl: Dsl,
    axiom: Option[Axiom[_]] = None,
    navigatorFactory: Result => Navigator = Navigator(_)
  )(s: String)(f: GenericTree => Unit): Unit = {
    assertAsts(dsl, axiom, navigatorFactory)(s) { n: Navigator =>
      assert(n.hasNext)
      val a = n.next()
      // println(a.root)
      assert(!n.hasNext, s"more than 1 ast found : \n${a.root.value}\n${n.next().root.value}")
      f(a)
    }
  }

  def withAst[T](
    dsl: Dsl,
    axiom: Option[Axiom[_]] = None,
    navigatorFactory: Result => Navigator = Navigator(_)
  )(s: String)(f: GenericTree => T): T = {
    val result = parse(dsl, s, axiom)
    assert(result.success)
    val n      = navigatorFactory(result)
    assert(n.hasNext)
    f(n.next())
  }

  def selectAst(
    dsl: Dsl,
    axiom: Option[Axiom[_]] = None,
    navigatorFactory: Result => Navigator = Navigator(_)
  )(s: String)(f: GenericTree => Unit): Unit = {
    val result    = parse(dsl, s, axiom)
    assert(result.success)
    val astOption = Navigator.select(navigatorFactory(result))
    assert(astOption.nonEmpty)
    f(astOption.get)
  }

  def assertNoMarkers(p: GenericTree, assertNoAmbiguity: Boolean = true): Unit = {
    assert(p.markers.isEmpty, s"markers found ${p.markers.mkString("\n")}")
    if (assertNoAmbiguity)
      assert(!p.toSeq.exists(n => n.hasAmbiguity))
  }

}
