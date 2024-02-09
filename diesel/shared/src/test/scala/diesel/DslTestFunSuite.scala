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
import munit.FunSuite

abstract class DslTestFunSuite extends FunSuite {

  protected def dsl: Dsl
  protected def axiom: Option[Axiom[_]] = None

  type Ast
  def ast(tree: GenericTree): Ast = tree.value.asInstanceOf[Ast]

  protected val withAssertNoAmbiguity: Boolean = true

  protected def assertAst(text: String)(expected: => Ast): Unit = {
    withAst(text) { ast =>
//      println(munitPrint(ast))
      assertEquals(ast, expected)
    }
  }

  protected def assertMarkers(text: String)(expected: => Seq[Marker]): Unit = {
    withTree(text) { tree =>
      assertEquals(tree.markers, expected)
    }
  }

  protected def withAst(text: String)(f: Ast => Unit): Unit = {
    withTree(text) { tree =>
      AstHelpers.assertNoMarkers(tree, withAssertNoAmbiguity)
      f(ast(tree))
    }
  }

  private val defaultNavigatorFactory =
    (r: Result) => Navigator(r, Seq.empty, Navigator.defaultReducer, None);

  protected def withAsts(
    text: String,
    navigatorFactory: Result => Navigator = defaultNavigatorFactory
  )(f: Navigator => Unit): Unit = {
    AstHelpers.assertAsts(dsl, axiom = axiom, navigatorFactory = navigatorFactory)(text) {
      navigator =>
        f(navigator)
    }
  }

  protected def withTree(text: String)(f: GenericTree => Unit): Unit = {
    AstHelpers.selectAst(
      dsl,
      axiom = axiom,
      navigatorFactory = defaultNavigatorFactory
    )(text) { tree =>
      f(tree)
    }
  }

  // IntellliJ hint
  // replace: testAst\(("[^"]*")\) \{([^\}]+)
  // by: test($1) { assertAst($1) { $2 }
  //
  @deprecated("use test(...) { assertAst(...) } style instead, for better IDE experience")
  protected def testAst(text: String)(expected: => Ast)(implicit loc: munit.Location): Unit = {
    val testName = getTestNameFromText(text)
    test(testName) {
      assertAst(text)(expected)
    }
  }

  private def getTestNameFromText(text: String): String = {
    val trimmed         = text.trim
    val lines           = trimmed.split('\n')
    lazy val firstLine  = {
      lines.filterNot(_.isEmpty)(0)
    }
    val oneLineTestName =
      if (lines.length > 1) { firstLine + s"...(${trimmed.length} on ${lines.length} lines)" }
      else { trimmed }
    oneLineTestName
  }

}
