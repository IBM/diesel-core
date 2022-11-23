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

package diesel.samples.jsmodeldsl

import diesel.AstHelpers
import munit.FunSuite

// TODO BMD
// - any magic wrt plurals?
// - start with array
// - optional field type, default to text
// - 'with' syntax for several fields

class BmdDslAstTest extends FunSuite {

  private def doTest(text: String, ast: Any): Unit = {
    AstHelpers.assertAst(BmdDsl, axiom = Some(BmdDsl.aCompileUnit))(text) { tree =>
      AstHelpers.assertNoMarkers(tree, false)
      assert(tree.root.value == ast)
    }
  }

  test("start with") {
    doTest("start with text.", JsModelDecl(Root(StringType), List()))
  }

  test("comment") {
    doTest(
      """
        |-- a comment
        |start with numeric.
        |""".stripMargin,
      JsModelDecl(Root(NumType), List())
    )
  }

  test(
    "many comments"
  ) {
    doTest(
      """
        |-- many comments in various places
        |start with numeric. -- another comment
        |-- final comment""".stripMargin,
      JsModelDecl(Root(NumType), List())
    )
  }

  test("big house") {
    doTest(
      """start with a big house.
        |a big house is a concept.
        |""".stripMargin,
      JsModelDecl(
        Root(CustomType(Offsets(13, 9), "big house")),
        List(ClassDeclaration(Offsets(26, 9), "big house"))
      )
    )

  }

  test("big concept") {
    doTest(
      """start with a big concept.
        |a big concept is a concept.
        |""".stripMargin,
      JsModelDecl(
        Root(CustomType(Offsets(13, 11), "big concept")),
        List(ClassDeclaration(Offsets(28, 11), "big concept"))
      )
    )
  }

  test("MyClass 1") {
    doTest(
      """start with a MyClass.
        |a MyClass is a concept.
        |""".stripMargin,
      JsModelDecl(
        Root(CustomType(Offsets(13, 7), "MyClass")),
        List(ClassDeclaration(Offsets(24, 7), "MyClass"))
      )
    )
  }

  test("MyClass 2") {
    doTest(
      """start with a MyClass.
        |a MyClass is a concept.
        |a MyClass has a foo (text).
        |""".stripMargin,
      JsModelDecl(
        Root(CustomType(Offsets(13, 7), "MyClass")),
        List(ClassDeclaration(
          Offsets(24, 7),
          "MyClass",
          List(AttrDecl(Offsets(62, 3), "foo", StringType))
        ))
      )
    )
  }

  test("MyClass 3") {
    doTest(
      """start with a MyClass.
        |a MyClass is a concept.
        |a MyClass has some foos (text).
        |""".stripMargin,
      JsModelDecl(
        Root(CustomType(Offsets(13, 7), "MyClass")),
        List(ClassDeclaration(
          Offsets(24, 7),
          "MyClass",
          List(AttrDecl(Offsets(65, 4), "foos", ArrayRefType(StringType)))
        ))
      )
    )
  }

  test("MyClass 4") {
    doTest(
      """start with a MyClass.
        |a MyClass is a concept.
        |a MyClass has a foo (text) [optional].
        |""".stripMargin,
      JsModelDecl(
        Root(CustomType(Offsets(13, 7), "MyClass")),
        List(ClassDeclaration(
          Offsets(24, 7),
          "MyClass",
          List(AttrDecl(Offsets(62, 3), "foo", StringType, optional = true))
        ))
      )
    )
  }

  test("Foo") {
    doTest(
      """start with a Foo.
        |a Foo is a concept.
        |a Foo has a bar (a Bar).
        |a Bar is a concept.
        |a Bar has a baz (numeric).
        |""".stripMargin,
      JsModelDecl(
        Root(CustomType(Offsets(13, 3), "Foo")),
        List(
          ClassDeclaration(
            Offsets(20, 3),
            "Foo",
            List(AttrDecl(Offsets(50, 3), "bar", CustomType(Offsets(57, 3), "Bar")))
          ),
          ClassDeclaration(
            Offsets(65, 3),
            "Bar",
            List(AttrDecl(Offsets(95, 3), "baz", NumType))
          )
        )
      )
    )
  }

  test("Foo2") {
    doTest(
      """start with a Foo.
        |a Foo is a concept.
        |a Foo can be gnu.
        |""".stripMargin,
      JsModelDecl(
        Root(CustomType(Offsets(13, 3), "Foo")),
        List(ClassDeclaration(
          Offsets(20, 3),
          "Foo",
          List(AttrDecl(Offsets(51, 3), "gnu", BoolType))
        ))
      )
    )
  }

  test("shopping cart") {
    doTest(
      """start with a shopping cart.
        |a shopping cart is a concept.
        |a shopping cart can be very empty.
        |""".stripMargin,
      JsModelDecl(
        Root(CustomType(Offsets(13, 13), "shopping cart")),
        List(ClassDeclaration(
          Offsets(30, 13),
          "shopping cart",
          List(AttrDecl(Offsets(81, 10), "very empty", BoolType))
        ))
      )
    )
  }

  test("start with Foo") {
    doTest(
      """start with a Foo.
        |a Foo can be one of: bar, gnu, more gnu.
        |""".stripMargin,
      JsModelDecl(
        Root(CustomType(Offsets(13, 3), "Foo")),
        List(DomainDeclaration(Offsets(20, 3), "Foo", List("bar", "gnu", "more gnu")))
      )
    )

  }

  test("start with Foo 2") {
    doTest(
      """start with a Foo.
        |a Foo is a Gnu.
        |a Foo has a foo (numeric).
        |a Gnu is a concept.
        |a Gnu has a gnu (text).
        |""".stripMargin,
      JsModelDecl(
        Root(CustomType(Offsets(13, 3), "Foo")),
        List(
          ClassDeclaration(
            Offsets(20, 3),
            "Foo",
            List(
              AttrDecl(Offsets(46, 3), "foo", NumType, false)
            ),
            Option(CustomType(Offsets(29, 3), "Gnu"))
          ),
          ClassDeclaration(
            Offsets(63, 3),
            "Gnu",
            List(
              AttrDecl(Offsets(93, 3), "gnu", StringType, false)
            )
          )
        )
      )
    )
  }

  test("start with Foo 3") {
    doTest(
      """start with a Foo.
        |a Foo is a Gnu.
        |a Foo has a foo (numeric).
        |a Gnu is a Bar.
        |a Gnu has a gnu (text).
        |a Bar is a concept.
        |a Bar can be open.
        |""".stripMargin,
      JsModelDecl(
        Root(CustomType(Offsets(13, 3), "Foo")),
        List(
          ClassDeclaration(
            Offsets(20, 3),
            "Foo",
            List(
              AttrDecl(Offsets(46, 3), "foo", NumType, false)
            ),
            Option(CustomType(Offsets(29, 3), "Gnu"))
          ),
          ClassDeclaration(
            Offsets(63, 3),
            "Gnu",
            List(
              AttrDecl(Offsets(89, 3), "gnu", StringType, false)
            ),
            Option(CustomType(Offsets(72, 3), "Bar"))
          ),
          ClassDeclaration(
            Offsets(103, 3),
            "Bar",
            List(
              AttrDecl(Offsets(134, 4), "open", BoolType, false)
            )
          )
        )
      )
    )
  }

  test("start with Tree") {
    doTest(
      """start with a Tree.
        |a Tree is a concept.
        |a Tree has a left (a Tree).
        |a Tree has a right (a Tree).
        |a Leaf is a Tree.
        |a Leaf has a value (numeric).
        |""".stripMargin,
      JsModelDecl(
        Root(CustomType(Offsets(13, 4), "Tree")),
        List(
          ClassDeclaration(
            Offsets(21, 4),
            "Tree",
            List(
              AttrDecl(Offsets(53, 4), "left", CustomType(Offsets(61, 4), "Tree"), false),
              AttrDecl(Offsets(81, 5), "right", CustomType(Offsets(90, 4), "Tree"), false)
            )
          ),
          ClassDeclaration(
            Offsets(99, 4),
            "Leaf",
            List(
              AttrDecl(Offsets(128, 5), "value", NumType, false)
            ),
            Option(CustomType(Offsets(109, 4), "Tree"))
          )
        )
      )
    )

  }

}
