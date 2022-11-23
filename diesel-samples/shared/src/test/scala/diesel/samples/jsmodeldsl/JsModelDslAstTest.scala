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

import diesel.DslTestFunSuite

class JsModelDslAstTest extends DslTestFunSuite {

  override def dsl: JsModelDsl.type = JsModelDsl
  type Ast = JsModelDecl

  test("root") {
    assertAst("root: number") {
      JsModelDecl(Root(NumType), List())
    }
  }

  test("comment") {
    assertAst(
      """// yallaaaaa
        |root: number
        |""".stripMargin
    ) {
      JsModelDecl(Root(NumType), List())
    }
  }

  test("class") {
    assertAst(
      """root: MyClass
        |class MyClass { }
        |""".stripMargin
    ) {
      JsModelDecl(
        Root(CustomType(Offsets(6, 7), "MyClass")),
        List(ClassDeclaration(Offsets(20, 7), "MyClass"))
      )
    }
  }

  test("?".ignore) {
    assertAst(
      """
        |root: class
        |class class { }
        |""".stripMargin
    ) {
      JsModelDecl(
        Root(CustomType(Offsets(6, 5), "class")),
        List(ClassDeclaration(Offsets(18, 5), "class"))
      )
    }
  }

  test("attribute") {
    assertAst(
      """root: MyClass
        |class MyClass {
        |  foo: string
        |}
        |""".stripMargin
    ) {
      JsModelDecl(
        Root(CustomType(Offsets(6, 7), "MyClass")),
        List(ClassDeclaration(
          Offsets(20, 7),
          "MyClass",
          List(AttrDecl(Offsets(32, 3), "foo", StringType))
        ))
      )
    }
  }

  test("root array") {
    assertAst("root: number[]") {
      JsModelDecl(Root(ArrayRefType(NumType)), List())
    }
  }

  test("attribute array") {
    assertAst(
      """root: MyClass
        |class MyClass {
        |  foos: string[]
        |}
        |""".stripMargin
    ) {
      JsModelDecl(
        Root(CustomType(Offsets(6, 7), "MyClass")),
        List(ClassDeclaration(
          Offsets(20, 7),
          "MyClass",
          List(AttrDecl(Offsets(32, 4), "foos", ArrayRefType(StringType)))
        ))
      )
    }
  }

  test("optional attribute") {
    assertAst(
      """root: MyClass
        |class MyClass {
        |  foo?: string
        |}
        |""".stripMargin
    ) {
      JsModelDecl(
        Root(CustomType(Offsets(6, 7), "MyClass")),
        List(ClassDeclaration(
          Offsets(20, 7),
          "MyClass",
          List(AttrDecl(Offsets(32, 3), "foo", StringType, optional = true))
        ))
      )
    }
  }

  test("two classes") {
    assertAst(
      """root: Foo
        |class Foo {
        |  bar: Bar
        |}
        |class Bar {
        |  baz: number
        |}
        |""".stripMargin
    ) {
      JsModelDecl(
        Root(CustomType(Offsets(6, 3), "Foo")),
        List(
          ClassDeclaration(
            Offsets(16, 3),
            "Foo",
            List(AttrDecl(Offsets(24, 3), "bar", CustomType(Offsets(29, 3), "Bar")))
          ),
          ClassDeclaration(Offsets(41, 3), "Bar", List(AttrDecl(Offsets(49, 3), "baz", NumType)))
        )
      )
    }
  }

  test("domain") {
    assertAst(
      """root: Rating
        |domain Rating [
        |  "bad", "medium", "good"
        |]""".stripMargin
    ) {
      JsModelDecl(
        Root(CustomType(Offsets(6, 6), "Rating")),
        List(DomainDeclaration(Offsets(20, 6), "Rating", List("bad", "medium", "good")))
      )
    }
  }

  test("domain 2") {
    assertAst(
      """root: Customer
        |class Customer {
        |  name: string
        |  rating?: Rating
        |}
        |domain Rating [
        |  "bad", "medium", "good"
        |]""".stripMargin
    ) {
      JsModelDecl(
        Root(CustomType(Offsets(6, 8), "Customer")),
        List(
          ClassDeclaration(
            Offsets(21, 8),
            "Customer",
            List(
              AttrDecl(Offsets(34, 4), "name", StringType),
              AttrDecl(
                Offsets(49, 6),
                "rating",
                CustomType(Offsets(58, 6), "Rating"),
                optional = true
              )
            )
          ),
          DomainDeclaration(Offsets(74, 6), "Rating", List("bad", "medium", "good"))
        )
      )
    }
  }

  test("extends") {
    assertAst(
      """root: Super
        |class Super {
        |  sup: string
        |}
        |class Sub extends Super {
        |  sub: number
        |}""".stripMargin
    ) {
      JsModelDecl(
        Root(CustomType(Offsets(6, 5), "Super")),
        List(
          ClassDeclaration(
            Offsets(18, 5),
            "Super",
            List(AttrDecl(Offsets(28, 3), "sup", StringType)),
            None
          ),
          ClassDeclaration(
            Offsets(48, 3),
            "Sub",
            List(AttrDecl(Offsets(70, 3), "sub", NumType)),
            Some(CustomType(Offsets(60, 5), "Super"))
          )
        )
      )
    }
  }

  test("discrimination") {
    assertAst(
      """root: Super
        |class Super
        |  discriminator field dType
        |{
        |  sup: string
        |}
        |class Sub1
        |  extends Super
        |  discriminator value "sub1"
        |{
        |  foo: number
        |}
        |class Sub2
        |  extends Super
        |  discriminator value "sub2"
        |{
        |  bar: number
        |}""".stripMargin
    ) {
      JsModelDecl(
        Root(CustomType(Offsets(6, 5), "Super")),
        List(
          ClassDeclaration(
            Offsets(18, 5),
            "Super",
            List(AttrDecl(Offsets(56, 3), "sup", StringType)),
            None,
            Some(DFieldName(Offsets(46, 5), "dType"))
          ),
          ClassDeclaration(
            Offsets(76, 4),
            "Sub1",
            List(AttrDecl(Offsets(130, 3), "foo", NumType)),
            Some(CustomType(Offsets(91, 5), "Super")),
            Some(DFieldValue(Offsets(119, 6), "sub1"))
          ),
          ClassDeclaration(
            Offsets(150, 4),
            "Sub2",
            List(AttrDecl(Offsets(204, 3), "bar", NumType)),
            Some(CustomType(Offsets(165, 5), "Super")),
            Some(DFieldValue(Offsets(193, 6), "sub2"))
          )
        )
      )
    }
  }

}
