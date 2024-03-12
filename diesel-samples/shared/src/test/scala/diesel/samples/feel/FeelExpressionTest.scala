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

package diesel.samples.feel

import diesel.samples.feel.Ast._
import diesel.Dsl

class FeelExpressionTest extends FeelFunSuite {

  override def axiom: Some[Dsl.Axiom[Expression]] = Some(Feel.a_expression)

  type Ast = Expression

  test("foo") {
    assertAst("foo") {
      ETextual(TEName(Name("foo")))
    }
  }

  test("123") {
    assertAst("123") {
      ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(123)))))
    }
  }

  test("true") {
    assertAst("true") {
      ETextual(TELiteral(LSimple(SLBool(BooleanLiteral(true)))))
    }
  }

  test("false") {
    assertAst("false") {
      ETextual(TELiteral(LSimple(SLBool(BooleanLiteral(false)))))
    }
  }

  test("\"yalla\"") {
    assertAst("\"yalla\"") {
      ETextual(TELiteral(LSimple(SLString(StringLiteral("yalla")))))
    }
  }

  test("@\"yalla\"") {
    assertAst("@\"yalla\"") {
      ETextual(TELiteral(LSimple(SLDateTime(DTAt(StringLiteral("yalla"))))))
    }
  }

  test("{ foo: 1 }") {
    assertAst("{ foo: 1 }") {
      EBoxed(BEContext(Context(List(ContextEntry(
        Left(Name("foo")),
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1)))))
      )))))
    }
  }

  test("""{ "foo": 1 }""") {
    assertAst("""{ "foo": 1 }""") {
      EBoxed(BEContext(Context(List(ContextEntry(
        Right(StringLiteral("foo")),
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1)))))
      )))))
    }
  }

  test("""{ foo : { "bar" : 2 }}""") {
    assertAst("""{ foo : { "bar" : 2 }}""") {
      EBoxed(BEContext(Context(List(ContextEntry(
        Left(Name("foo")),
        EBoxed(BEContext(Context(List(ContextEntry(
          Right(StringLiteral("bar")),
          ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(2)))))
        )))))
      )))))
    }
  }

  test("{ x: 1, y: x+2 }") {
    assertAst("{ x: 1, y: x+2 }") {
      EBoxed(BEContext(Context(List(
        ContextEntry(Left(Name("x")), ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1)))))),
        ContextEntry(
          Left(Name("y")),
          ETextual(TEArith(Addition(
            ETextual(TEName(Name("x"))),
            ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(2)))))
          )))
        )
      ))))
    }
  }

  test("function() 1") {
    assertAst("function() 1") {
      EBoxed(BEFunDef(FunctionDefinition(
        List(),
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1)))))
      )))
    }
  }

  test("function(a: foo) 1") {
    assertAst("function(a: foo) 1") {
      EBoxed(BEFunDef(FunctionDefinition(
        List(FormalParameter(Name("a"), Some(TQualified(QualifiedName(List(Name("foo"))))))),
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1.0)))))
      )))
    }
  }

  test("function(a: foo, b) 1") {
    assertAst("function(a: foo, b) 1") {
      EBoxed(BEFunDef(FunctionDefinition(
        List(
          FormalParameter(Name("a"), Some(TQualified(QualifiedName(List(Name("foo")))))),
          FormalParameter(Name("b"), None)
        ),
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1)))))
      )))
    }
  }

  test("function(a: foo, b) external 1") {
    assertAst("function(a: foo, b) external 1") {
      EBoxed(BEFunDef(FunctionDefinition(
        List(
          FormalParameter(Name("a"), Some(TQualified(QualifiedName(List(Name("foo")))))),
          FormalParameter(Name("b"), None)
        ),
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1))))),
        external = true
      )))
    }
  }

  test("[]") {
    assertAst("[]") {
      EBoxed(BEList(List()))
    }
  }

  test("[1]") {
    assertAst("[1]") {
      EBoxed(BEList(List(ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1))))))))
    }
  }

  test("""[1, "yalla"]""") {
    assertAst("""[1, "yalla"]""") {
      EBoxed(BEList(List(
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1))))),
        ETextual(TELiteral(LSimple(SLString(StringLiteral("yalla")))))
      )))
    }
  }

  test("foo instance of bar") {
    assertAst("foo instance of bar") {
      ETextual(TEInstanceOf(InstanceOf(
        ETextual(TEName(Name("foo"))),
        TQualified(QualifiedName(List(Name("bar"))))
      )))
    }
  }

  test("foo instance of list<bar>") {
    assertAst("foo instance of list<bar>") {
      ETextual(TEInstanceOf(InstanceOf(
        ETextual(TEName(Name("foo"))),
        TList(TQualified(QualifiedName(List(Name("bar")))))
      )))
    }
  }

  test("foo instance of list<list<bar>>") {
    assertAst("foo instance of list<list<bar>>") {
      ETextual(TEInstanceOf(InstanceOf(
        ETextual(TEName(Name("foo"))),
        TList(TList(TQualified(QualifiedName(List(Name("bar"))))))
      )))
    }
  }

  test("foo instance of context<x:y>") {
    assertAst("foo instance of context<x:y>") {
      ETextual(TEInstanceOf(InstanceOf(
        ETextual(TEName(Name("foo"))),
        TContext(List((Name("x"), TQualified(QualifiedName(List(Name("y")))))))
      )))
    }
  }

  test("foo instance of context<x:list<y>,z:w>") {
    assertAst("foo instance of context<x:list<y>,z:w>") {
      ETextual(TEInstanceOf(InstanceOf(
        ETextual(TEName(Name("foo"))),
        TContext(List(
          (Name("x"), TList(TQualified(QualifiedName(List(Name("y")))))),
          (Name("z"), TQualified(QualifiedName(List(Name("w")))))
        ))
      )))
    }
  }

  test("foo instance of function<> -> bar") {
    assertAst("foo instance of function<> -> bar") {
      ETextual(TEInstanceOf(InstanceOf(
        ETextual(TEName(Name("foo"))),
        TFunction(List(), TQualified(QualifiedName(List(Name("bar")))))
      )))
    }
  }

  test("foo instance of function<bar> -> baz") {
    assertAst("foo instance of function<bar> -> baz") {
      ETextual(TEInstanceOf(InstanceOf(
        ETextual(TEName(Name("foo"))),
        TFunction(
          List(TQualified(QualifiedName(List(Name("bar"))))),
          TQualified(QualifiedName(List(Name("baz"))))
        )
      )))
    }
  }

  test("foo instance of function<bar,baz> -> yalla") {
    assertAst("foo instance of function<bar,baz> -> yalla") {
      ETextual(TEInstanceOf(InstanceOf(
        ETextual(TEName(Name("foo"))),
        TFunction(
          List(
            TQualified(QualifiedName(List(Name("bar")))),
            TQualified(QualifiedName(List(Name("baz"))))
          ),
          TQualified(QualifiedName(List(Name("yalla"))))
        )
      )))
    }
  }

  test("foo[1]") {
    assertAst("foo[1]") {
      ETextual(TEFilter(FilterExpression(
        ETextual(TEName(Name("foo"))),
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1)))))
      )))
    }
  }

  test("foo[bar[1]]") {
    assertAst("foo[bar[1]]") {
      ETextual(TEFilter(FilterExpression(
        ETextual(TEName(Name("foo"))),
        ETextual(TEFilter(FilterExpression(
          ETextual(TEName(Name("bar"))),
          ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1)))))
        )))
      )))
    }
  }

  test("1 < 2") {
    assertAst("1 < 2") {
      ETextual(TEComp(CCompare(
        CLt,
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1))))),
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(2)))))
      )))
    }
  }

  test("1 <= 2") {
    assertAst("1 <= 2") {
      ETextual(TEComp(CCompare(
        CLte,
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1))))),
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(2)))))
      )))
    }
  }

  test("1 > 2") {
    assertAst("1 > 2") {
      ETextual(TEComp(CCompare(
        CGt,
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1))))),
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(2)))))
      )))
    }
  }

  test("1 >= 2") {
    assertAst("1 >= 2") {
      ETextual(TEComp(CCompare(
        CGte,
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1))))),
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(2)))))
      )))
    }
  }

  test("1 != 2") {
    assertAst("1 != 2") {
      ETextual(TEComp(CCompare(
        CNeq,
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1))))),
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(2)))))
      )))
    }
  }

  test("1 = 2") {
    assertAst("1 = 2") {
      ETextual(TEComp(CCompare(
        CEq,
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1))))),
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(2)))))
      )))
    }
  }

  test("foo between 1 and bar") {
    assertAst("foo between 1 and bar") {
      ETextual(TEComp(CBetween(
        ETextual(TEName(Name("foo"))),
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1))))),
        ETextual(TEName(Name("bar")))
      )))
    }
  }

  test("foo in bar") {
    assertAst("foo in bar") {
      ETextual(TEComp(CIn(
        ETextual(TEName(Name("foo"))),
        List(PositiveUnaryTest(ETextual(TEName(Name("bar")))))
      )))
    }
  }

  test("foo in (bar, baz)") {
    assertAst("foo in (bar, baz)") {
      ETextual(TEComp(CIn(
        ETextual(TEName(Name("foo"))),
        List(
          PositiveUnaryTest(ETextual(TEName(Name("bar")))),
          PositiveUnaryTest(ETextual(TEName(Name("baz"))))
        )
      )))
    }
  }

  test("foo and bar") {
    assertAst("foo and bar") {
      ETextual(TEConj(Conjunction(ETextual(TEName(Name("foo"))), ETextual(TEName(Name("bar"))))))
    }
  }

  test("foo and bar and baz") {
    assertAst("foo and bar and baz") {
      ETextual(TEConj(Conjunction(
        ETextual(TEConj(Conjunction(
          ETextual(TEName(Name("foo"))),
          ETextual(TEName(Name("bar")))
        ))),
        ETextual(TEName(Name("baz")))
      )))
    }
  }

  test("foo or bar") {
    assertAst("foo or bar") {
      ETextual(TEDisj(Disjunction(ETextual(TEName(Name("foo"))), ETextual(TEName(Name("bar"))))))
    }
  }

  test("foo or bar and baz") {
    assertAst("foo or bar and baz") {
      ETextual(TEDisj(Disjunction(
        ETextual(TEName(Name("foo"))),
        ETextual(TEConj(Conjunction(
          ETextual(TEName(Name("bar"))),
          ETextual(TEName(Name("baz")))
        )))
      )))
    }
  }

  test("foo and bar or baz") {
    assertAst("foo and bar or baz") {
      ETextual(TEDisj(Disjunction(
        ETextual(TEConj(Conjunction(
          ETextual(TEName(Name("foo"))),
          ETextual(TEName(Name("bar")))
        ))),
        ETextual(TEName(Name("baz")))
      )))
    }
  }

  test("some x in bar satisfies x") {
    assertAst("some x in bar satisfies x") {
      ETextual(TEQuant(QuantifiedExpression(
        true,
        List((Name("x"), ETextual(TEName(Name("bar"))))),
        ETextual(TEName(Name("x")))
      )))
    }
  }

  test("every x in bar, z in x satisfies z > 10") {
    assertAst("every x in bar, z in x satisfies z > 10") {
      ETextual(TEQuant(QuantifiedExpression(
        false,
        List((Name("x"), ETextual(TEName(Name("bar")))), (Name("z"), ETextual(TEName(Name("x"))))),
        ETextual(TEComp(CCompare(
          CGt,
          ETextual(TEName(Name("z"))),
          ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(10.0)))))
        )))
      )))
    }
  }

  test("if true then 1 else 2") {
    assertAst("if true then 1 else 2") {
      ETextual(TEIf(IfExpression(
        ETextual(TELiteral(LSimple(SLBool(BooleanLiteral(true))))),
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1))))),
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(2)))))
      )))
    }
  }

  test("if true then if false then 1 else 2 else 3") {
    assertAst("if true then if false then 1 else 2 else 3") {
      ETextual(TEIf(IfExpression(
        ETextual(TELiteral(LSimple(SLBool(BooleanLiteral(true))))),
        ETextual(TEIf(IfExpression(
          ETextual(TELiteral(LSimple(SLBool(BooleanLiteral(false))))),
          ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1))))),
          ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(2)))))
        ))),
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(3)))))
      )))
    }
  }

  test("for foo in bar return baz") {
    assertAst("for foo in bar return baz") {
      ETextual(TEFor(ForExpression(
        List((Name("foo"), IterationContext(ETextual(TEName(Name("bar"))), None))),
        ETextual(TEName(Name("baz")))
      )))
    }
  }

  test("for foo in bar, baz in foo return yalla") {
    assertAst("for foo in bar, baz in foo return yalla") {
      ETextual(TEFor(ForExpression(
        List(
          (Name("foo"), IterationContext(ETextual(TEName(Name("bar"))), None)),
          (Name("baz"), IterationContext(ETextual(TEName(Name("foo"))), None))
        ),
        ETextual(TEName(Name("yalla")))
      )))
    }
  }

  test("for foo in bar return (foo.yalla)") {
    assertAst("for foo in bar return (foo.yalla)") {
      ETextual(TEFor(ForExpression(
        List((Name("foo"), IterationContext(ETextual(TEName(Name("bar"))), None))),
        ETextual(TEParens(ETextual(TEPath(PathExpression(
          ETextual(TEName(Name("foo"))),
          Name("yalla")
        )))))
      )))
    }
  }

  test("for x in 1..10 return (x + 1)") {
    assertAst("for x in 1..10 return (x + 1)") {
      ETextual(TEFor(ForExpression(
        List((
          Name("x"),
          IterationContext(
            ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1))))),
            Some(ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(10))))))
          )
        )),
        ETextual(TEParens(ETextual(TEArith(Addition(
          ETextual(TEName(Name("x"))),
          ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1)))))
        )))))
      )))
    }
  }

  test("foo.bar") {
    assertAst("foo.bar") {
      ETextual(TEPath(PathExpression(ETextual(TEName(Name("foo"))), Name("bar"))))
    }
  }

  test("foo.bar.baz") {
    assertAst("foo.bar.baz") {
      ETextual(TEPath(PathExpression(
        ETextual(TEPath(PathExpression(ETextual(TEName(Name("foo"))), Name("bar")))),
        Name("baz")
      )))
    }
  }

  test("foo(bar)") {
    assertAst("foo(bar)") {
      ETextual(TEFuncInv(FunctionInvocation(
        ETextual(TEName(Name("foo"))),
        PPositional(List(ETextual(TEName(Name("bar")))))
      )))
    }
  }

  test("foo(bar).baz") {
    assertAst("foo(bar).baz") {
      ETextual(TEPath(PathExpression(
        ETextual(TEFuncInv(FunctionInvocation(
          ETextual(TEName(Name("foo"))),
          PPositional(List(ETextual(TEName(Name("bar")))))
        ))),
        Name("baz")
      )))
    }
  }

  test("foo(bar, baz)") {
    assertAst("foo(bar, baz)") {
      ETextual(TEFuncInv(FunctionInvocation(
        ETextual(TEName(Name("foo"))),
        PPositional(List(ETextual(TEName(Name("bar"))), ETextual(TEName(Name("baz")))))
      )))
    }
  }

  test("foo(bar:baz)") {
    assertAst("foo(bar:baz)") {
      ETextual(TEFuncInv(FunctionInvocation(
        ETextual(TEName(Name("foo"))),
        PNamed(List((Name("bar"), ETextual(TEName(Name("baz"))))))
      )))
    }
  }

  test("foo(bar:baz, blah:1)") {
    assertAst("foo(bar:baz, blah:1)") {
      ETextual(TEFuncInv(FunctionInvocation(
        ETextual(TEName(Name("foo"))),
        PNamed(List(
          (Name("bar"), ETextual(TEName(Name("baz")))),
          (Name("blah"), ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1))))))
        ))
      )))
    }
  }

  test("1 + 2") {
    assertAst("1 + 2") {
      ETextual(TEArith(Addition(
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1))))),
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(2)))))
      )))
    }
  }

  test("1 - 2") {
    assertAst("1 - 2") {
      ETextual(TEArith(Subtraction(
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1))))),
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(2)))))
      )))
    }
  }

  test("1 / 2") {
    assertAst("1 / 2") {
      ETextual(TEArith(Division(
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1))))),
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(2)))))
      )))
    }
  }

  test("1 * 2") {
    assertAst("1 * 2") {
      ETextual(TEArith(Multiplication(
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1))))),
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(2)))))
      )))
    }
  }

  test("1 + 2 + 3") {
    assertAst("1 + 2 + 3") {
      ETextual(TEArith(Addition(
        ETextual(TEArith(Addition(
          ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1))))),
          ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(2)))))
        ))),
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(3)))))
      )))
    }
  }

  test("1 * 2 + 3") {
    assertAst("1 * 2 + 3") {
      ETextual(TEArith(Addition(
        ETextual(TEArith(Multiplication(
          ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1))))),
          ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(2)))))
        ))),
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(3)))))
      )))
    }
  }

  test("1 + 2 * 3") {
    assertAst("1 + 2 * 3") {
      ETextual(TEArith(Addition(
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1))))),
        ETextual(TEArith(Multiplication(
          ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(2))))),
          ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(3)))))
        )))
      )))
    }
  }

  test("1 + 2 and 3") {
    assertAst("1 + 2 and 3") {
      ETextual(TEArith(Addition(
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1))))),
        ETextual(TEConj(Conjunction(
          ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(2))))),
          ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(3)))))
        )))
      )))
    }
  }

  test("-4") {
    assertAst("-4") {
      ETextual(
        TEArith(ArithmeticNegation(ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(4)))))))
      )
    }
  }

  test("[0..1]") {
    assertAst("[0..1]") {
      ETextual(TESPUT(SPUTInterval(Interval(
        "[",
        Endpoint(SVSimpleLiteral(SLNumeric(NumericLiteral(0)))),
        Endpoint(SVSimpleLiteral(SLNumeric(NumericLiteral(1)))),
        "]"
      ))))
    }
  }

  test("< 12") {
    assertAst("< 12") {
      ETextual(TESPUT(SPUTEndpoint(Lt, Endpoint(SVSimpleLiteral(SLNumeric(NumericLiteral(12)))))))
    }
  }

  test("(1)") {
    assertAst("(1)") {
      ETextual(TEParens(ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1)))))))
    }
  }

  test("(1 + bar) + 2") {
    assertAst("(1 + bar) + 2") {
      ETextual(TEArith(Addition(
        ETextual(TEParens(ETextual(TEArith(Addition(
          ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1))))),
          ETextual(TEName(Name("bar")))
        ))))),
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(2)))))
      )))
    }
  }

  test("function(foo) true") {
    assertAst("function(foo) true") {
      EBoxed(BEFunDef(FunctionDefinition(
        List(FormalParameter(Name("foo"), None)),
        ETextual(TELiteral(LSimple(SLBool(BooleanLiteral(true)))))
      )))
    }
  }

  test("function(foo) 1") {
    assertAst("function(foo) 1") {
      EBoxed(BEFunDef(FunctionDefinition(
        List(FormalParameter(Name("foo"), None)),
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1)))))
      )))
    }
  }

  test("function(foo) (foo + 1)") {
    assertAst("function(foo) (foo + 1)") {
      EBoxed(BEFunDef(FunctionDefinition(
        List(FormalParameter(Name("foo"), None)),
        ETextual(TEParens(ETextual(TEArith(Addition(
          ETextual(TEName(Name("foo"))),
          ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(1)))))
        )))))
      )))
    }
  }

  test("sum(customers[name > 2].income)") {
    assertAst("sum(customers[name > 2].income)") {
      ETextual(TEFuncInv(FunctionInvocation(
        ETextual(TEName(Name("sum"))),
        PPositional(List(ETextual(TEPath(PathExpression(
          ETextual(TEFilter(FilterExpression(
            ETextual(TEName(Name("customers"))),
            ETextual(TEComp(CCompare(
              CGt,
              ETextual(TEName(Name("name"))),
              ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(2)))))
            )))
          ))),
          Name("income")
        )))))
      )))
    }
  }

}
