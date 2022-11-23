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

class FeelUnaryTestsTest extends FeelFunSuite {

  type Ast = UnaryTests
  override def axiom: Some[Dsl.Axiom[UnaryTests]] = Some(dsl.a_unary_tests)

  test("foo") {
    UTPUT(List(PositiveUnaryTest(ETextual(TEName(Name("foo"))))))
  }

  test("not(foo)") {
    UTPUT(List(PositiveUnaryTest(ETextual(TEName(Name("foo"))))), not = true)
  }

  test("-") {
    UTMinus
  }

  test("not(foo < 2)") {
    UTPUT(
      List(PositiveUnaryTest(ETextual(TEComp(CCompare(
        CLt,
        ETextual(TEName(Name("foo"))),
        ETextual(TELiteral(LSimple(SLNumeric(NumericLiteral(2)))))
      ))))),
      not = true
    )
  }

  test("> 12") {
    UTPUT(
      List(PositiveUnaryTest(ETextual(TESPUT(SPUTEndpoint(
        Gt,
        Endpoint(SVSimpleLiteral(SLNumeric(NumericLiteral(12))))
      ))))),
      false
    )
  }

  test("< 12") {
    UTPUT(
      List(PositiveUnaryTest(ETextual(TESPUT(SPUTEndpoint(
        Lt,
        Endpoint(SVSimpleLiteral(SLNumeric(NumericLiteral(12))))
      ))))),
      false
    )
  }

  test("<= 12") {
    UTPUT(
      List(PositiveUnaryTest(ETextual(TESPUT(SPUTEndpoint(
        Lte,
        Endpoint(SVSimpleLiteral(SLNumeric(NumericLiteral(12))))
      ))))),
      false
    )
  }

  test(">= 12") {
    UTPUT(
      List(PositiveUnaryTest(ETextual(TESPUT(SPUTEndpoint(
        Gte,
        Endpoint(SVSimpleLiteral(SLNumeric(NumericLiteral(12))))
      ))))),
      false
    )
  }

}
