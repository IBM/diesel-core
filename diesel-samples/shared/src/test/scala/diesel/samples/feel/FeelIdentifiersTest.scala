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

class FeelIdentifiersTest extends FeelFunSuite {

  override def axiom: Some[Dsl.Axiom[Expression]] = Some(dsl.a_expression)

  type Ast = Expression

  test("foo + 1") {
    assertAst("foo + 1") {
      ETextual(e =
        TEArith(e =
          Addition(
            lhs = ETextual(e =
              TEName(e =
                Name(s =
                  "foo"
                )
              )
            ),
            rhs = ETextual(e =
              TELiteral(e =
                LSimple(l =
                  SLNumeric(l =
                    NumericLiteral(v =
                      1.0
                    )
                  )
                )
              )
            )
          )
        )
      )
    }
  }

  test("{ foo: 1, x: foo + 1 }") {
    assertAst("{ foo: 1, x: foo + 1 }") {
      EBoxed(e =
        BEContext(c =
          Context(entries =
            List(
              ContextEntry(
                key = Left(value =
                  Name(s =
                    "foo"
                  )
                ),
                e = ETextual(e =
                  TELiteral(e =
                    LSimple(l =
                      SLNumeric(l =
                        NumericLiteral(v =
                          1.0
                        )
                      )
                    )
                  )
                )
              ),
              ContextEntry(
                key = Left(value =
                  Name(s =
                    "x"
                  )
                ),
                e = ETextual(e =
                  TEArith(e =
                    Addition(
                      lhs = ETextual(e =
                        TEName(e =
                          Name(s =
                            "foo"
                          )
                        )
                      ),
                      rhs = ETextual(e =
                        TELiteral(e =
                          LSimple(l =
                            SLNumeric(l =
                              NumericLiteral(v =
                                1.0
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )

    }
  }

  test("{ foo + bar: 1 }") {
    assertAst("{ foo + bar: 1 }") {
      EBoxed(e =
        BEContext(c =
          Context(entries =
            List(
              ContextEntry(
                key = Left(value =
                  Name(s =
                    "foo + bar"
                  )
                ),
                e = ETextual(e =
                  TELiteral(e =
                    LSimple(l =
                      SLNumeric(l =
                        NumericLiteral(v =
                          1.0
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    }
  }

  test("{ foo: 1, bar: 1, foo + bar: 2, x: foo + bar }") {
    assertAst("{ foo: 1, bar: 1, foo + bar: 2, x: foo + bar }") {
      EBoxed(e =
        BEContext(c =
          Context(entries =
            List(
              ContextEntry(
                key = Left(value =
                  Name(s =
                    "foo"
                  )
                ),
                e = ETextual(e =
                  TELiteral(e =
                    LSimple(l =
                      SLNumeric(l =
                        NumericLiteral(v =
                          1.0
                        )
                      )
                    )
                  )
                )
              ),
              ContextEntry(
                key = Left(value =
                  Name(s =
                    "bar"
                  )
                ),
                e = ETextual(e =
                  TELiteral(e =
                    LSimple(l =
                      SLNumeric(l =
                        NumericLiteral(v =
                          1.0
                        )
                      )
                    )
                  )
                )
              ),
              ContextEntry(
                key = Left(value =
                  Name(s =
                    "foo + bar"
                  )
                ),
                e = ETextual(e =
                  TELiteral(e =
                    LSimple(l =
                      SLNumeric(l =
                        NumericLiteral(v =
                          2.0
                        )
                      )
                    )
                  )
                )
              ),
              ContextEntry(
                key = Left(value =
                  Name(s =
                    "x"
                  )
                ),
                e = ETextual(e =
                  TEName(e =
                    Name(s =
                      "foo + bar"
                    )
                  )
                )
              )
            )
          )
        )
      )
    }
  }

}
