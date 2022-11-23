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

package diesel.samples.glsl

import diesel.DslTestFunSuite
import diesel.samples.glsl.Ast._

class Glsl2Test extends DslTestFunSuite {

  type Ast = TranslationUnit
  override def dsl: Glsl.type = Glsl

  test("float x = 12.3;") {
    assertAst("float x = 12.3;") {
      TranslationUnit(
        None,
        EDDec(
          DInitDecList(
            IDL1(
              SingleDeclaration(
                FullySpecifiedType(
                  None,
                  TypeSpecifier(
                    TSNAFloat,
                    None
                  )
                ),
                Some("x"),
                None,
                Some(
                  Initializer(
                    AEConditionalExpression(
                      ConditionalExpression(
                        LOELogicalXorExpression(
                          LXELogicalAndExpression(
                            LAEInclusiveOrExpression(
                              IOEExclusiveOrExpression(
                                EOEAndExpression(
                                  AEEqualityExpression(
                                    EERelationalExpression(
                                      REShiftExpression(
                                        SEAdditiveExpression(
                                          AEMultiplicativeExpression(
                                            MEUnaryExpression(
                                              UEPostfixExpr(
                                                PostPrimaryExpression(
                                                  PrimNumberConstant("12.3")
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
                        ),
                        None
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

  // TODO
//  val blah: TranslationUnit = TranslationUnit(
//    None,
//    EDDec(
//      DInitDecList(
//        IDL1(
//          SingleDeclaration(
//            FullySpecifiedType(
//              None,
//              TypeSpecifier(TSNAVoid, None)
//            ),
//            None,
//            None,
//            None
//          )
//        )
//      )
//    )
//  )

  test("float x = .123;") {
    assertAst("float x = .123;") {
      TranslationUnit(
        None,
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
          Some("x"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOELogicalXorExpression(LXELogicalAndExpression(
              LAEInclusiveOrExpression(
                IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                  EERelationalExpression(
                    REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                      MEUnaryExpression(
                        UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant(".123")))
                      )
                    )))
                  )
                )))
              )
            )),
            None
          ))))
        ))))
      )
    }
  }

  test("float x = 123.;") {
    assertAst("float x = 123.;") {
      TranslationUnit(
        None,
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
          Some("x"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOELogicalXorExpression(LXELogicalAndExpression(
              LAEInclusiveOrExpression(
                IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                  EERelationalExpression(
                    REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                      MEUnaryExpression(
                        UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("123.")))
                      )
                    )))
                  )
                )))
              )
            )),
            None
          ))))
        ))))
      )
    }
  }

  test("float y = x;") {
    assertAst("float y = x;") {
      TranslationUnit(
        None,
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
          Some("y"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOELogicalXorExpression(LXELogicalAndExpression(
              LAEInclusiveOrExpression(
                IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                  EERelationalExpression(
                    REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                      MEUnaryExpression(
                        UEPostfixExpr(
                          PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("x")))
                        )
                      )
                    )))
                  )
                )))
              )
            )),
            None
          ))))
        ))))
      )
    }
  }

  test("float y = x.xyz;") {
    assertAst("float y = x.xyz;") {
      TranslationUnit(
        None,
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
          Some("y"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOELogicalXorExpression(LXELogicalAndExpression(LAEInclusiveOrExpression(
              IOEExclusiveOrExpression(
                EOEAndExpression(AEEqualityExpression(EERelationalExpression(REShiftExpression(
                  SEAdditiveExpression(
                    AEMultiplicativeExpression(MEUnaryExpression(UEPostfixExpr(PostDotSelect(
                      PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("x"))),
                      "xyz"
                    ))))
                  )
                ))))
              )
            ))),
            None
          ))))
        ))))
      )
    }
  }

  test("vec2 x = 123;") {
    assertAst("vec2 x = 123;") {
      TranslationUnit(
        None,
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAVec2, None)),
          Some("x"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOELogicalXorExpression(LXELogicalAndExpression(
              LAEInclusiveOrExpression(
                IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                  EERelationalExpression(
                    REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                      MEUnaryExpression(
                        UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("123")))
                      )
                    )))
                  )
                )))
              )
            )),
            None
          ))))
        ))))
      )
    }
  }

  test("float x = 123; float y = 0;") {
    assertAst("float x = 123; float y = 0;") {
      TranslationUnit(
        Some(TranslationUnit(
          None,
          EDDec(DInitDecList(IDL1(SingleDeclaration(
            FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
            Some("x"),
            None,
            Some(Initializer(AEConditionalExpression(ConditionalExpression(
              LOELogicalXorExpression(LXELogicalAndExpression(
                LAEInclusiveOrExpression(
                  IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                    EERelationalExpression(
                      REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                        MEUnaryExpression(
                          UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("123")))
                        )
                      )))
                    )
                  )))
                )
              )),
              None
            ))))
          ))))
        )),
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
          Some("y"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOELogicalXorExpression(LXELogicalAndExpression(
              LAEInclusiveOrExpression(
                IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                  EERelationalExpression(
                    REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                      MEUnaryExpression(
                        UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("0")))
                      )
                    )))
                  )
                )))
              )
            )),
            None
          ))))
        ))))
      )
    }
  }

  test("float doStuff(float x) { return 0; }") {
    assertAst("float doStuff(float x) { return 0; }") {
      TranslationUnit(
        None,
        EDFunc(FunctionDefinition(
          FunctionPrototype(FDHeaderWithParams(FHWP1(
            FunctionHeader(FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)), "doStuff"),
            PD2(None, ParameterDeclarator(TypeSpecifier(TSNAFloat, None), "x", None))
          ))),
          CompoundStatementNoNewScope(Some(StatementList(
            SSimple(SSJump(
              JSReturn(Some(EAssignmentExpression(AEConditionalExpression(ConditionalExpression(
                LOELogicalXorExpression(LXELogicalAndExpression(
                  LAEInclusiveOrExpression(
                    IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                      EERelationalExpression(
                        REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                          MEUnaryExpression(
                            UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("0")))
                          )
                        )))
                      )
                    )))
                  )
                )),
                None
              )))))
            )),
            None
          )))
        ))
      )
    }
  }

  test("float doStuff(float x) { float z = 0; return 1; }") {
    assertAst("float doStuff(float x) { float z = 0; return 1; }") {
      TranslationUnit(
        None,
        EDFunc(FunctionDefinition(
          FunctionPrototype(FDHeaderWithParams(FHWP1(
            FunctionHeader(FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)), "doStuff"),
            PD2(None, ParameterDeclarator(TypeSpecifier(TSNAFloat, None), "x", None))
          ))),
          CompoundStatementNoNewScope(Some(StatementList(
            SSimple(SSJump(
              JSReturn(Some(EAssignmentExpression(AEConditionalExpression(ConditionalExpression(
                LOELogicalXorExpression(LXELogicalAndExpression(
                  LAEInclusiveOrExpression(
                    IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                      EERelationalExpression(
                        REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                          MEUnaryExpression(
                            UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("1")))
                          )
                        )))
                      )
                    )))
                  )
                )),
                None
              )))))
            )),
            Some(StatementList(
              SSimple(SSDecl(DeclarationStatement(DInitDecList(IDL1(SingleDeclaration(
                FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
                Some("z"),
                None,
                Some(Initializer(AEConditionalExpression(ConditionalExpression(
                  LOELogicalXorExpression(LXELogicalAndExpression(
                    LAEInclusiveOrExpression(
                      IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                        EERelationalExpression(
                          REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                            MEUnaryExpression(
                              UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("0")))
                            )
                          )))
                        )
                      )))
                    )
                  )),
                  None
                ))))
              )))))),
              None
            ))
          )))
        ))
      )
    }
  }

  test("float doStuff(float x, float y) { return 1; }") {
    assertAst("float doStuff(float x, float y) { return 1; }") {
      TranslationUnit(
        None,
        EDFunc(FunctionDefinition(
          FunctionPrototype(FDHeaderWithParams(FHWP2(
            FHWP1(
              FunctionHeader(FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)), "doStuff"),
              PD2(None, ParameterDeclarator(TypeSpecifier(TSNAFloat, None), "x", None))
            ),
            PD2(None, ParameterDeclarator(TypeSpecifier(TSNAFloat, None), "y", None))
          ))),
          CompoundStatementNoNewScope(Some(StatementList(
            SSimple(SSJump(
              JSReturn(Some(EAssignmentExpression(AEConditionalExpression(ConditionalExpression(
                LOELogicalXorExpression(LXELogicalAndExpression(
                  LAEInclusiveOrExpression(
                    IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                      EERelationalExpression(
                        REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                          MEUnaryExpression(
                            UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("1")))
                          )
                        )))
                      )
                    )))
                  )
                )),
                None
              )))))
            )),
            None
          )))
        ))
      )
    }
  }

  test("fun") {
    assertAst(
      """
        |float doStuff(float x, float y) { return 1; }
        |float x = 12;
        |""".stripMargin
    ) {
      TranslationUnit(
        Some(TranslationUnit(
          None,
          EDFunc(FunctionDefinition(
            FunctionPrototype(FDHeaderWithParams(FHWP2(
              FHWP1(
                FunctionHeader(FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)), "doStuff"),
                PD2(None, ParameterDeclarator(TypeSpecifier(TSNAFloat, None), "x", None))
              ),
              PD2(None, ParameterDeclarator(TypeSpecifier(TSNAFloat, None), "y", None))
            ))),
            CompoundStatementNoNewScope(Some(StatementList(
              SSimple(SSJump(
                JSReturn(Some(EAssignmentExpression(AEConditionalExpression(ConditionalExpression(
                  LOELogicalXorExpression(LXELogicalAndExpression(
                    LAEInclusiveOrExpression(
                      IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                        EERelationalExpression(
                          REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                            MEUnaryExpression(
                              UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("1")))
                            )
                          )))
                        )
                      )))
                    )
                  )),
                  None
                )))))
              )),
              None
            )))
          ))
        )),
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
          Some("x"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOELogicalXorExpression(LXELogicalAndExpression(
              LAEInclusiveOrExpression(
                IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                  EERelationalExpression(
                    REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                      MEUnaryExpression(
                        UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("12")))
                      )
                    )))
                  )
                )))
              )
            )),
            None
          ))))
        ))))
      )
    }
  }

  test("void myFunc(in float x) { return 1; }") {
    assertAst("void myFunc(in float x) { return 1; }") {
      TranslationUnit(
        None,
        EDFunc(FunctionDefinition(
          FunctionPrototype(FDHeaderWithParams(FHWP1(
            FunctionHeader(FullySpecifiedType(None, TypeSpecifier(TSNAVoid, None)), "myFunc"),
            PD2(Some(PQIn), ParameterDeclarator(TypeSpecifier(TSNAFloat, None), "x", None))
          ))),
          CompoundStatementNoNewScope(Some(StatementList(
            SSimple(SSJump(
              JSReturn(Some(EAssignmentExpression(AEConditionalExpression(ConditionalExpression(
                LOELogicalXorExpression(LXELogicalAndExpression(
                  LAEInclusiveOrExpression(
                    IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                      EERelationalExpression(
                        REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                          MEUnaryExpression(
                            UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("1")))
                          )
                        )))
                      )
                    )))
                  )
                )),
                None
              )))))
            )),
            None
          )))
        ))
      )
    }
  }

  test("void myFunc(out float x) { return 1; }") {
    assertAst("void myFunc(out float x) { return 1; }") {
      TranslationUnit(
        None,
        EDFunc(FunctionDefinition(
          FunctionPrototype(FDHeaderWithParams(FHWP1(
            FunctionHeader(FullySpecifiedType(None, TypeSpecifier(TSNAVoid, None)), "myFunc"),
            PD2(Some(PQOut), ParameterDeclarator(TypeSpecifier(TSNAFloat, None), "x", None))
          ))),
          CompoundStatementNoNewScope(Some(StatementList(
            SSimple(SSJump(
              JSReturn(Some(EAssignmentExpression(AEConditionalExpression(ConditionalExpression(
                LOELogicalXorExpression(LXELogicalAndExpression(
                  LAEInclusiveOrExpression(
                    IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                      EERelationalExpression(
                        REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                          MEUnaryExpression(
                            UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("1")))
                          )
                        )))
                      )
                    )))
                  )
                )),
                None
              )))))
            )),
            None
          )))
        ))
      )
    }
  }

  test("empty fun") {
    assertAst(
      """
        |void f() {
        |
        |}
    """.stripMargin
    ) {
      TranslationUnit(
        None,
        EDFunc(FunctionDefinition(
          FunctionPrototype(FDHeader(FunctionHeader(
            FullySpecifiedType(None, TypeSpecifier(TSNAVoid, None)),
            "f"
          ))),
          CompoundStatementNoNewScope(None)
        ))
      )
    }
  }

  test("todo 1".ignore) {
    // "x;"
  }

  test("todo 2".ignore) {
    // "x = x;"
  }

  test("float x = x + 12;") {
    assertAst("float x = x + 12;") {
      TranslationUnit(
        None,
        EDDec(
          DInitDecList(
            IDL1(
              SingleDeclaration(
                FullySpecifiedType(
                  None,
                  TypeSpecifier(TSNAFloat, None)
                ),
                Some("x"),
                None,
                Some(
                  Initializer(
                    AEConditionalExpression(
                      ConditionalExpression(
                        LOELogicalXorExpression(
                          LXELogicalAndExpression(
                            LAEInclusiveOrExpression(
                              IOEExclusiveOrExpression(
                                EOEAndExpression(
                                  AEEqualityExpression(
                                    EERelationalExpression(
                                      REShiftExpression(
                                        SEAdditiveExpression(
                                          AEAdd(
                                            AEMultiplicativeExpression(
                                              MEUnaryExpression(
                                                UEPostfixExpr(
                                                  PostPrimaryExpression(
                                                    PrimVariableIdentifier(
                                                      VariableIdentifier("x")
                                                    )
                                                  )
                                                )
                                              )
                                            ),
                                            MEUnaryExpression(
                                              UEPostfixExpr(
                                                PostPrimaryExpression(
                                                  PrimNumberConstant("12")
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
                        ),
                        None
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

  test("float x = x * 12;") {
    assertAst("float x = x * 12;") {
      TranslationUnit(
        None,
        EDDec(
          DInitDecList(
            IDL1(
              SingleDeclaration(
                FullySpecifiedType(
                  None,
                  TypeSpecifier(TSNAFloat, None)
                ),
                Some("x"),
                None,
                Some(
                  Initializer(
                    AEConditionalExpression(
                      ConditionalExpression(
                        LOELogicalXorExpression(
                          LXELogicalAndExpression(
                            LAEInclusiveOrExpression(
                              IOEExclusiveOrExpression(
                                EOEAndExpression(
                                  AEEqualityExpression(
                                    EERelationalExpression(
                                      REShiftExpression(
                                        SEAdditiveExpression(
                                          AEMultiplicativeExpression(
                                            MEMul(
                                              MEUnaryExpression(
                                                UEPostfixExpr(
                                                  PostPrimaryExpression(
                                                    PrimVariableIdentifier(
                                                      VariableIdentifier("x")
                                                    )
                                                  )
                                                )
                                              ),
                                              UEPostfixExpr(
                                                PostPrimaryExpression(
                                                  PrimNumberConstant("12")
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
                        ),
                        None
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

  test("float x = 1 + 2 * 3;") {
    assertAst("float x = 1 + 2 * 3;") {
      TranslationUnit(
        None,
        EDDec(
          DInitDecList(
            IDL1(
              SingleDeclaration(
                FullySpecifiedType(
                  None,
                  TypeSpecifier(TSNAFloat, None)
                ),
                Some("x"),
                None,
                Some(
                  Initializer(
                    AEConditionalExpression(
                      ConditionalExpression(
                        LOELogicalXorExpression(
                          LXELogicalAndExpression(
                            LAEInclusiveOrExpression(
                              IOEExclusiveOrExpression(
                                EOEAndExpression(
                                  AEEqualityExpression(
                                    EERelationalExpression(
                                      REShiftExpression(
                                        SEAdditiveExpression(
                                          AEAdd(
                                            AEMultiplicativeExpression(
                                              MEUnaryExpression(
                                                UEPostfixExpr(
                                                  PostPrimaryExpression(
                                                    PrimNumberConstant("1")
                                                  )
                                                )
                                              )
                                            ),
                                            MEMul(
                                              MEUnaryExpression(
                                                UEPostfixExpr(
                                                  PostPrimaryExpression(
                                                    PrimNumberConstant("2")
                                                  )
                                                )
                                              ),
                                              UEPostfixExpr(
                                                PostPrimaryExpression(
                                                  PrimNumberConstant("3")
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
                        ),
                        None
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

  test("float x = 1 * 2 + 3;") {
    assertAst("float x = 1 * 2 + 3;") {
      TranslationUnit(
        None,
        EDDec(
          DInitDecList(
            IDL1(
              SingleDeclaration(
                FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
                Some("x"),
                None,
                Some(
                  Initializer(
                    AEConditionalExpression(
                      ConditionalExpression(
                        LOELogicalXorExpression(
                          LXELogicalAndExpression(
                            LAEInclusiveOrExpression(
                              IOEExclusiveOrExpression(
                                EOEAndExpression(
                                  AEEqualityExpression(
                                    EERelationalExpression(
                                      REShiftExpression(
                                        SEAdditiveExpression(
                                          AEAdd(
                                            AEMultiplicativeExpression(
                                              MEMul(
                                                MEUnaryExpression(
                                                  UEPostfixExpr(
                                                    PostPrimaryExpression(
                                                      PrimNumberConstant("1")
                                                    )
                                                  )
                                                ),
                                                UEPostfixExpr(
                                                  PostPrimaryExpression(
                                                    PrimNumberConstant("2")
                                                  )
                                                )
                                              )
                                            ),
                                            MEUnaryExpression(
                                              UEPostfixExpr(
                                                PostPrimaryExpression(
                                                  PrimNumberConstant("3")
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
                        ),
                        None
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

  test("vec2 uv = fragCoord/iResolution.xy;") {
    assertAst("vec2 uv = fragCoord/iResolution.xy;") {
      TranslationUnit(
        None,
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAVec2, None)),
          Some("uv"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOELogicalXorExpression(LXELogicalAndExpression(
              LAEInclusiveOrExpression(
                IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                  EERelationalExpression(
                    REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(MEDiv(
                      MEUnaryExpression(
                        UEPostfixExpr(PostPrimaryExpression(
                          PrimVariableIdentifier(VariableIdentifier("fragCoord"))
                        ))
                      ),
                      UEPostfixExpr(PostDotSelect(
                        PostPrimaryExpression(
                          PrimVariableIdentifier(VariableIdentifier("iResolution"))
                        ),
                        "xy"
                      ))
                    ))))
                  )
                )))
              )
            )),
            None
          ))))
        ))))
      )
    }
  }

  test("vec3 col = 0.5 + 0.5*cos(iTime+uv.xyx+vec3(0,2,4));") {
    assertAst("vec3 col = 0.5 + 0.5*cos(iTime+uv.xyx+vec3(0,2,4));") {
      TranslationUnit(
        None,
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAVec3, None)),
          Some("col"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOELogicalXorExpression(
              LXELogicalAndExpression(LAEInclusiveOrExpression(IOEExclusiveOrExpression(
                EOEAndExpression(AEEqualityExpression(
                  EERelationalExpression(REShiftExpression(SEAdditiveExpression(AEAdd(
                    AEMultiplicativeExpression(
                      MEUnaryExpression(
                        UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("0.5")))
                      )
                    ),
                    MEMul(
                      MEUnaryExpression(
                        UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("0.5")))
                      ),
                      UEPostfixExpr(PostFunctionCall(FunctionCall(FCMGeneric(FCGWithParams(FCHWP1(
                        FunctionCallHeader(FITIdentifier("cos")),
                        AEConditionalExpression(ConditionalExpression(
                          LOELogicalXorExpression(
                            LXELogicalAndExpression(
                              LAEInclusiveOrExpression(IOEExclusiveOrExpression(EOEAndExpression(
                                AEEqualityExpression(EERelationalExpression(
                                  REShiftExpression(SEAdditiveExpression(AEAdd(
                                    AEAdd(
                                      AEMultiplicativeExpression(MEUnaryExpression(
                                        UEPostfixExpr(PostPrimaryExpression(
                                          PrimVariableIdentifier(VariableIdentifier("iTime"))
                                        ))
                                      )),
                                      MEUnaryExpression(UEPostfixExpr(PostDotSelect(
                                        PostPrimaryExpression(
                                          PrimVariableIdentifier(VariableIdentifier("uv"))
                                        ),
                                        "xyx"
                                      )))
                                    ),
                                    MEUnaryExpression(
                                      UEPostfixExpr(PostFunctionCall(
                                        FunctionCall(FCMGeneric(FCGWithParams(FCHWP2(
                                          FCHWP2(
                                            FCHWP1(
                                              FunctionCallHeader(FITypeSpecifier(TypeSpecifier(
                                                TSNAVec3,
                                                None
                                              ))),
                                              AEConditionalExpression(ConditionalExpression(
                                                LOELogicalXorExpression(
                                                  LXELogicalAndExpression(LAEInclusiveOrExpression(
                                                    IOEExclusiveOrExpression(
                                                      EOEAndExpression(AEEqualityExpression(
                                                        EERelationalExpression(REShiftExpression(
                                                          SEAdditiveExpression(
                                                            AEMultiplicativeExpression(
                                                              MEUnaryExpression(
                                                                UEPostfixExpr(PostPrimaryExpression(
                                                                  PrimNumberConstant("0")
                                                                ))
                                                              )
                                                            )
                                                          )
                                                        ))
                                                      ))
                                                    )
                                                  ))
                                                ),
                                                None
                                              ))
                                            ),
                                            AEConditionalExpression(ConditionalExpression(
                                              LOELogicalXorExpression(
                                                LXELogicalAndExpression(LAEInclusiveOrExpression(
                                                  IOEExclusiveOrExpression(
                                                    EOEAndExpression(
                                                      AEEqualityExpression(EERelationalExpression(
                                                        REShiftExpression(SEAdditiveExpression(
                                                          AEMultiplicativeExpression(
                                                            MEUnaryExpression(
                                                              UEPostfixExpr(PostPrimaryExpression(
                                                                PrimNumberConstant("2")
                                                              ))
                                                            )
                                                          )
                                                        ))
                                                      ))
                                                    )
                                                  )
                                                ))
                                              ),
                                              None
                                            ))
                                          ),
                                          AEConditionalExpression(ConditionalExpression(
                                            LOELogicalXorExpression(
                                              LXELogicalAndExpression(LAEInclusiveOrExpression(
                                                IOEExclusiveOrExpression(EOEAndExpression(
                                                  AEEqualityExpression(EERelationalExpression(
                                                    REShiftExpression(SEAdditiveExpression(
                                                      AEMultiplicativeExpression(
                                                        MEUnaryExpression(
                                                          UEPostfixExpr(PostPrimaryExpression(
                                                            PrimNumberConstant("4")
                                                          ))
                                                        )
                                                      )
                                                    ))
                                                  ))
                                                ))
                                              ))
                                            ),
                                            None
                                          ))
                                        ))))
                                      ))
                                    )
                                  )))
                                ))
                              )))
                            )
                          ),
                          None
                        ))
                      ))))))
                    )
                  ))))
                ))
              )))
            ),
            None
          ))))
        ))))
      )
    }
  }

  test("todo 3".ignore) {
    // "fragColor = vec4(col,1.0);"
  }

  test("todo 4".ignore) {
    // """
    //      |// comment
    //      |x = 1;
    //      |""".stripMargin
  }

  test("todo 5".ignore) {
    //  """
    //      |/*
    //      |  comment
    //      |*/
    //      |x = 1;
    //      |""".stripMargin.
  }

  test("big fun") {
    assertAst(
      """
        |void mainImage( out vec4 fragColor, in vec2 fragCoord )
        |{
        |    vec2 uv = fragCoord/iResolution.xy;
        |
        |    vec3 col = 0.5 + 0.5*cos(iTime+uv.xyx+vec3(0,2,4));
        |
        |    fragColor = vec4(col,1.0);
        |}
        |""".stripMargin
    ) {
      TranslationUnit(
        None,
        EDFunc(
          FunctionDefinition(
            FunctionPrototype(
              FDHeaderWithParams(
                FHWP2(
                  FHWP1(
                    FunctionHeader(
                      FullySpecifiedType(None, TypeSpecifier(TSNAVoid, None)),
                      "mainImage"
                    ),
                    PD2(
                      Some(PQOut),
                      ParameterDeclarator(
                        TypeSpecifier(TSNAVec4, None),
                        "fragColor",
                        None
                      )
                    )
                  ),
                  PD2(
                    Some(PQIn),
                    ParameterDeclarator(
                      TypeSpecifier(TSNAVec2, None),
                      "fragCoord",
                      None
                    )
                  )
                )
              )
            ),
            CompoundStatementNoNewScope(
              Some(
                StatementList(
                  SSimple(
                    SSExp(
                      ExpressionStatement(
                        Some(
                          EAssignmentExpression(
                            AEAssign(
                              UEPostfixExpr(
                                PostPrimaryExpression(
                                  PrimVariableIdentifier(
                                    VariableIdentifier("fragColor")
                                  )
                                )
                              ),
                              AEEqual,
                              AEConditionalExpression(
                                ConditionalExpression(
                                  LOELogicalXorExpression(
                                    LXELogicalAndExpression(
                                      LAEInclusiveOrExpression(
                                        IOEExclusiveOrExpression(
                                          EOEAndExpression(
                                            AEEqualityExpression(
                                              EERelationalExpression(
                                                REShiftExpression(
                                                  SEAdditiveExpression(
                                                    AEMultiplicativeExpression(
                                                      MEUnaryExpression(
                                                        UEPostfixExpr(
                                                          PostFunctionCall(
                                                            FunctionCall(
                                                              FCMGeneric(
                                                                FCGWithParams(
                                                                  FCHWP2(
                                                                    FCHWP1(
                                                                      FunctionCallHeader(
                                                                        FITypeSpecifier(
                                                                          TypeSpecifier(
                                                                            TSNAVec4,
                                                                            None
                                                                          )
                                                                        )
                                                                      ),
                                                                      AEConditionalExpression(
                                                                        ConditionalExpression(
                                                                          LOELogicalXorExpression(
                                                                            LXELogicalAndExpression(
                                                                              LAEInclusiveOrExpression(
                                                                                IOEExclusiveOrExpression(
                                                                                  EOEAndExpression(
                                                                                    AEEqualityExpression(
                                                                                      EERelationalExpression(
                                                                                        REShiftExpression(
                                                                                          SEAdditiveExpression(
                                                                                            AEMultiplicativeExpression(
                                                                                              MEUnaryExpression(
                                                                                                UEPostfixExpr(
                                                                                                  PostPrimaryExpression(
                                                                                                    PrimVariableIdentifier(
                                                                                                      VariableIdentifier(
                                                                                                        "col"
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
                                                                            )
                                                                          ),
                                                                          None
                                                                        )
                                                                      )
                                                                    ),
                                                                    AEConditionalExpression(
                                                                      ConditionalExpression(
                                                                        LOELogicalXorExpression(
                                                                          LXELogicalAndExpression(
                                                                            LAEInclusiveOrExpression(
                                                                              IOEExclusiveOrExpression(
                                                                                EOEAndExpression(
                                                                                  AEEqualityExpression(
                                                                                    EERelationalExpression(
                                                                                      REShiftExpression(
                                                                                        SEAdditiveExpression(
                                                                                          AEMultiplicativeExpression(
                                                                                            MEUnaryExpression(
                                                                                              UEPostfixExpr(
                                                                                                PostPrimaryExpression(
                                                                                                  PrimNumberConstant(
                                                                                                    "1.0"
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
                                                                        ),
                                                                        None
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
                                            )
                                          )
                                        )
                                      )
                                    )
                                  ),
                                  None
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  Some(
                    StatementList(
                      SSimple(
                        SSDecl(
                          DeclarationStatement(
                            DInitDecList(
                              IDL1(
                                SingleDeclaration(
                                  FullySpecifiedType(None, TypeSpecifier(TSNAVec3, None)),
                                  Some("col"),
                                  None,
                                  Some(
                                    Initializer(
                                      AEConditionalExpression(
                                        ConditionalExpression(
                                          LOELogicalXorExpression(
                                            LXELogicalAndExpression(
                                              LAEInclusiveOrExpression(
                                                IOEExclusiveOrExpression(
                                                  EOEAndExpression(
                                                    AEEqualityExpression(
                                                      EERelationalExpression(
                                                        REShiftExpression(
                                                          SEAdditiveExpression(
                                                            AEAdd(
                                                              AEMultiplicativeExpression(
                                                                MEUnaryExpression(
                                                                  UEPostfixExpr(
                                                                    PostPrimaryExpression(
                                                                      PrimNumberConstant("0.5")
                                                                    )
                                                                  )
                                                                )
                                                              ),
                                                              MEMul(
                                                                MEUnaryExpression(
                                                                  UEPostfixExpr(
                                                                    PostPrimaryExpression(
                                                                      PrimNumberConstant("0.5")
                                                                    )
                                                                  )
                                                                ),
                                                                UEPostfixExpr(
                                                                  PostFunctionCall(
                                                                    FunctionCall(
                                                                      FCMGeneric(
                                                                        FCGWithParams(
                                                                          FCHWP1(
                                                                            FunctionCallHeader(
                                                                              FITIdentifier("cos")
                                                                            ),
                                                                            AEConditionalExpression(
                                                                              ConditionalExpression(
                                                                                LOELogicalXorExpression(
                                                                                  LXELogicalAndExpression(
                                                                                    LAEInclusiveOrExpression(
                                                                                      IOEExclusiveOrExpression(
                                                                                        EOEAndExpression(
                                                                                          AEEqualityExpression(
                                                                                            EERelationalExpression(
                                                                                              REShiftExpression(
                                                                                                SEAdditiveExpression(
                                                                                                  AEAdd(
                                                                                                    AEAdd(
                                                                                                      AEMultiplicativeExpression(
                                                                                                        MEUnaryExpression(
                                                                                                          UEPostfixExpr(
                                                                                                            PostPrimaryExpression(
                                                                                                              PrimVariableIdentifier(
                                                                                                                VariableIdentifier(
                                                                                                                  "iTime"
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      ),
                                                                                                      MEUnaryExpression(
                                                                                                        UEPostfixExpr(
                                                                                                          PostDotSelect(
                                                                                                            PostPrimaryExpression(
                                                                                                              PrimVariableIdentifier(
                                                                                                                VariableIdentifier(
                                                                                                                  "uv"
                                                                                                                )
                                                                                                              )
                                                                                                            ),
                                                                                                            "xyx"
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    ),
                                                                                                    MEUnaryExpression(
                                                                                                      UEPostfixExpr(
                                                                                                        PostFunctionCall(
                                                                                                          FunctionCall(
                                                                                                            FCMGeneric(
                                                                                                              FCGWithParams(
                                                                                                                FCHWP2(
                                                                                                                  FCHWP2(
                                                                                                                    FCHWP1(
                                                                                                                      FunctionCallHeader(
                                                                                                                        FITypeSpecifier(
                                                                                                                          TypeSpecifier(TSNAVec3, None)
                                                                                                                        )
                                                                                                                      ),
                                                                                                                      AEConditionalExpression(
                                                                                                                        ConditionalExpression(
                                                                                                                          LOELogicalXorExpression(
                                                                                                                            LXELogicalAndExpression(
                                                                                                                              LAEInclusiveOrExpression(
                                                                                                                                IOEExclusiveOrExpression(
                                                                                                                                  EOEAndExpression(
                                                                                                                                    AEEqualityExpression(
                                                                                                                                      EERelationalExpression(
                                                                                                                                        REShiftExpression(
                                                                                                                                          SEAdditiveExpression(
                                                                                                                                            AEMultiplicativeExpression(
                                                                                                                                              MEUnaryExpression(
                                                                                                                                                UEPostfixExpr(
                                                                                                                                                  PostPrimaryExpression(
                                                                                                                                                    PrimNumberConstant("0")
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
                                                                                                                          ),
                                                                                                                          None
                                                                                                                        )
                                                                                                                      )
                                                                                                                    ),
                                                                                                                    AEConditionalExpression(
                                                                                                                      ConditionalExpression(
                                                                                                                        LOELogicalXorExpression(
                                                                                                                          LXELogicalAndExpression(
                                                                                                                            LAEInclusiveOrExpression(
                                                                                                                              IOEExclusiveOrExpression(
                                                                                                                                EOEAndExpression(
                                                                                                                                  AEEqualityExpression(
                                                                                                                                    EERelationalExpression(
                                                                                                                                      REShiftExpression(
                                                                                                                                        SEAdditiveExpression(
                                                                                                                                          AEMultiplicativeExpression(
                                                                                                                                            MEUnaryExpression(
                                                                                                                                              UEPostfixExpr(
                                                                                                                                                PostPrimaryExpression(
                                                                                                                                                  PrimNumberConstant("2")
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
                                                                                                                        ),
                                                                                                                        None
                                                                                                                      )
                                                                                                                    )
                                                                                                                  ),
                                                                                                                  AEConditionalExpression(
                                                                                                                    ConditionalExpression(
                                                                                                                      LOELogicalXorExpression(
                                                                                                                        LXELogicalAndExpression(
                                                                                                                          LAEInclusiveOrExpression(
                                                                                                                            IOEExclusiveOrExpression(
                                                                                                                              EOEAndExpression(
                                                                                                                                AEEqualityExpression(
                                                                                                                                  EERelationalExpression(
                                                                                                                                    REShiftExpression(
                                                                                                                                      SEAdditiveExpression(
                                                                                                                                        AEMultiplicativeExpression(
                                                                                                                                          MEUnaryExpression(
                                                                                                                                            UEPostfixExpr(
                                                                                                                                              PostPrimaryExpression(
                                                                                                                                                PrimNumberConstant("4")
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
                                                                                                                      ),
                                                                                                                      None
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
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                ),
                                                                                None
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
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          ),
                                          None
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      ),
                      Some(
                        StatementList(
                          SSimple(
                            SSDecl(
                              DeclarationStatement(
                                DInitDecList(
                                  IDL1(
                                    SingleDeclaration(
                                      FullySpecifiedType(None, TypeSpecifier(TSNAVec2, None)),
                                      Some("uv"),
                                      None,
                                      Some(
                                        Initializer(
                                          AEConditionalExpression(
                                            ConditionalExpression(
                                              LOELogicalXorExpression(
                                                LXELogicalAndExpression(
                                                  LAEInclusiveOrExpression(
                                                    IOEExclusiveOrExpression(
                                                      EOEAndExpression(
                                                        AEEqualityExpression(
                                                          EERelationalExpression(
                                                            REShiftExpression(
                                                              SEAdditiveExpression(
                                                                AEMultiplicativeExpression(
                                                                  MEDiv(
                                                                    MEUnaryExpression(
                                                                      UEPostfixExpr(
                                                                        PostPrimaryExpression(
                                                                          PrimVariableIdentifier(
                                                                            VariableIdentifier(
                                                                              "fragCoord"
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    ),
                                                                    UEPostfixExpr(
                                                                      PostDotSelect(
                                                                        PostPrimaryExpression(
                                                                          PrimVariableIdentifier(
                                                                            VariableIdentifier(
                                                                              "iResolution"
                                                                            )
                                                                          )
                                                                        ),
                                                                        "xy"
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
                                              ),
                                              None
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          ),
                          None
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
}
