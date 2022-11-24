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

class Glsl1Test extends DslTestFunSuite[Glsl.type] {

  type Ast = TranslationUnit
  override def dsl: Glsl.type = Glsl

  test("float x = (y);") {
    assertAst("float x = (y);") {
      TranslationUnit(
        None,
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
          Some("x"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOELogicalXorExpression(LXELogicalAndExpression(LAEInclusiveOrExpression(
              IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(EERelationalExpression(
                REShiftExpression(
                  SEAdditiveExpression(AEMultiplicativeExpression(MEUnaryExpression(UEPostfixExpr(
                    PostPrimaryExpression(PrimParens(
                      EAssignmentExpression(AEConditionalExpression(ConditionalExpression(
                        LOELogicalXorExpression(LXELogicalAndExpression(LAEInclusiveOrExpression(
                          IOEExclusiveOrExpression(
                            EOEAndExpression(AEEqualityExpression(EERelationalExpression(
                              REShiftExpression(
                                SEAdditiveExpression(AEMultiplicativeExpression(MEUnaryExpression(
                                  UEPostfixExpr(PostPrimaryExpression(
                                    PrimVariableIdentifier(VariableIdentifier("y"))
                                  ))
                                )))
                              )
                            )))
                          )
                        ))),
                        None
                      )))
                    ))
                  ))))
                )
              ))))
            ))),
            None
          ))))
        ))))
      )
    }
  }

  test("float x = (y + 1) - 2;") {
    assertAst("float x = (y + 1) - 2;") {
      TranslationUnit(
        None,
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
          Some("x"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOELogicalXorExpression(
              LXELogicalAndExpression(LAEInclusiveOrExpression(IOEExclusiveOrExpression(
                EOEAndExpression(AEEqualityExpression(
                  EERelationalExpression(REShiftExpression(SEAdditiveExpression(AESub(
                    AEMultiplicativeExpression(MEUnaryExpression(UEPostfixExpr(
                      PostPrimaryExpression(PrimParens(
                        EAssignmentExpression(AEConditionalExpression(ConditionalExpression(
                          LOELogicalXorExpression(
                            LXELogicalAndExpression(
                              LAEInclusiveOrExpression(IOEExclusiveOrExpression(EOEAndExpression(
                                AEEqualityExpression(EERelationalExpression(
                                  REShiftExpression(SEAdditiveExpression(AEAdd(
                                    AEMultiplicativeExpression(MEUnaryExpression(
                                      UEPostfixExpr(PostPrimaryExpression(
                                        PrimVariableIdentifier(VariableIdentifier("y"))
                                      ))
                                    )),
                                    MEUnaryExpression(
                                      UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("1")))
                                    )
                                  )))
                                ))
                              )))
                            )
                          ),
                          None
                        )))
                      ))
                    ))),
                    MEUnaryExpression(UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("2"))))
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

  test("float x = y + (1 - 2);") {
    assertAst("float x = y + (1 - 2);") {
      TranslationUnit(
        None,
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
          Some("x"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOELogicalXorExpression(
              LXELogicalAndExpression(LAEInclusiveOrExpression(IOEExclusiveOrExpression(
                EOEAndExpression(AEEqualityExpression(
                  EERelationalExpression(REShiftExpression(SEAdditiveExpression(AEAdd(
                    AEMultiplicativeExpression(
                      MEUnaryExpression(
                        UEPostfixExpr(
                          PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                        )
                      )
                    ),
                    MEUnaryExpression(UEPostfixExpr(
                      PostPrimaryExpression(PrimParens(
                        EAssignmentExpression(AEConditionalExpression(ConditionalExpression(
                          LOELogicalXorExpression(
                            LXELogicalAndExpression(
                              LAEInclusiveOrExpression(IOEExclusiveOrExpression(EOEAndExpression(
                                AEEqualityExpression(EERelationalExpression(
                                  REShiftExpression(SEAdditiveExpression(AESub(
                                    AEMultiplicativeExpression(
                                      MEUnaryExpression(UEPostfixExpr(
                                        PostPrimaryExpression(PrimNumberConstant("1"))
                                      ))
                                    ),
                                    MEUnaryExpression(
                                      UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("2")))
                                    )
                                  )))
                                ))
                              )))
                            )
                          ),
                          None
                        )))
                      ))
                    ))
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

  test("float x = +y;") {
    assertAst("float x = +y;") {
      TranslationUnit(
        None,
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
          Some("x"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOELogicalXorExpression(LXELogicalAndExpression(LAEInclusiveOrExpression(
              IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(EERelationalExpression(
                REShiftExpression(
                  SEAdditiveExpression(AEMultiplicativeExpression(MEUnaryExpression(UEUnaryOp(
                    UOPlus,
                    UEPostfixExpr(
                      PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                    )
                  ))))
                )
              ))))
            ))),
            None
          ))))
        ))))
      )
    }
  }

  test("float x = -y;") {
    assertAst("float x = -y;") {
      TranslationUnit(
        None,
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
          Some("x"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOELogicalXorExpression(LXELogicalAndExpression(LAEInclusiveOrExpression(
              IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(EERelationalExpression(
                REShiftExpression(
                  SEAdditiveExpression(AEMultiplicativeExpression(MEUnaryExpression(UEUnaryOp(
                    UODash,
                    UEPostfixExpr(
                      PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                    )
                  ))))
                )
              ))))
            ))),
            None
          ))))
        ))))
      )
    }
  }

  test("float x = ~y;") {
    assertAst("float x = ~y;") {
      TranslationUnit(
        None,
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
          Some("x"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOELogicalXorExpression(LXELogicalAndExpression(LAEInclusiveOrExpression(
              IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(EERelationalExpression(
                REShiftExpression(
                  SEAdditiveExpression(AEMultiplicativeExpression(MEUnaryExpression(UEUnaryOp(
                    UOTilde,
                    UEPostfixExpr(
                      PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                    )
                  ))))
                )
              ))))
            ))),
            None
          ))))
        ))))
      )
    }
  }

  test("float x = !y;") {
    assertAst("float x = !y;") {
      TranslationUnit(
        None,
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
          Some("x"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOELogicalXorExpression(LXELogicalAndExpression(LAEInclusiveOrExpression(
              IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(EERelationalExpression(
                REShiftExpression(
                  SEAdditiveExpression(AEMultiplicativeExpression(MEUnaryExpression(UEUnaryOp(
                    UOBang,
                    UEPostfixExpr(
                      PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                    )
                  ))))
                )
              ))))
            ))),
            None
          ))))
        ))))
      )
    }
  }

  test("float x = y % 1;") {
    assertAst("float x = y % 1;") {
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
                    REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(MEMod(
                      MEUnaryExpression(
                        UEPostfixExpr(
                          PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                        )
                      ),
                      UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("1")))
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

  test("float x = y << 1;") {
    assertAst("float x = y << 1;") {
      TranslationUnit(
        None,
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
          Some("x"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOELogicalXorExpression(
              LXELogicalAndExpression(LAEInclusiveOrExpression(IOEExclusiveOrExpression(
                EOEAndExpression(
                  AEEqualityExpression(EERelationalExpression(REShiftExpression(SELeft(
                    SEAdditiveExpression(AEMultiplicativeExpression(
                      MEUnaryExpression(
                        UEPostfixExpr(
                          PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                        )
                      )
                    )),
                    AEMultiplicativeExpression(
                      MEUnaryExpression(
                        UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("1")))
                      )
                    )
                  ))))
                )
              )))
            ),
            None
          ))))
        ))))
      )
    }
  }

  test("float x = y >> 1;") {
    assertAst("float x = y >> 1;") {
      TranslationUnit(
        None,
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
          Some("x"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOELogicalXorExpression(
              LXELogicalAndExpression(LAEInclusiveOrExpression(IOEExclusiveOrExpression(
                EOEAndExpression(
                  AEEqualityExpression(EERelationalExpression(REShiftExpression(SERight(
                    SEAdditiveExpression(AEMultiplicativeExpression(
                      MEUnaryExpression(
                        UEPostfixExpr(
                          PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                        )
                      )
                    )),
                    AEMultiplicativeExpression(
                      MEUnaryExpression(
                        UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("1")))
                      )
                    )
                  ))))
                )
              )))
            ),
            None
          ))))
        ))))
      )
    }
  }

  test("float x = y > 1;") {
    assertAst("float x = y > 1;") {
      TranslationUnit(
        None,
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
          Some("x"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOELogicalXorExpression(LXELogicalAndExpression(LAEInclusiveOrExpression(
              IOEExclusiveOrExpression(
                EOEAndExpression(AEEqualityExpression(EERelationalExpression(REGt(
                  REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                    MEUnaryExpression(
                      UEPostfixExpr(
                        PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                      )
                    )
                  ))),
                  SEAdditiveExpression(AEMultiplicativeExpression(
                    MEUnaryExpression(UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("1"))))
                  ))
                ))))
              )
            ))),
            None
          ))))
        ))))
      )
    }
  }

  test("float x = y >= 1;") {
    assertAst("float x = y >= 1;") {
      TranslationUnit(
        None,
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
          Some("x"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOELogicalXorExpression(LXELogicalAndExpression(LAEInclusiveOrExpression(
              IOEExclusiveOrExpression(
                EOEAndExpression(AEEqualityExpression(EERelationalExpression(REGte(
                  REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                    MEUnaryExpression(
                      UEPostfixExpr(
                        PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                      )
                    )
                  ))),
                  SEAdditiveExpression(AEMultiplicativeExpression(
                    MEUnaryExpression(UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("1"))))
                  ))
                ))))
              )
            ))),
            None
          ))))
        ))))
      )
    }
  }

  test("float x = y < 1;") {
    assertAst("float x = y < 1;") {
      TranslationUnit(
        None,
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
          Some("x"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOELogicalXorExpression(LXELogicalAndExpression(LAEInclusiveOrExpression(
              IOEExclusiveOrExpression(
                EOEAndExpression(AEEqualityExpression(EERelationalExpression(RELt(
                  REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                    MEUnaryExpression(
                      UEPostfixExpr(
                        PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                      )
                    )
                  ))),
                  SEAdditiveExpression(AEMultiplicativeExpression(
                    MEUnaryExpression(UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("1"))))
                  ))
                ))))
              )
            ))),
            None
          ))))
        ))))
      )
    }
  }

  test("float x = y <= 1;") {
    assertAst("float x = y <= 1;") {
      TranslationUnit(
        None,
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
          Some("x"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOELogicalXorExpression(LXELogicalAndExpression(LAEInclusiveOrExpression(
              IOEExclusiveOrExpression(
                EOEAndExpression(AEEqualityExpression(EERelationalExpression(RELte(
                  REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                    MEUnaryExpression(
                      UEPostfixExpr(
                        PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                      )
                    )
                  ))),
                  SEAdditiveExpression(AEMultiplicativeExpression(
                    MEUnaryExpression(UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("1"))))
                  ))
                ))))
              )
            ))),
            None
          ))))
        ))))
      )
    }
  }

  test("float x = y == 1;") {
    assertAst("float x = y == 1;") {
      TranslationUnit(
        None,
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
          Some("x"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOELogicalXorExpression(LXELogicalAndExpression(
              LAEInclusiveOrExpression(
                IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(EEEq(
                  EERelationalExpression(
                    REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                      MEUnaryExpression(
                        UEPostfixExpr(
                          PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                        )
                      )
                    )))
                  ),
                  REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                    MEUnaryExpression(UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("1"))))
                  )))
                ))))
              )
            )),
            None
          ))))
        ))))
      )
    }
  }

  test("float x = y != 1;") {
    assertAst("float x = y != 1;") {
      TranslationUnit(
        None,
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
          Some("x"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOELogicalXorExpression(LXELogicalAndExpression(
              LAEInclusiveOrExpression(
                IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(EENeq(
                  EERelationalExpression(
                    REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                      MEUnaryExpression(
                        UEPostfixExpr(
                          PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                        )
                      )
                    )))
                  ),
                  REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                    MEUnaryExpression(UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("1"))))
                  )))
                ))))
              )
            )),
            None
          ))))
        ))))
      )
    }
  }

  test("float x = y & 1;") {
    assertAst("float x = y & 1;") {
      TranslationUnit(
        None,
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
          Some("x"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOELogicalXorExpression(
              LXELogicalAndExpression(
                LAEInclusiveOrExpression(IOEExclusiveOrExpression(EOEAndExpression(AEAmp(
                  AEEqualityExpression(EERelationalExpression(
                    REShiftExpression(
                      SEAdditiveExpression(AEMultiplicativeExpression(MEUnaryExpression(
                        UEPostfixExpr(
                          PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                        )
                      )))
                    )
                  )),
                  EERelationalExpression(
                    REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                      MEUnaryExpression(
                        UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("1")))
                      )
                    )))
                  )
                ))))
              )
            ),
            None
          ))))
        ))))
      )
    }
  }

  test("float x = y ^ 1;") {
    assertAst("float x = y ^ 1;") {
      TranslationUnit(
        None,
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
          Some("x"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOELogicalXorExpression(
              LXELogicalAndExpression(LAEInclusiveOrExpression(IOEExclusiveOrExpression(EOXor(
                EOEAndExpression(AEEqualityExpression(
                  EERelationalExpression(
                    REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                      MEUnaryExpression(
                        UEPostfixExpr(
                          PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                        )
                      )
                    )))
                  )
                )),
                AEEqualityExpression(
                  EERelationalExpression(
                    REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                      MEUnaryExpression(
                        UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("1")))
                      )
                    )))
                  )
                )
              ))))
            ),
            None
          ))))
        ))))
      )
    }
  }

  test("float x = y | 1;") {
    assertAst("float x = y | 1;") {
      TranslationUnit(
        None,
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
          Some("x"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOELogicalXorExpression(LXELogicalAndExpression(LAEInclusiveOrExpression(IOEIor(
              IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                EERelationalExpression(
                  REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                    MEUnaryExpression(
                      UEPostfixExpr(
                        PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                      )
                    )
                  )))
                )
              ))),
              EOEAndExpression(AEEqualityExpression(
                EERelationalExpression(
                  REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                    MEUnaryExpression(UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("1"))))
                  )))
                )
              ))
            )))),
            None
          ))))
        ))))
      )
    }
  }

  test("float x = y && 1;") {
    assertAst("float x = y && 1;") {
      TranslationUnit(
        None,
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
          Some("x"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOELogicalXorExpression(LXELogicalAndExpression(LAEAnd(
              LAEInclusiveOrExpression(
                IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                  EERelationalExpression(
                    REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                      MEUnaryExpression(
                        UEPostfixExpr(
                          PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                        )
                      )
                    )))
                  )
                )))
              ),
              IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                EERelationalExpression(
                  REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                    MEUnaryExpression(UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("1"))))
                  )))
                )
              )))
            ))),
            None
          ))))
        ))))
      )
    }
  }

  test("float x = y || 1;") {
    assertAst("float x = y || 1;") {
      TranslationUnit(
        None,
        EDDec(DInitDecList(IDL1(SingleDeclaration(
          FullySpecifiedType(None, TypeSpecifier(TSNAFloat, None)),
          Some("x"),
          None,
          Some(Initializer(AEConditionalExpression(ConditionalExpression(
            LOEOr(
              LOELogicalXorExpression(LXELogicalAndExpression(LAEInclusiveOrExpression(
                IOEExclusiveOrExpression(
                  EOEAndExpression(AEEqualityExpression(EERelationalExpression(
                    REShiftExpression(
                      SEAdditiveExpression(AEMultiplicativeExpression(MEUnaryExpression(
                        UEPostfixExpr(
                          PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                        )
                      )))
                    )
                  )))
                )
              ))),
              LXELogicalAndExpression(
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
              )
            ),
            None
          ))))
        ))))
      )
    }
  }

  test("fun 1") {
    assertAst(
      """
        |void f() {
        | if (x == 1)
        |   y = 2;
        |}
        |""".stripMargin
    ) {
      TranslationUnit(
        None,
        EDFunc(FunctionDefinition(
          FunctionPrototype(FDHeader(FunctionHeader(
            FullySpecifiedType(None, TypeSpecifier(TSNAVoid, None)),
            "f"
          ))),
          CompoundStatementNoNewScope(Some(StatementList(
            SSimple(SSSel(SelectionStatement(
              EAssignmentExpression(AEConditionalExpression(ConditionalExpression(
                LOELogicalXorExpression(LXELogicalAndExpression(
                  LAEInclusiveOrExpression(
                    IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(EEEq(
                      EERelationalExpression(
                        REShiftExpression(
                          SEAdditiveExpression(AEMultiplicativeExpression(MEUnaryExpression(
                            UEPostfixExpr(
                              PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("x")))
                            )
                          )))
                        )
                      ),
                      REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                        MEUnaryExpression(
                          UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("1")))
                        )
                      )))
                    ))))
                  )
                )),
                None
              ))),
              SelectionRestStatement(
                SSimple(SSExp(ExpressionStatement(Some(EAssignmentExpression(AEAssign(
                  UEPostfixExpr(
                    PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                  ),
                  AEEqual,
                  AEConditionalExpression(ConditionalExpression(
                    LOELogicalXorExpression(LXELogicalAndExpression(
                      LAEInclusiveOrExpression(
                        IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                          EERelationalExpression(
                            REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                              MEUnaryExpression(
                                UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("2")))
                              )
                            )))
                          )
                        )))
                      )
                    )),
                    None
                  ))
                )))))),
                None
              )
            ))),
            None
          )))
        ))
      )
    }
  }

  test("fun 2") {
    assertAst(
      """
        |void f() {
        | if (x == 1) {
        |   y = 2;
        | }
        |}
        |""".stripMargin
    ) {
      TranslationUnit(
        None,
        EDFunc(FunctionDefinition(
          FunctionPrototype(FDHeader(FunctionHeader(
            FullySpecifiedType(None, TypeSpecifier(TSNAVoid, None)),
            "f"
          ))),
          CompoundStatementNoNewScope(Some(StatementList(
            SSimple(SSSel(SelectionStatement(
              EAssignmentExpression(AEConditionalExpression(ConditionalExpression(
                LOELogicalXorExpression(LXELogicalAndExpression(
                  LAEInclusiveOrExpression(
                    IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(EEEq(
                      EERelationalExpression(
                        REShiftExpression(
                          SEAdditiveExpression(AEMultiplicativeExpression(MEUnaryExpression(
                            UEPostfixExpr(
                              PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("x")))
                            )
                          )))
                        )
                      ),
                      REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                        MEUnaryExpression(
                          UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("1")))
                        )
                      )))
                    ))))
                  )
                )),
                None
              ))),
              SelectionRestStatement(
                SCompound(CompoundStatement(Some(StatementList(
                  SSimple(SSExp(ExpressionStatement(Some(EAssignmentExpression(AEAssign(
                    UEPostfixExpr(
                      PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                    ),
                    AEEqual,
                    AEConditionalExpression(ConditionalExpression(
                      LOELogicalXorExpression(LXELogicalAndExpression(
                        LAEInclusiveOrExpression(
                          IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                            EERelationalExpression(
                              REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                                MEUnaryExpression(
                                  UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("2")))
                                )
                              )))
                            )
                          )))
                        )
                      )),
                      None
                    ))
                  )))))),
                  None
                )))),
                None
              )
            ))),
            None
          )))
        ))
      )
    }
  }

  test("fun 3") {
    assertAst(
      """
        |void f() {
        |  if (x == 1) {
        |    y = 2;
        |  } else if (x == 3) {
        |    y = 4;
        |  }
        |}
        |""".stripMargin
    ) {
      TranslationUnit(
        None,
        EDFunc(FunctionDefinition(
          FunctionPrototype(FDHeader(FunctionHeader(
            FullySpecifiedType(None, TypeSpecifier(TSNAVoid, None)),
            "f"
          ))),
          CompoundStatementNoNewScope(Some(StatementList(
            SSimple(SSSel(SelectionStatement(
              EAssignmentExpression(AEConditionalExpression(ConditionalExpression(
                LOELogicalXorExpression(LXELogicalAndExpression(
                  LAEInclusiveOrExpression(
                    IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(EEEq(
                      EERelationalExpression(
                        REShiftExpression(
                          SEAdditiveExpression(AEMultiplicativeExpression(MEUnaryExpression(
                            UEPostfixExpr(
                              PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("x")))
                            )
                          )))
                        )
                      ),
                      REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                        MEUnaryExpression(
                          UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("1")))
                        )
                      )))
                    ))))
                  )
                )),
                None
              ))),
              SelectionRestStatement(
                SCompound(CompoundStatement(Some(StatementList(
                  SSimple(SSExp(ExpressionStatement(Some(EAssignmentExpression(AEAssign(
                    UEPostfixExpr(
                      PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                    ),
                    AEEqual,
                    AEConditionalExpression(ConditionalExpression(
                      LOELogicalXorExpression(LXELogicalAndExpression(
                        LAEInclusiveOrExpression(
                          IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                            EERelationalExpression(
                              REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                                MEUnaryExpression(
                                  UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("2")))
                                )
                              )))
                            )
                          )))
                        )
                      )),
                      None
                    ))
                  )))))),
                  None
                )))),
                Some(SSimple(SSSel(SelectionStatement(
                  EAssignmentExpression(AEConditionalExpression(ConditionalExpression(
                    LOELogicalXorExpression(LXELogicalAndExpression(
                      LAEInclusiveOrExpression(
                        IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(EEEq(
                          EERelationalExpression(
                            REShiftExpression(
                              SEAdditiveExpression(AEMultiplicativeExpression(MEUnaryExpression(
                                UEPostfixExpr(PostPrimaryExpression(
                                  PrimVariableIdentifier(VariableIdentifier("x"))
                                ))
                              )))
                            )
                          ),
                          REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                            MEUnaryExpression(
                              UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("3")))
                            )
                          )))
                        ))))
                      )
                    )),
                    None
                  ))),
                  SelectionRestStatement(
                    SCompound(CompoundStatement(Some(StatementList(
                      SSimple(SSExp(ExpressionStatement(Some(EAssignmentExpression(AEAssign(
                        UEPostfixExpr(
                          PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                        ),
                        AEEqual,
                        AEConditionalExpression(ConditionalExpression(
                          LOELogicalXorExpression(LXELogicalAndExpression(
                            LAEInclusiveOrExpression(
                              IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                                EERelationalExpression(
                                  REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                                    MEUnaryExpression(
                                      UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("4")))
                                    )
                                  )))
                                )
                              )))
                            )
                          )),
                          None
                        ))
                      )))))),
                      None
                    )))),
                    None
                  )
                ))))
              )
            ))),
            None
          )))
        ))
      )
    }
  }

  test("fun 4") {
    assertAst(
      """
        |void f() {
        |  if (x == 1)
        |    y = 2;
        |  else
        |    y = 3;
        |}
        |""".stripMargin
    ) {
      TranslationUnit(
        None,
        EDFunc(FunctionDefinition(
          FunctionPrototype(FDHeader(FunctionHeader(
            FullySpecifiedType(None, TypeSpecifier(TSNAVoid, None)),
            "f"
          ))),
          CompoundStatementNoNewScope(Some(StatementList(
            SSimple(SSSel(SelectionStatement(
              EAssignmentExpression(AEConditionalExpression(ConditionalExpression(
                LOELogicalXorExpression(LXELogicalAndExpression(
                  LAEInclusiveOrExpression(
                    IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(EEEq(
                      EERelationalExpression(
                        REShiftExpression(
                          SEAdditiveExpression(AEMultiplicativeExpression(MEUnaryExpression(
                            UEPostfixExpr(
                              PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("x")))
                            )
                          )))
                        )
                      ),
                      REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                        MEUnaryExpression(
                          UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("1")))
                        )
                      )))
                    ))))
                  )
                )),
                None
              ))),
              SelectionRestStatement(
                SSimple(SSExp(ExpressionStatement(Some(EAssignmentExpression(AEAssign(
                  UEPostfixExpr(
                    PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                  ),
                  AEEqual,
                  AEConditionalExpression(ConditionalExpression(
                    LOELogicalXorExpression(LXELogicalAndExpression(
                      LAEInclusiveOrExpression(
                        IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                          EERelationalExpression(
                            REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                              MEUnaryExpression(
                                UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("2")))
                              )
                            )))
                          )
                        )))
                      )
                    )),
                    None
                  ))
                )))))),
                Some(SSimple(SSExp(ExpressionStatement(Some(EAssignmentExpression(AEAssign(
                  UEPostfixExpr(
                    PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                  ),
                  AEEqual,
                  AEConditionalExpression(ConditionalExpression(
                    LOELogicalXorExpression(LXELogicalAndExpression(
                      LAEInclusiveOrExpression(
                        IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                          EERelationalExpression(
                            REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                              MEUnaryExpression(
                                UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("3")))
                              )
                            )))
                          )
                        )))
                      )
                    )),
                    None
                  ))
                )))))))
              )
            ))),
            None
          )))
        ))
      )
    }
  }

  test("fun 5") {
    assertAst(
      """
        |void f() {
        |  if (x == 1)
        |    y = 10;
        |  else if (x == 2)
        |    y = 20;
        |  else if (x == 3)
        |    y = 30;
        |  else
        |    y = 40;
        |}
        |""".stripMargin
    ) {
      TranslationUnit(
        None,
        EDFunc(FunctionDefinition(
          FunctionPrototype(FDHeader(FunctionHeader(
            FullySpecifiedType(None, TypeSpecifier(TSNAVoid, None)),
            "f"
          ))),
          CompoundStatementNoNewScope(Some(StatementList(
            SSimple(SSSel(SelectionStatement(
              EAssignmentExpression(AEConditionalExpression(ConditionalExpression(
                LOELogicalXorExpression(LXELogicalAndExpression(
                  LAEInclusiveOrExpression(
                    IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(EEEq(
                      EERelationalExpression(
                        REShiftExpression(
                          SEAdditiveExpression(AEMultiplicativeExpression(MEUnaryExpression(
                            UEPostfixExpr(
                              PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("x")))
                            )
                          )))
                        )
                      ),
                      REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                        MEUnaryExpression(
                          UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("1")))
                        )
                      )))
                    ))))
                  )
                )),
                None
              ))),
              SelectionRestStatement(
                SSimple(SSExp(ExpressionStatement(Some(EAssignmentExpression(AEAssign(
                  UEPostfixExpr(
                    PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                  ),
                  AEEqual,
                  AEConditionalExpression(ConditionalExpression(
                    LOELogicalXorExpression(LXELogicalAndExpression(
                      LAEInclusiveOrExpression(
                        IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                          EERelationalExpression(
                            REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                              MEUnaryExpression(
                                UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("10")))
                              )
                            )))
                          )
                        )))
                      )
                    )),
                    None
                  ))
                )))))),
                Some(SSimple(SSSel(SelectionStatement(
                  EAssignmentExpression(AEConditionalExpression(ConditionalExpression(
                    LOELogicalXorExpression(LXELogicalAndExpression(
                      LAEInclusiveOrExpression(
                        IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(EEEq(
                          EERelationalExpression(
                            REShiftExpression(
                              SEAdditiveExpression(AEMultiplicativeExpression(MEUnaryExpression(
                                UEPostfixExpr(PostPrimaryExpression(
                                  PrimVariableIdentifier(VariableIdentifier("x"))
                                ))
                              )))
                            )
                          ),
                          REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                            MEUnaryExpression(
                              UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("2")))
                            )
                          )))
                        ))))
                      )
                    )),
                    None
                  ))),
                  SelectionRestStatement(
                    SSimple(SSExp(ExpressionStatement(Some(EAssignmentExpression(AEAssign(
                      UEPostfixExpr(
                        PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                      ),
                      AEEqual,
                      AEConditionalExpression(ConditionalExpression(
                        LOELogicalXorExpression(LXELogicalAndExpression(
                          LAEInclusiveOrExpression(
                            IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                              EERelationalExpression(
                                REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                                  MEUnaryExpression(
                                    UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("20")))
                                  )
                                )))
                              )
                            )))
                          )
                        )),
                        None
                      ))
                    )))))),
                    Some(SSimple(SSSel(SelectionStatement(
                      EAssignmentExpression(AEConditionalExpression(ConditionalExpression(
                        LOELogicalXorExpression(LXELogicalAndExpression(
                          LAEInclusiveOrExpression(
                            IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(EEEq(
                              EERelationalExpression(
                                REShiftExpression(
                                  SEAdditiveExpression(AEMultiplicativeExpression(MEUnaryExpression(
                                    UEPostfixExpr(PostPrimaryExpression(
                                      PrimVariableIdentifier(VariableIdentifier("x"))
                                    ))
                                  )))
                                )
                              ),
                              REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                                MEUnaryExpression(
                                  UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("3")))
                                )
                              )))
                            ))))
                          )
                        )),
                        None
                      ))),
                      SelectionRestStatement(
                        SSimple(SSExp(ExpressionStatement(Some(EAssignmentExpression(AEAssign(
                          UEPostfixExpr(
                            PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                          ),
                          AEEqual,
                          AEConditionalExpression(ConditionalExpression(
                            LOELogicalXorExpression(LXELogicalAndExpression(
                              LAEInclusiveOrExpression(
                                IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                                  EERelationalExpression(REShiftExpression(
                                    SEAdditiveExpression(AEMultiplicativeExpression(
                                      MEUnaryExpression(UEPostfixExpr(
                                        PostPrimaryExpression(PrimNumberConstant("30"))
                                      ))
                                    ))
                                  ))
                                )))
                              )
                            )),
                            None
                          ))
                        )))))),
                        Some(SSimple(SSExp(ExpressionStatement(Some(EAssignmentExpression(AEAssign(
                          UEPostfixExpr(
                            PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                          ),
                          AEEqual,
                          AEConditionalExpression(ConditionalExpression(
                            LOELogicalXorExpression(LXELogicalAndExpression(
                              LAEInclusiveOrExpression(
                                IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                                  EERelationalExpression(REShiftExpression(
                                    SEAdditiveExpression(AEMultiplicativeExpression(
                                      MEUnaryExpression(UEPostfixExpr(
                                        PostPrimaryExpression(PrimNumberConstant("40"))
                                      ))
                                    ))
                                  ))
                                )))
                              )
                            )),
                            None
                          ))
                        )))))))
                      )
                    ))))
                  )
                ))))
              )
            ))),
            None
          )))
        ))
      )
    }
  }

  test("fun 6") {
    assertAst(
      """
        |void f() {
        |  float y = 12;
        |  if (x == 1) {
        |    y = 13;
        |  } else if (x == 2)
        |    y = 14;
        |  z = 15;
        |}
        |""".stripMargin
    ) {
      TranslationUnit(
        None,
        EDFunc(FunctionDefinition(
          FunctionPrototype(FDHeader(FunctionHeader(
            FullySpecifiedType(None, TypeSpecifier(TSNAVoid, None)),
            "f"
          ))),
          CompoundStatementNoNewScope(Some(StatementList(
            SSimple(SSExp(ExpressionStatement(Some(EAssignmentExpression(AEAssign(
              UEPostfixExpr(PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("z")))),
              AEEqual,
              AEConditionalExpression(ConditionalExpression(
                LOELogicalXorExpression(LXELogicalAndExpression(
                  LAEInclusiveOrExpression(
                    IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                      EERelationalExpression(
                        REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                          MEUnaryExpression(
                            UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("15")))
                          )
                        )))
                      )
                    )))
                  )
                )),
                None
              ))
            )))))),
            Some(StatementList(
              SSimple(SSSel(SelectionStatement(
                EAssignmentExpression(AEConditionalExpression(ConditionalExpression(
                  LOELogicalXorExpression(LXELogicalAndExpression(
                    LAEInclusiveOrExpression(
                      IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(EEEq(
                        EERelationalExpression(
                          REShiftExpression(
                            SEAdditiveExpression(AEMultiplicativeExpression(MEUnaryExpression(
                              UEPostfixExpr(PostPrimaryExpression(
                                PrimVariableIdentifier(VariableIdentifier("x"))
                              ))
                            )))
                          )
                        ),
                        REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                          MEUnaryExpression(
                            UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("1")))
                          )
                        )))
                      ))))
                    )
                  )),
                  None
                ))),
                SelectionRestStatement(
                  SCompound(CompoundStatement(Some(StatementList(
                    SSimple(SSExp(ExpressionStatement(Some(EAssignmentExpression(AEAssign(
                      UEPostfixExpr(
                        PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                      ),
                      AEEqual,
                      AEConditionalExpression(ConditionalExpression(
                        LOELogicalXorExpression(LXELogicalAndExpression(
                          LAEInclusiveOrExpression(
                            IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                              EERelationalExpression(
                                REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                                  MEUnaryExpression(
                                    UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("13")))
                                  )
                                )))
                              )
                            )))
                          )
                        )),
                        None
                      ))
                    )))))),
                    None
                  )))),
                  Some(SSimple(SSSel(SelectionStatement(
                    EAssignmentExpression(AEConditionalExpression(ConditionalExpression(
                      LOELogicalXorExpression(LXELogicalAndExpression(
                        LAEInclusiveOrExpression(
                          IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(EEEq(
                            EERelationalExpression(
                              REShiftExpression(
                                SEAdditiveExpression(AEMultiplicativeExpression(MEUnaryExpression(
                                  UEPostfixExpr(PostPrimaryExpression(
                                    PrimVariableIdentifier(VariableIdentifier("x"))
                                  ))
                                )))
                              )
                            ),
                            REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                              MEUnaryExpression(
                                UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("2")))
                              )
                            )))
                          ))))
                        )
                      )),
                      None
                    ))),
                    SelectionRestStatement(
                      SSimple(SSExp(ExpressionStatement(Some(EAssignmentExpression(AEAssign(
                        UEPostfixExpr(
                          PostPrimaryExpression(PrimVariableIdentifier(VariableIdentifier("y")))
                        ),
                        AEEqual,
                        AEConditionalExpression(ConditionalExpression(
                          LOELogicalXorExpression(LXELogicalAndExpression(
                            LAEInclusiveOrExpression(
                              IOEExclusiveOrExpression(EOEAndExpression(AEEqualityExpression(
                                EERelationalExpression(
                                  REShiftExpression(SEAdditiveExpression(AEMultiplicativeExpression(
                                    MEUnaryExpression(
                                      UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("14")))
                                    )
                                  )))
                                )
                              )))
                            )
                          )),
                          None
                        ))
                      )))))),
                      None
                    )
                  ))))
                )
              ))),
              Some(StatementList(
                SSimple(SSDecl(DeclarationStatement(DInitDecList(IDL1(SingleDeclaration(
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
                                UEPostfixExpr(PostPrimaryExpression(PrimNumberConstant("12")))
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
            ))
          )))
        ))
      )
    }
  }

}
