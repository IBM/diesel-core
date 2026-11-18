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

import diesel.Dsl._
import diesel.{Context, Dsl}

import Ast._
import diesel.Lexer.Scanner
import diesel.samples.glsl.GStyles.{Constant, Keyword, Type, VariableStyle}

object Glsl extends Dsl with Identifiers with Comments {

  override def identScanner: Scanner = "[a-zA-Z][a-zA-Z0-9]*".r

  override def commentScanners: Seq[Scanner] = Seq(
    "/\\*([^*]|\\*(\\*)*[^*/])*\\*(\\*)*/".r,
    "//[^\n]*(\n|$)".r
  )

  val variable_identifier: Concept[VariableIdentifier]                                = concept
  val primary_expression: Concept[PrimaryExpression]                                  = concept
  val postfix_expression: Concept[PostfixExpression]                                  = concept
  val integer_expression: Concept[IntegerExpression]                                  = concept
  val function_call: Concept[FunctionCall]                                            = concept
  val function_call_or_method: Concept[FunctionCallOrMethod]                          = concept
  val function_call_generic: Concept[FunctionCallGeneric]                             = concept
  val function_call_header_no_parameters: Concept[FunctionCallHeaderNoParameters]     = concept
  val function_call_header_with_parameters: Concept[FunctionCallHeaderWithParameters] = concept
  val function_call_header: Concept[FunctionCallHeader]                               = concept
  val function_identifier: Concept[FunctionIdentifier]                                = concept
  val unary_expression: Concept[UnaryExpression]                                      = concept
  val unary_operator: Concept[UnaryOperator]                                          = concept
  val multiplicative_expression: Concept[MultiplicativeExpression]                    = concept
  val additive_expression: Concept[AdditiveExpression]                                = concept
  val shift_expression: Concept[ShiftExpression]                                      = concept
  val relational_expression: Concept[RelationalExpression]                            = concept
  val equality_expression: Concept[EqualityExpression]                                = concept
  val and_expression: Concept[AndExpression]                                          = concept
  val exclusive_or_expression: Concept[ExclusiveOrExpression]                         = concept
  val inclusive_or_expression: Concept[InclusiveOrExpression]                         = concept
  val logical_and_expression: Concept[LogicalAndExpression]                           = concept
  val logical_xor_expression: Concept[LogicalXorExpression]                           = concept
  val logical_or_expression: Concept[LogicalOrExpression]                             = concept
  val conditional_expression: Concept[ConditionalExpression]                          = concept
  val assignment_expression: Concept[AssignmentExpression]                            = concept
  val assignment_operator: Concept[AssignmentOperator]                                = concept
  val expression: Concept[Expression]                                                 = concept
  val constant_expression: Concept[ConstantExpression]                                = concept
  val declaration: Concept[Declaration]                                               = concept
  val function_prototype: Concept[FunctionPrototype]                                  = concept
  val function_declarator: Concept[FunctionDeclarator]                                = concept
  val function_header_with_parameters: Concept[FunctionHeaderWithParameters]          = concept
  val function_header: Concept[FunctionHeader]                                        = concept
  val parameter_declarator: Concept[ParameterDeclarator]                              = concept
  val parameter_declaration: Concept[ParameterDeclaration]                            = concept
  val parameter_qualifier: Concept[ParameterQualifier]                                = concept
  val parameter_type_specifier: Concept[ParameterTypeSpecifier]                       = concept
  val init_declarator_list: Concept[InitDeclaratorList]                               = concept
  val single_declaration: Concept[SingleDeclaration]                                  = concept
  val fully_specified_type: Concept[FullySpecifiedType]                               = concept
  val type_qualifier: Concept[TypeQualifier]                                          = concept
  val type_specifier: Concept[TypeSpecifier]                                          = concept
  val type_specifier_nonarray: Concept[TypeSpecifierNonArray]                         = concept
  val struct_specifier: Concept[StructSpecifier]                                      = concept
  val struct_declaration_list: Concept[StructDeclarationList]                         = concept
  val struct_declaration: Concept[StructDeclaration]                                  = concept
  val struct_declarator_list: Concept[StructDeclaratorList]                           = concept
  val struct_declarator: Concept[StructDeclarator]                                    = concept
  val initializer: Concept[Initializer]                                               = concept
  val declaration_statement: Concept[DeclarationStatement]                            = concept
  val statement: Concept[Statement]                                                   = concept
  val simple_statement: Concept[SimpleStatement]                                      = concept
  val compound_statement: Concept[CompoundStatement]                                  = concept
  val statement_no_new_scope: Concept[StatementNoNewScope]                            = concept
  val compound_statement_no_new_scope: Concept[CompoundStatementNoNewScope]           = concept
  val statement_list: Concept[StatementList]                                          = concept
  val expression_statement: Concept[ExpressionStatement]                              = concept
  val selection_statement: Concept[SelectionStatement]                                = concept
  val selection_rest_statement: Concept[SelectionRestStatement]                       = concept
  val condition: Concept[Condition]                                                   = concept
  val iteration_statement: Concept[IterationStatement]                                = concept
  val for_init_statement: Concept[ForInitStatement]                                   = concept
  val for_rest_statement: Concept[ForRestStatement]                                   = concept
  val jump_statement: Concept[JumpStatement]                                          = concept
  val translation_unit: Concept[TranslationUnit]                                      = concept
  val external_declaration: Concept[ExternalDeclaration]                              = concept
  val function_definition: Concept[FunctionDefinition]                                = concept

  val FIELD_SELECTION: Concept[String] = concept("\\.[xyzwrgbastpq]+".r, ".x", None) map { (_, t) =>
    t.text.substring(1)
  }

  // variable_identifier

  val s_variable_identifier_1: Syntax[VariableIdentifier] = syntax(variable_identifier)(
    id map {
      case (c, t) =>
        c.setStyle(VariableStyle)
        VariableIdentifier(t.text)
    }
  )

  // primary_expression

  val s_primary_expression_1: Syntax[PrimaryExpression] = syntax(primary_expression)(
    variable_identifier map {
      case (_, v) =>
        PrimVariableIdentifier(v)
    }
  )

  val s_primary_expression_2: Concept[PrimNumberConstant] =
    concept[PrimNumberConstant, PrimaryExpression](
      "(\\d+[\\.]?\\d*|[\\.]\\d+)".r,
      PrimNumberConstant("0"),
      Some(primary_expression)
    )
      .tokenStyle(Constant)
      .map { (_, t) =>
        PrimNumberConstant(t.text)
      }

  val s_primary_expression_3: Instance[PrimaryExpression] =
    instance(primary_expression)("true") map { (c: Context) =>
      c.setStyle(Keyword)
      PrimBoolConstant(true)
    }

  val s_primary_expression_4: Instance[PrimaryExpression] =
    instance(primary_expression)("false") map { (c: Context) =>
      c.setStyle(Keyword)
      PrimBoolConstant(false)
    }

  val s_primary_expression_5: Syntax[PrimaryExpression] = syntax(primary_expression)(
    "(" ~ expression ~ ")" map {
      case (_, (_, e, _)) =>
        PrimParens(e)
    }
  )

  // postfix_expression

  val s_postfix_expression_1: Syntax[PostfixExpression] = syntax(postfix_expression)(
    primary_expression map {
      case (_, e) =>
        PostPrimaryExpression(e)
    }
  )

  val s_postfix_expression_2: Syntax[PostfixExpression] = syntax(postfix_expression)(
    postfix_expression ~ "[" ~ integer_expression ~ "]" map {
      case (_, (e, _, e2, _)) =>
        PostIndexedAccess(e, e2)
    }
  )

  val s_postfix_expression_3: Syntax[PostfixExpression] = syntax(postfix_expression)(
    function_call map {
      case (_, f) =>
        PostFunctionCall(f)
    }
  )

  val s_postfix_expression_4: Syntax[PostfixExpression] = syntax(postfix_expression)(
    postfix_expression ~ FIELD_SELECTION map {
      case (_, (e, fs)) =>
        PostDotSelect(e, fs)
    }
  )

  val s_postfix_expression_5: Syntax[PostfixExpression] = syntax(postfix_expression)(
    postfix_expression ~ "++" map {
      case (_, (e, _)) =>
        PostInc(e)
    }
  )

  val s_postfix_expression_6: Syntax[PostfixExpression] = syntax(postfix_expression)(
    postfix_expression ~ "--" map {
      case (_, (e, _)) =>
        PostDec(e)
    }
  )

  // integer_expression

  val s_integer_expression_1: Syntax[IntegerExpression] = syntax(integer_expression)(
    expression map {
      case (_, e) =>
        IntegerExpression(e)
    }
  )

  // function_call

  val s_function_call_1: Syntax[FunctionCall] = syntax(function_call)(
    function_call_or_method map {
      case (_, f) =>
        FunctionCall(f)
    }
  )

  // function_call_or_method

  val s_function_call_or_method_1: Syntax[FunctionCallOrMethod] = syntax(function_call_or_method)(
    function_call_generic map {
      case (_, f) =>
        FCMGeneric(f)
    }
  )

  val s_function_call_or_method_2: Syntax[FunctionCallOrMethod] = syntax(function_call_or_method)(
    postfix_expression ~ "." ~ function_call_generic map {
      case (_, (e, _, f)) =>
        FCMethod(e, f)
    }
  )

  // function_call_generic

  val s_function_call_generic_1: Syntax[FunctionCallGeneric] = syntax(function_call_generic)(
    function_call_header_with_parameters ~ ")" map {
      case (_, (f, _)) =>
        FCGWithParams(f)
    }
  )

  val s_function_call_generic_2: Syntax[FunctionCallGeneric] = syntax(function_call_generic)(
    function_call_header_no_parameters ~ ")" map {
      case (_, (f, _)) =>
        FCGNoParams(f)
    }
  )

  // function_call_header_no_parameters

  val s_function_call_header_no_parameters_1: Syntax[FunctionCallHeaderNoParameters] =
    syntax(function_call_header_no_parameters)(
      function_call_header ~ "void".? map {
        case (_, (f, v)) =>
          FunctionCallHeaderNoParameters(f, v.isDefined)
      }
    )

  // function_call_header_with_parameters

  val s_function_call_header_with_parameters_1: Syntax[FunctionCallHeaderWithParameters] =
    syntax(function_call_header_with_parameters)(
      function_call_header ~ assignment_expression map {
        case (_, (h, e)) =>
          FCHWP1(h, e)
      }
    )

  val s_function_call_header_with_parameters_2: Syntax[FunctionCallHeaderWithParameters] =
    syntax(function_call_header_with_parameters)(
      function_call_header_with_parameters ~ "," ~ assignment_expression map {
        case (_, (h, _, e)) =>
          FCHWP2(h, e)
      }
    )

  // function_call_header

  val s_function_call_header_1: Syntax[FunctionCallHeader] = syntax(function_call_header)(
    function_identifier ~ "(" map {
      case (_, (i, _)) =>
        FunctionCallHeader(i)
    }
  )

  // function_identifier

  val s_function_identifier_1: Syntax[FunctionIdentifier] = syntax(function_identifier)(
    type_specifier map {
      case (_, t) =>
        FITypeSpecifier(t)
    }
  )

  val s_function_identifier_2: Syntax[FunctionIdentifier] = syntax(function_identifier)(
    id map {
      case (_, t) =>
        FITIdentifier(t.text)
    }
  )

  val s_function_identifier_3: Syntax[FunctionIdentifier] = syntax(function_identifier)(
    FIELD_SELECTION map {
      case (_, s) =>
        FTFieldSelection(s)
    }
  )

  // unary_expression

  val s_unary_expression_1: Syntax[UnaryExpression] = syntax(unary_expression)(
    postfix_expression map {
      case (_, e) =>
        UEPostfixExpr(e)
    }
  )

  val s_unary_expression_2: Syntax[UnaryExpression] = syntax(unary_expression)(
    "++" ~ unary_expression map {
      case (_, (_, e)) =>
        UEInc(e)
    }
  )

  val s_unary_expression_3: Syntax[UnaryExpression] = syntax(unary_expression)(
    "--" ~ unary_expression map {
      case (_, (_, e)) =>
        UEDec(e)
    }
  )

  val s_unary_expression_4: Syntax[UnaryExpression] = syntax(unary_expression)(
    unary_operator ~ unary_expression map {
      case (_, (uo, ue)) =>
        UEUnaryOp(uo, ue)
    }
  )

  // unary_operator

  val i_unary_operator_1: Instance[UnaryOperator] = instance(unary_operator)("+") map (c => UOPlus)

  val i_unary_operator_2: Instance[UnaryOperator] = instance(unary_operator)("-") map (c => UODash)

  val i_unary_operator_3: Instance[UnaryOperator] = instance(unary_operator)("!") map (c => UOBang)

  val i_unary_operator_4: Instance[UnaryOperator] = instance(unary_operator)("~") map (c => UOTilde)

  // multiplicative_expression

  val s_multiplicative_expression_1: Syntax[MultiplicativeExpression] =
    syntax(multiplicative_expression)(
      unary_expression map {
        case (_, e) =>
          MEUnaryExpression(e)
      }
    )

  val s_multiplicative_expression_2: Syntax[MultiplicativeExpression] =
    syntax(multiplicative_expression)(
      multiplicative_expression ~ "*" ~ unary_expression map {
        case (_, (l, _, r)) =>
          MEMul(l, r)
      }
    )

  val s_multiplicative_expression_3: Syntax[MultiplicativeExpression] =
    syntax(multiplicative_expression)(
      multiplicative_expression ~ "/" ~ unary_expression map {
        case (_, (l, _, r)) =>
          MEDiv(l, r)
      }
    )

  val s_multiplicative_expression_4: Syntax[MultiplicativeExpression] =
    syntax(multiplicative_expression)(
      multiplicative_expression ~ "%" ~ unary_expression map {
        case (_, (l, _, r)) =>
          MEMod(l, r)
      }
    )

  // additive_expression

  val s_additive_expression_1: Syntax[AdditiveExpression] = syntax(additive_expression)(
    multiplicative_expression map {
      case (_, e) =>
        AEMultiplicativeExpression(e)
    }
  )

  val s_additive_expression_2: Syntax[AdditiveExpression] = syntax(additive_expression)(
    additive_expression ~ "+" ~ multiplicative_expression map {
      case (_, (l, _, r)) =>
        AEAdd(l, r)
    }
  )

  val s_additive_expression_3: Syntax[AdditiveExpression] = syntax(additive_expression)(
    additive_expression ~ "-" ~ multiplicative_expression map {
      case (_, (l, _, r)) =>
        AESub(l, r)
    }
  )

  // shift_expression

  val s_shift_expression_1: Syntax[ShiftExpression] = syntax(shift_expression)(
    additive_expression map {
      case (_, e) =>
        SEAdditiveExpression(e)
    }
  )

  val s_shift_expression_2: Syntax[ShiftExpression] = syntax(shift_expression)(
    shift_expression ~ "<<" ~ additive_expression map {
      case (_, (l, _, r)) =>
        SELeft(l, r)
    }
  )

  val s_shift_expression_3: Syntax[ShiftExpression] = syntax(shift_expression)(
    shift_expression ~ ">>" ~ additive_expression map {
      case (_, (l, _, r)) =>
        SERight(l, r)
    }
  )

  // relational_expression

  val s_relational_expression_1: Syntax[RelationalExpression] = syntax(relational_expression)(
    shift_expression map {
      case (_, e) =>
        REShiftExpression(e)
    }
  )

  val s_relational_expression_2: Syntax[RelationalExpression] = syntax(relational_expression)(
    relational_expression ~ "<" ~ shift_expression map {
      case (_, (l, _, r)) =>
        RELt(l, r)
    }
  )

  val s_relational_expression_3: Syntax[RelationalExpression] = syntax(relational_expression)(
    relational_expression ~ ">" ~ shift_expression map {
      case (_, (l, _, r)) =>
        REGt(l, r)
    }
  )

  val s_relational_expression_4: Syntax[RelationalExpression] = syntax(relational_expression)(
    relational_expression ~ "<=" ~ shift_expression map {
      case (_, (l, _, r)) =>
        RELte(l, r)
    }
  )

  val s_relational_expression_5: Syntax[RelationalExpression] = syntax(relational_expression)(
    relational_expression ~ ">=" ~ shift_expression map {
      case (_, (l, _, r)) =>
        REGte(l, r)
    }
  )

  // equality_expression

  val s_equality_expression_1: Syntax[EqualityExpression] = syntax(equality_expression)(
    relational_expression map {
      case (_, r) =>
        EERelationalExpression(r)
    }
  )

  val s_equality_expression_2: Syntax[EqualityExpression] = syntax(equality_expression)(
    equality_expression ~ "==" ~ relational_expression map {
      case (_, (l, _, r)) =>
        EEEq(l, r)
    }
  )

  val s_equality_expression_3: Syntax[EqualityExpression] = syntax(equality_expression)(
    equality_expression ~ "!=" ~ relational_expression map {
      case (_, (l, _, r)) =>
        EENeq(l, r)
    }
  )

  // and_expression

  val s_and_expression_1: Syntax[AndExpression] = syntax(and_expression)(
    equality_expression map {
      case (_, e) =>
        AEEqualityExpression(e)
    }
  )

  val s_and_expression_2: Syntax[AndExpression] = syntax(and_expression)(
    and_expression ~ "&" ~ equality_expression map {
      case (_, (l, _, r)) =>
        AEAmp(l, r)
    }
  )

  // exclusive_or_expression

  val s_exclusive_or_expression_1: Syntax[ExclusiveOrExpression] = syntax(exclusive_or_expression)(
    and_expression map {
      case (_, e) =>
        EOEAndExpression(e)
    }
  )

  val s_exclusive_or_expression_2: Syntax[ExclusiveOrExpression] = syntax(exclusive_or_expression)(
    exclusive_or_expression ~ "^" ~ and_expression map {
      case (_, (l, _, r)) =>
        EOXor(l, r)
    }
  )

  // inclusive_or_expression

  val s_inclusive_or_expression_1: Syntax[InclusiveOrExpression] = syntax(inclusive_or_expression)(
    exclusive_or_expression map {
      case (_, e) =>
        IOEExclusiveOrExpression(e)
    }
  )

  val s_inclusive_or_expression_2: Syntax[InclusiveOrExpression] = syntax(inclusive_or_expression)(
    inclusive_or_expression ~ "|" ~ exclusive_or_expression map {
      case (_, (l, _, r)) =>
        IOEIor(l, r)
    }
  )

  // logical_and_expression

  val s_logical_and_expression_1: Syntax[LogicalAndExpression] = syntax(logical_and_expression)(
    inclusive_or_expression map {
      case (_, e) =>
        LAEInclusiveOrExpression(e)
    }
  )

  val s_logical_and_expression_2: Syntax[LogicalAndExpression] = syntax(logical_and_expression)(
    logical_and_expression ~ "&&" ~ inclusive_or_expression map {
      case (_, (l, _, r)) =>
        LAEAnd(l, r)
    }
  )

  // logical_xor_expression

  val s_logical_xor_expression_1: Syntax[LogicalXorExpression] = syntax(logical_xor_expression)(
    logical_and_expression map {
      case (_, e) =>
        LXELogicalAndExpression(e)
    }
  )

  val s_logical_xor_expression_2: Syntax[LogicalXorExpression] = syntax(logical_xor_expression)(
    logical_xor_expression ~ "^^" ~ logical_and_expression map {
      case (_, (l, _, r)) =>
        LXEXor(l, r)
    }
  )

  // logical_or_expression

  val s_logical_or_expression_1: Syntax[LogicalOrExpression] = syntax(logical_or_expression)(
    logical_xor_expression map {
      case (_, e) =>
        LOELogicalXorExpression(e)
    }
  )

  val s_logical_or_expression_2: Syntax[LogicalOrExpression] = syntax(logical_or_expression)(
    logical_or_expression ~ "||" ~ logical_xor_expression map {
      case (_, (l, _, r)) =>
        LOEOr(l, r)
    }
  )

  // conditional_expression

  val s_conditional_expression_1: Syntax[ConditionalExpression] = syntax(conditional_expression)(
    logical_or_expression map {
      case (_, e) =>
        ConditionalExpression(e, None)
    }
  )

  val s_conditional_expression_2: Syntax[ConditionalExpression] = syntax(conditional_expression)(
    logical_or_expression ~ "?" ~ expression ~ ":" ~ assignment_expression map {
      case (_, (l, _, e, _, ae)) =>
        ConditionalExpression(l, Some(Tuple2(e, ae)))
    }
  )

  // assignment_expression

  val s_assignment_expression_1: Syntax[AssignmentExpression] = syntax(assignment_expression)(
    conditional_expression map {
      case (_, e) =>
        AEConditionalExpression(e)
    }
  )

  val s_assignment_expression_2: Syntax[AssignmentExpression] = syntax(assignment_expression)(
    unary_expression ~ assignment_operator ~ assignment_expression map {
      case (_, (ue, ao, ae)) =>
        AEAssign(ue, ao, ae)
    }
  )

  // assignment_operator

  val i_assignment_operator_1: Instance[AssignmentOperator] =
    instance(assignment_operator)("=") map (_ => AEEqual)
  val i_assignment_operator_2: Instance[AssignmentOperator] =
    instance(assignment_operator)("*=") map (_ => AEMulAssign)
  val i_assignment_operator_3: Instance[AssignmentOperator] =
    instance(assignment_operator)("/=") map (_ => AEDivAssign)
  val i_assignment_operator_4: Instance[AssignmentOperator] =
    instance(assignment_operator)("%=") map (_ => AEModAssign)
  val i_assignment_operator_5: Instance[AssignmentOperator] =
    instance(assignment_operator)("+=") map (_ => AEAddAssign)
  val i_assignment_operator_6: Instance[AssignmentOperator] =
    instance(assignment_operator)("-=") map (_ => AESubAssign)

  // expression

  val s_expression_1: Syntax[Expression] = syntax(expression)(
    assignment_expression map {
      case (_, e) =>
        EAssignmentExpression(e)
    }
  )

  val s_expression_2: Syntax[Expression] = syntax(expression)(
    expression ~ "," ~ assignment_expression map {
      case (_, (e, _, e2)) =>
        EAComma(e, e2)
    }
  )

  // constant_expression

  val s_constant_expression_1: Syntax[ConstantExpression] = syntax(constant_expression)(
    conditional_expression map {
      case (_, e) =>
        ConstantExpression(e)
    }
  )

  // declaration

  val s_declaration_1: Syntax[Declaration] = syntax(declaration)(
    function_prototype ~ ";" map {
      case (_, (f, _)) =>
        DFProto(f)
    }
  )

  val s_declaration_2: Syntax[Declaration] = syntax(declaration)(
    init_declarator_list ~ ";" map {
      case (_, (idl, _)) =>
        DInitDecList(idl)
    }
  )

  // function_prototype

  val s_function_prototype_1: Syntax[FunctionPrototype] = syntax(function_prototype)(
    function_declarator ~ ")" map {
      case (_, (f, _)) =>
        FunctionPrototype(f)
    }
  )

  // function_declarator

  val s_function_declarator_1: Syntax[FunctionDeclarator] = syntax(function_declarator)(
    function_header map {
      case (_, e) =>
        FDHeader(e)
    }
  )

  val s_function_declarator_2: Syntax[FunctionDeclarator] = syntax(function_declarator)(
    function_header_with_parameters map {
      case (_, e) =>
        FDHeaderWithParams(e)
    }
  )

  // function_header_with_parameters

  val s_function_header_with_parameters_1: Syntax[FunctionHeaderWithParameters] =
    syntax(function_header_with_parameters)(
      function_header ~ parameter_declaration map {
        case (_, (f, p)) =>
          FHWP1(f, p)
      }
    )

  val s_function_header_with_parameters_2: Syntax[FunctionHeaderWithParameters] =
    syntax(function_header_with_parameters)(
      function_header_with_parameters ~ "," ~ parameter_declaration map {
        case (_, (f, _, p)) =>
          FHWP2(f, p)
      }
    )

  // function_header

  val s_function_header_1: Syntax[FunctionHeader] = syntax(function_header)(
    fully_specified_type ~ id ~ "(" map {
      case (_, (t, id, _)) =>
        FunctionHeader(t, id.text)
    }
  )

  // parameter_declarator

  val s_parameter_declarator_1: Syntax[ParameterDeclarator] = syntax(parameter_declarator)(
    type_specifier ~ id map {
      case (_, (t, i)) =>
        ParameterDeclarator(t, i.text, None)
    }
  )

  val s_parameter_declarator_2: Syntax[ParameterDeclarator] = syntax(parameter_declarator)(
    type_specifier ~ id ~ "{" ~ constant_expression ~ "}" map {
      case (_, (t, i, _, e, _)) =>
        ParameterDeclarator(t, i.text, Some(e))
    }
  )

  // parameter_declaration

  val s_parameter_declaration_1: Syntax[ParameterDeclaration] = syntax(parameter_declaration)(
    type_qualifier ~ parameter_qualifier.? ~ parameter_declarator map {
      case (_, (tq, pq, pd)) =>
        PD1(tq, pq, pd)
    }
  )

  val s_parameter_declaration_2: Syntax[ParameterDeclaration] = syntax(parameter_declaration)(
    parameter_qualifier.? ~ parameter_declarator map {
      case (_, (pq, pd)) =>
        PD2(pq, pd)
    }
  )

  val s_parameter_declaration_3: Syntax[ParameterDeclaration] = syntax(parameter_declaration)(
    type_qualifier ~ parameter_qualifier.? ~ parameter_type_specifier map {
      case (_, (tq, pq, pts)) =>
        PD3(tq, pq, pts)
    }
  )

  val s_parameter_declaration_4: Syntax[ParameterDeclaration] = syntax(parameter_declaration)(
    parameter_qualifier.? ~ parameter_type_specifier map {
      case (_, (pq, pts)) =>
        PD4(pq, pts)
    }
  )

  // parameter_qualifier

  val i_parameter_qualifier_1: Instance[ParameterQualifier] =
    instance(parameter_qualifier)("in") map (_ => PQIn)
  val i_parameter_qualifier_2: Instance[ParameterQualifier] =
    instance(parameter_qualifier)("out") map (_ => PQOut)
  val i_parameter_qualifier_3: Instance[ParameterQualifier] =
    instance(parameter_qualifier)("inout") map (_ => PQInOut)

  // parameter_type_specifier

  val s_parameter_type_specifier_1: Syntax[ParameterTypeSpecifier] =
    syntax(parameter_type_specifier)(
      type_specifier map {
        case (_, x) =>
          ParameterTypeSpecifier(x)
      }
    )

  // init_declarator_list

  val s_init_declarator_list_1: Syntax[InitDeclaratorList] = syntax(init_declarator_list)(
    single_declaration map {
      case (_, x) =>
        IDL1(x)
    }
  )

  val s_init_declarator_list_2: Syntax[InitDeclaratorList] = syntax(init_declarator_list)(
    init_declarator_list ~ "," ~ id map {
      case (_, (i, _, id)) =>
        IDL2(i, id.text, None, None)
    }
  )

  val s_init_declarator_list_3: Syntax[InitDeclaratorList] = syntax(init_declarator_list)(
    init_declarator_list ~ "," ~ id ~ "{" ~ "}" map {
      case (_, (idl, _, id, _, _)) =>
        IDL2(idl, id.text, None, None)
    }
  )

  val s_init_declarator_list_4: Syntax[InitDeclaratorList] = syntax(init_declarator_list)(
    init_declarator_list ~ "," ~ id ~ "{" ~ constant_expression ~ "}" map {
      case (_, (idl, _, id, _, e, _)) =>
        IDL2(idl, id.text, Some(e), None)
    }
  )

  val s_init_declarator_list_5: Syntax[InitDeclaratorList] = syntax(init_declarator_list)(
    init_declarator_list ~ "," ~ id ~ "{" ~ "}" ~ "=" ~ initializer map {
      case (_, (idl, _, id, _, _, _, i)) =>
        IDL2(idl, id.text, None, Some(i))
    }
  )

  val s_init_declarator_list_6: Syntax[InitDeclaratorList] = syntax(init_declarator_list)(
    init_declarator_list ~ "," ~ id ~ "{" ~ constant_expression ~ "}" ~ "=" ~ initializer map {
      case (_, (idl, _, id, _, e, _, _, i)) =>
        IDL2(idl, id.text, Some(e), Some(i))
    }
  )

  val s_init_declarator_list_7: Syntax[InitDeclaratorList] = syntax(init_declarator_list)(
    init_declarator_list ~ "," ~ id ~ "=" ~ initializer map {
      case (_, (idl, _, id, _, i)) =>
        IDL2(idl, id.text, None, Some(i))
    }
  )

  // single_declaration

  val s_single_declaration_1: Syntax[SingleDeclaration] = syntax(single_declaration)(
    fully_specified_type map {
      case (_, x) =>
        SingleDeclaration(x, None, None, None)
    }
  )

  val s_single_declaration_2: Syntax[SingleDeclaration] = syntax(single_declaration)(
    fully_specified_type ~ id map {
      case (_, (x, i)) =>
        SingleDeclaration(x, Some(i.text), None, None)
    }
  )

  val s_single_declaration_3: Syntax[SingleDeclaration] = syntax(single_declaration)(
    fully_specified_type ~ id ~ "{" ~ "}" map {
      case (_, (x, i, _, _)) =>
        SingleDeclaration(x, Some(i.text), None, None)
    }
  )

  val s_single_declaration_4: Syntax[SingleDeclaration] = syntax(single_declaration)(
    fully_specified_type ~ id ~ "{" ~ constant_expression ~ "}" map {
      case (_, (x, i, _, e, _)) =>
        SingleDeclaration(x, Some(i.text), Some(e), None)
    }
  )

  val s_single_declaration_5: Syntax[SingleDeclaration] = syntax(single_declaration)(
    fully_specified_type ~ id ~ "{" ~ "}" ~ "=" ~ initializer map {
      case (_, (x, id, _, _, _, i)) =>
        SingleDeclaration(x, Some(id.text), None, Some(i))
    }
  )

  val s_single_declaration_6: Syntax[SingleDeclaration] = syntax(single_declaration)(
    fully_specified_type ~ id ~ "{" ~ constant_expression ~ "}" ~ "=" ~ initializer map {
      case (_, (x, id, _, e, _, _, i)) =>
        SingleDeclaration(x, Some(id.text), Some(e), Some(i))
    }
  )

  val s_single_declaration_7: Syntax[SingleDeclaration] = syntax(single_declaration)(
    fully_specified_type ~ id ~ "=" ~ initializer map {
      case (_, (x, id, _, i)) =>
        SingleDeclaration(x, Some(id.text), None, Some(i))
    }
  )

  // fully_specified_type

  val s_fully_specified_type_1: Syntax[FullySpecifiedType] = syntax(fully_specified_type)(
    type_specifier map {
      case (_, x) =>
        FullySpecifiedType(None, x)
    }
  )

  val s_fully_specified_type_2: Syntax[FullySpecifiedType] = syntax(fully_specified_type)(
    type_qualifier ~ type_specifier map {
      case (_, (tq, ts)) =>
        FullySpecifiedType(Some(tq), ts)
    }
  )

  // type_qualifier

  val i_type_qualifier_const: Instance[TypeQualifier]     =
    instance(type_qualifier)("const") map (_ => TQConst)
  val i_type_qualifier_attribute: Instance[TypeQualifier] =
    instance(type_qualifier)("attribute") map (_ => TQAttribute)
  val i_type_qualifier_varying: Instance[TypeQualifier]   =
    instance(type_qualifier)("varying") map (_ => TQVarying)
  val i_type_qualifier_centroid: Instance[TypeQualifier]  =
    instance(type_qualifier)("centroid") map (_ => TQCentroid)
  val i_type_qualifier_uniform: Instance[TypeQualifier]   =
    instance(type_qualifier)("uniform") map (_ => TQUniform)

  // type_specifier

  val s_type_specifier_1: Syntax[TypeSpecifier] = syntax(type_specifier)(
    type_specifier_nonarray map {
      case (_, t) =>
        TypeSpecifier(t, None)
    }
  )

  val s_type_specifier_2: Syntax[TypeSpecifier] = syntax(type_specifier)(
    type_specifier_nonarray ~ "[" ~ constant_expression ~ "]" map {
      case (_, (t, _, e, _)) =>
        TypeSpecifier(t, Some(e))
    }
  )

  // type_specifier_nonarray

  val i_type_specifier_nonarray_void: Instance[TypeSpecifierNonArray]  =
    instance(type_specifier_nonarray)("void") tokenStyle Type map { _ => TSNAVoid }
  val i_type_specifier_nonarray_float: Instance[TypeSpecifierNonArray] =
    instance(type_specifier_nonarray)("float") tokenStyle Type map { _ => TSNAFloat }
  val i_type_specifier_nonarray_int: Instance[TypeSpecifierNonArray]   =
    instance(type_specifier_nonarray)("int") tokenStyle Type map { _ => TSNAInt }
  val i_type_specifier_nonarray_bool: Instance[TypeSpecifierNonArray]  =
    instance(type_specifier_nonarray)("bool") tokenStyle Type map { _ => TSNABool }
  val i_type_specifier_nonarray_vec2: Instance[TypeSpecifierNonArray]  =
    instance(type_specifier_nonarray)("vec2") tokenStyle Type map { _ => TSNAVec2 }
  val i_type_specifier_nonarray_vec3: Instance[TypeSpecifierNonArray]  =
    instance(type_specifier_nonarray)("vec3") tokenStyle Type map { _ => TSNAVec3 }
  val i_type_specifier_nonarray_vec4: Instance[TypeSpecifierNonArray]  =
    instance(type_specifier_nonarray)("vec4") tokenStyle Type map { _ => TSNAVec4 }

  // struct_specifier

  val s_struct_specifier_1: Syntax[StructSpecifier] = syntax(struct_specifier)(
    "struct" ~ id.? ~ "{" ~ struct_declaration_list ~ "}" map {
      case (_, (_, id, _, s, _)) =>
        StructSpecifier(id.map(_.text), s)
    }
  )

  // struct_declaration_list

  val s_struct_declaration_list_1: Syntax[StructDeclarationList] = syntax(struct_declaration_list)(
    struct_declaration_list.? ~ struct_declaration map {
      case (_, (l, d)) =>
        StructDeclarationList(l, d)
    }
  )

  // struct_declaration

  val s_struct_declaration_1: Syntax[StructDeclaration] = syntax(struct_declaration)(
    type_specifier ~ struct_declarator_list ~ ";" map {
      case (_, (ts, sdl, _)) =>
        StructDeclaration(ts, sdl)
    }
  )

  // struct_declarator_list

  val s_struct_declarator_list_1: Syntax[StructDeclaratorList] = syntax(struct_declarator_list)(
    (struct_declarator_list ~ ",").? ~ struct_declarator map {
      case (_, (sdl, sd)) =>
        StructDeclaratorList(sdl.map(_._1), sd)
    }
  )

  // struct_declarator

  val s_struct_declarator_1: Syntax[StructDeclarator] = syntax(struct_declarator)(
    id ~ ("[" ~ constant_expression ~ "]").? map {
      case (_, (id, ce)) =>
        StructDeclarator(id.text, ce.map(_._2))
    }
  )

  // initializer

  val s_initializer_1: Syntax[Initializer] = syntax(initializer)(
    assignment_expression map {
      case (_, x) =>
        Initializer(x)
    }
  )

  // declaration_statement

  val s_declaration_statement_1: Syntax[DeclarationStatement] = syntax(declaration_statement)(
    declaration map {
      case (_, x) =>
        DeclarationStatement(x)
    }
  )

  // statement

  val s_statement_1: Syntax[Statement] = syntax(statement)(
    compound_statement | simple_statement map {
      case (_, (Left(s)))  =>
        SCompound(s)
      case (_, (Right(s))) =>
        SSimple(s)
    }
  )

  // simple_statement

  val s_simple_statement_1: Syntax[SimpleStatement] = syntax(simple_statement)(
    declaration_statement map {
      case (_, x) =>
        SSDecl(x)
    }
  )

  val s_simple_statement_2: Syntax[SimpleStatement] = syntax(simple_statement)(
    expression_statement map {
      case (_, x) =>
        SSExp(x)
    }
  )

  val s_simple_statement_3: Syntax[SimpleStatement] = syntax(simple_statement)(
    selection_statement map {
      case (_, x) =>
        SSSel(x)
    }
  )

  val s_simple_statement_4: Syntax[SimpleStatement] = syntax(simple_statement)(
    iteration_statement map {
      case (_, x) =>
        SSIter(x)
    }
  )

  val s_simple_statement_5: Syntax[SimpleStatement] = syntax(simple_statement)(
    jump_statement map {
      case (_, x) =>
        SSJump(x)
    }
  )

  // compound_statement

  val s_compound_statement_1: Syntax[CompoundStatement] = syntax(compound_statement)(
    "{" ~ statement_list.? ~ "}" map {
      case (_, (_, l, _)) =>
        CompoundStatement(l)
    }
  )

  // statement_no_new_scope

  val s_statement_no_new_scope_1: Syntax[StatementNoNewScope] = syntax(statement_no_new_scope)(
    compound_statement_no_new_scope | simple_statement map {
      case (_, (Left(s)))  =>
        SNNSCompound(s)
      case (_, (Right(s))) =>
        SNSSSimple(s)

    }
  )

  // compound_statement_no_new_scope

  val s_compound_statement_no_new_scope_1: Syntax[CompoundStatementNoNewScope] =
    syntax(compound_statement_no_new_scope)(
      "{" ~ statement_list.? ~ "}" map {
        case (_, (_, l, _)) =>
          CompoundStatementNoNewScope(l)
      }
    )

  // statement_list

  val s_statement_list_1: Syntax[StatementList] = syntax(statement_list)(
    statement_list.? ~ statement map {
      case (_, (sl, st)) =>
        StatementList(st, sl)
    }
  )

  // expression_statement

  val s_expression_statement_1: Syntax[ExpressionStatement] = syntax(expression_statement)(
    expression.? ~ ";" map {
      case (_, (e, _)) =>
        ExpressionStatement(e)
    }
  )

  // selection_statement

  val s_selection_statement_1: Syntax[SelectionStatement] = syntax(selection_statement)(
    "if".tokenStyle(Keyword) ~ "(" ~ expression ~ ")" ~ selection_rest_statement map {
      case (_, (_, _, e, _, r)) =>
        SelectionStatement(e, r)
    }
  )

  // selection_rest_statement

  val s_selection_rest_statement_1: Syntax[SelectionRestStatement] =
    syntax(selection_rest_statement)(
      statement ~ ("else" ~ statement).? map {
        case (_, (s, e)) =>
          SelectionRestStatement(s, e.map(_._2))
      }
    )

  // condition

  val s_condition_1: Syntax[Condition] = syntax(condition)(
    expression map {
      case (_, x) =>
        CExpression(x)
    }
  )

  val s_condition_2: Syntax[Condition] = syntax(condition)(
    fully_specified_type ~ id ~ "=" ~ initializer map {
      case (_, (t, id, _, i)) =>
        CTypeInit(t, id.text, i);
    }
  )

  // iteration_statement

  val s_iteration_statement_1: Syntax[IterationStatement] = syntax(iteration_statement)(
    "while" ~ "(" ~ condition ~ ")" ~ statement_no_new_scope map {
      case (_, (_, _, c, _, s)) =>
        ISWhile(c, s)
    }
  )

  val s_iteration_statement_2: Syntax[IterationStatement] = syntax(iteration_statement)(
    "do" ~ statement ~ "while" ~ "(" ~ expression ~ ")" ~ ";" map {
      case (_, (_, s, _, _, e, _, _)) =>
        ISDoWhile(s, e)
    }
  )

  val s_iteration_statement_3: Syntax[IterationStatement] = syntax(iteration_statement)(
    "for" ~ "(" ~ for_init_statement ~ for_rest_statement ~ ")" ~ statement_no_new_scope map {
      case (_, (_, _, fi, fr, _, s)) =>
        ISFor(fi, fr, s)
    }
  )

  // for_init_statement

  val s_for_init_statement_1: Syntax[ForInitStatement] = syntax(for_init_statement)(
    expression_statement | declaration_statement map {
      case (_, Left(e))  =>
        FIExpr(e)
      case (_, Right(d)) =>
        FIDecl(d)
    }
  )

  // conditionopt

//    val s_conditionopt_1: Syntax[Expr] = syntax(conditionopt)(
//      condition map {
//        case _ => Dummy
//      }
//    )

  // for_rest_statement

  val s_for_rest_statement: Syntax[ForRestStatement] = syntax(for_rest_statement)(
    condition.? ~ ";" ~ expression map {
      case (_, (c, _, e)) =>
        ForRestStatement(c, e)
    }
  )

  // jump_statement

  val s_jump_statement_1: Syntax[JumpStatement] = syntax(jump_statement)(
    "continue" ~ ";" map {
      case _ => JSContinue
    }
  )

  val s_jump_statement_2: Syntax[JumpStatement] = syntax(jump_statement)(
    "break" ~ ";" map {
      case _ => JSBreak
    }
  )

  val s_jump_statement_3: Syntax[JumpStatement] = syntax(jump_statement)(
    "return" ~ expression.? ~ ";" map {
      case (_, (_, e, _)) =>
        JSReturn(e)
    }
  )

  val s_jump_statement_4: Syntax[JumpStatement] = syntax(jump_statement)(
    "discard" ~ ";" map {
      case _ => JSDiscard
    }
  )

  // translation_unit

  val s_translation_unit_1: Syntax[TranslationUnit] = syntax(translation_unit)(
    translation_unit.? ~ external_declaration map {
      case (_, (tu, ed)) =>
        TranslationUnit(tu, ed)
    }
  )

  // external_declaration

  val s_external_declaration_1: Syntax[ExternalDeclaration] = syntax(external_declaration)(
    function_definition | declaration map {
      case (_, (Left(f)))  =>
        EDFunc(f)
      case (_, (Right(d))) =>
        EDDec(d)
    }
  )

  // function_definition

  val s_function_definition_1: Syntax[FunctionDefinition] = syntax(function_definition)(
    function_prototype ~ compound_statement_no_new_scope map {
      case (_, (p, c)) =>
        FunctionDefinition(p, c)
    }
  )

  // axiom

  val a: Axiom[TranslationUnit] = axiom(translation_unit)

}
