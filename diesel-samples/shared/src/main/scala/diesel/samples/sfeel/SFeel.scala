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

package diesel.samples.sfeel

import diesel.{Dsl, Lexer}
import Ast._
import diesel.Dsl.{Axiom, Concept, Identifiers, Instance, Syntax}

object SFeel extends Dsl with Identifiers {

  override def identScanner: Lexer.Scanner = "[a-z]+".r

  val expression: Concept[Expression]                              = concept
  val simple_expression: Concept[SimpleExpression]                 = concept
  val simple_expressions: Concept[Seq[SimpleExpression]]           = concept
  val arithmetic_expression: Concept[ArithmeticExpression]         = concept
  val simple_value: Concept[SimpleValue]                           = concept
  val comparison: Concept[Comparison]                              = concept
  val simple_positive_unary_test: Concept[SimplePositiveUnaryTest] = concept
  val endpoint: Concept[Endpoint]                                  = concept
  val interval: Concept[Interval]                                  = concept
  val qualifiedName: Concept[QualifiedName]                        = concept
  val name: Concept[Name]                                          = concept
  val simple_unary_tests: Concept[SimpleUnaryTests]                = concept
  val qualified_name: Concept[QualifiedName]                       = concept
  val simple_literal: Concept[SimpleLiteral]                       = concept
  val numeric_literal: Concept[NumericLiteral]                     =
    concept("[0-9]+".r, NumericLiteral(0)) map ((_, t) => NumericLiteral(t.text.toInt))
  val string_literal: Concept[StringLiteral]                       =
    concept("\"[a-z]*\"".r, StringLiteral("")) map ((_, t) =>
      StringLiteral(t.text.drop(1).dropRight(1))
    )
  val boolean_literal: Concept[BooleanLiteral]                     = concept

  val s_expression: Syntax[Expression] = syntax(expression)(
    simple_expression map {
      case (_, e) =>
        Expression(e)
    }
  )

  val s_simple_expr_1: Syntax[SimpleExpression] = syntax(simple_expression)(
    arithmetic_expression map {
      case (_, a) =>
        SEArithmeticExpression(a)
    }
  )

  val s_simple_expr_2: Syntax[SimpleExpression] = syntax(simple_expression)(
    simple_value map {
      case (_, sv) =>
        SESimpleValue(sv)
    }
  )

  val s_simple_expr_3: Syntax[SimpleExpression] = syntax(simple_expression)(
    comparison map {
      case (_, c) =>
        SEComparison(c)
    }
  )

  val s_simple_expressions: Syntax[Seq[SimpleExpression]] = syntax(simple_expressions)(
    simple_expression ~ ("," ~ simple_expression).rep(true) map {
      case (_, (se1, rest)) =>
        Seq(se1) ++ rest.map(_._2)
    }
  )

  val s_simple_positive_unary_test_1: Syntax[SimplePositiveUnaryTest] =
    syntax(simple_positive_unary_test)(
      ("<" | "<=" | ">" | ">=").? ~ endpoint map {
        case (_, (optChars, e)) =>
          SPUTEndpoint(Some("TODO"), e)
      }
    )

  val s_simple_positive_unary_test_2: Syntax[SimplePositiveUnaryTest] =
    syntax(simple_positive_unary_test)(
      interval map {
        case (_, i) =>
          SPUTInterval(i)
      }
    )

  val s_interval: Syntax[Interval] = syntax(interval)(
    "[" ~ endpoint ~ ".." ~ endpoint ~ "]" map {
      case (_, (start, e1, _, e2, end)) =>
        Interval(start.text, e1, e2, end.text)
    }
  )

  val s_simple_positive_unary_tests: Syntax[Seq[SimplePositiveUnaryTest]] = syntax(
    simple_positive_unary_test ~ ("," ~ simple_positive_unary_test).rep(true) map {
      case (_, (s, rest)) =>
        Seq(s) ++ rest.map(_._2)
    }
  )

  val s_simple_unary_tests_1: Syntax[SimpleUnaryTests] = syntax(simple_unary_tests)(
    s_simple_positive_unary_tests map {
      case (_, xs) =>
        SUTPositive(xs)
    }
  )

  val s_simple_unary_tests_2: Syntax[SimpleUnaryTests] = syntax(simple_unary_tests)(
    "not" ~ "(" ~ s_simple_positive_unary_tests ~ ")" map {
      case (_, (_, _, xs, _)) =>
        SUTPositiveNot(xs)
    }
  )

  val i_simple_unary_tests_3: Instance[SimpleUnaryTests] = instance(simple_unary_tests)("-") map {
    _ =>
      SUTMinus
  }

  val s_endpoint: Syntax[Endpoint] = syntax(endpoint)(
    simple_value map {
      case (_, v) =>
        Endpoint(v)
    }
  )

  val s_simple_value: Syntax[SimpleValue] = syntax(simple_value)(
    qualified_name | simple_literal map {
      case (_, Left(qn))  =>
        SVQualifiedName(qn)
      case (_, Right(sl)) =>
        SVSimpleLiteral(sl)
    }
  )

  val s_qualified_name: Syntax[QualifiedName] = syntax(qualified_name)(
    name ~ ("." ~ name).rep(true) map {
      case (_, (n, rest)) =>
        QualifiedName(Seq(n) ++ rest.map(_._2))
    }
  )

  val s_addition: Syntax[ArithmeticExpression] = syntax(arithmetic_expression)(
    expression ~ "+".leftAssoc(10) ~ expression map {
      case (_, (l, _, r)) =>
        Addition(l, r)
    }
  )

////  val s_subtraction: Syntax[Subtraction] = syntax(
////    expression ~ "-" ~ expression map {
////      case (_,(l,_,r)) =>
////        Subtraction(l,r)
////    }
////  )
////
////  val s_multiplication: Syntax[Multiplication] = syntax(
////    expression ~ "*" ~ expression map {
////      case (_,(l,_,r)) =>
////        Multiplication(l,r)
////    }
////  )
////
////  val s_division: Syntax[Division] = syntax(
////    expression ~ "/" ~ expression map {
////      case (_,(l,_,r)) =>
////        Division(l,r)
////    }
////  )
//
//  val s_exponentiation: Syntax[Exponentiation] = syntax(
//    expression ~ "**" ~ expression map {
//      case (_,(l,_,r)) =>
//        Exponentiation(l,r)
//    }
//  )
//
//  val s_arithmetic_negation: Syntax[ArithmeticNegation] = syntax(
//    "-" ~ expression map {
//      case (_,(_,e)) =>
//        ArithmeticNegation(e)
//    }
//  )

  val s_name: Syntax[Name] = syntax(name)(
    id map {
      case (_, i) =>
        Name(i.text)
    }
  )

  val s_simple_literal_1: Syntax[SimpleLiteral] = syntax(simple_literal)(
    numeric_literal map {
      case (_, l) =>
        SLNumeric(l)
    }
  )

  val s_simple_literal_2: Syntax[SimpleLiteral] = syntax(simple_literal)(
    string_literal map {
      case (_, l) =>
        SLString(l)
    }
  )

  val s_simple_literal_3: Syntax[SimpleLiteral] = syntax(simple_literal)(
    boolean_literal map {
      case (_, l) =>
        SLBool(l)
    }
  )

  val date_time_literal: Syntax[DateTimeLiteral] = syntax(
    ("date" | "time" | "duration") ~ "(" ~ string_literal ~ ")" map {
      case (_, (_, _, sl, _)) =>
        DateTimeLiteral(sl)
    }
  )

  val s_simple_literal_4: Syntax[SimpleLiteral] = syntax(simple_literal)(
    date_time_literal map {
      case (_, l) =>
        SLDateTime(l)
    }
  )

  val i_boolean_true: Instance[BooleanLiteral]  =
    instance(boolean_literal)("true") map (_ => BooleanLiteral(true))
  val i_boolean_false: Instance[BooleanLiteral] =
    instance(boolean_literal)("false") map (_ => BooleanLiteral(false))

  val s_comparison: Syntax[Comparison] = syntax(comparison) {
//    expression ~ ("=" | "!=" | "<" | "<=" | ">" | ">=").leftAssoc(1) ~ expression map {
    expression ~ ">".leftAssoc(20) ~ expression map {
      case (_, (e1, _, e2)) =>
        Comparison(e1, "TODO", e2)
    }
  }

  val a_expression: Axiom[Expression] = axiom(expression)

  val a_simple_unary_tests: Axiom[SimpleUnaryTests] = axiom(simple_unary_tests)

}
