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

import diesel.{Dsl, Lexer}
import diesel.Dsl.{Axiom, Concept, DynamicLexer, Identifiers, Instance, Syntax}
import diesel.Lexer.Scanner
import diesel.samples.feel.Ast._

class Feel extends Dsl with Identifiers with DynamicLexer {

  private var scope: Set[String] = Set("foo", "bar")

  override def identScanner: Lexer.Scanner = new Lexer.Scanner {
    override def name: String = "[a-z]+"

    override def findPrefixOf(source: CharSequence): Option[String] = {
      idRegex.findPrefixOf(source).map { str =>
        // regex matches : check in scope/keywords
        var i                 = 1
        var m: Option[String] = None
        while i <= str.length() do {
          val s = str.substring(0, i)
          if scope.contains(s) || keywords.contains(s) then {
            m = Some(s)
          }
          i = i + 1
        }
        m match {
          case Some(x) =>
            x
          case None    =>
            val i = str.indexOf(" in")
            val r =
              if i != -1 then {
                str.substring(0, i)
              } else {
                str
              }
            r
        }
      }
    }
  }

  private val idRegex = "[a-z]+( [a-z]+)*".r

  private var keywords: Set[String] = Set.empty

  override def keywordScanner: Lexer.Scanner = new Lexer.Scanner {
    override def name: String = "xxx"

    override def findPrefixOf(source: CharSequence): Option[String] = {
      val res = idRegex.findPrefixOf(source)
      res match {
        case Some(x) =>
          keywords = keywords + x
        case None    =>
        //
      }
      res
    }
  }

  // use this concept for declarations along with dynamic lexer
  val declarationScanner: Scanner = new Scanner {
    override def name: String = "decl"

    override def findPrefixOf(source: CharSequence): Option[String] = {
      val m = idRegex.findPrefixOf(source)
      m match {
        case Some(x) =>
          val i = x.indexOf(" in")
          val r =
            if i != -1 then {
              x.substring(0, i)
            } else {
              x
            }
          scope = scope + r
          Some(r)
        case None    =>
          None
      }
    }
  }

  val variable_declaration: Concept[Name] =
    concept(declarationScanner, Name("x")) map ((_, t) => Name(t.text))

  val expression: Concept[Expression]                              = concept
  val boxed_expression: Concept[BoxedExpression]                   = concept
  val textual_expression: Concept[TextualExpression]               = concept
  val for_expression: Concept[ForExpression]                       = concept
  val if_expression: Concept[IfExpression]                         = concept
  val quantified_expression: Concept[QuantifiedExpression]         = concept
  val disjunction: Concept[Disjunction]                            = concept
  val conjunction: Concept[Conjunction]                            = concept
  val comparison: Concept[Comparison]                              = concept
  val arithmetic_expression: Concept[ArithmeticExpression]         = concept
  val instance_of: Concept[InstanceOf]                             = concept
  val path_expression: Concept[PathExpression]                     = concept
  val filter_expression: Concept[FilterExpression]                 = concept
  val function_invocation: Concept[FunctionInvocation]             = concept
  val literal: Concept[Literal]                                    = concept
  val simple_positive_unary_test: Concept[SimplePositiveUnaryTest] = concept
  val name: Concept[Name]                                          = concept
  val simple_expression: Concept[SimpleExpression]                 = concept
  val simple_value: Concept[SimpleValue]                           = concept
  val endpoint: Concept[Endpoint]                                  = concept
  val interval: Concept[Interval]                                  = concept
  val positive_unary_test: Concept[PositiveUnaryTest]              = concept
  val positive_unary_tests: Concept[Seq[PositiveUnaryTest]]        = concept
  val unary_tests: Concept[UnaryTests]                             = concept
  val qualified_name: Concept[QualifiedName]                       = concept
  val simple_literal: Concept[SimpleLiteral]                       = concept
  val numeric_literal: Concept[NumericLiteral]                     = concept("[0-9]+(\\.[0-9]+)?".r, NumericLiteral(0))
    .valueToString(i => "%1.0f" format i.v)
    .map((_, t) => NumericLiteral(t.text.toDouble))
  val string_literal: Concept[StringLiteral]                       = concept("\"[a-z]*\"".r, StringLiteral(""))
    .valueToString(x => "\"" + x.v + "\"")
    .map((_, t) => StringLiteral(t.text.drop(1).dropRight(1)))
  val boolean_literal: Concept[BooleanLiteral]                     = concept
  val date_time_literal: Concept[DateTimeLiteral]                  = concept
  val iteration_context: Concept[IterationContext]                 = concept
  val c_type: Concept[Type]                                        = concept
  val boxed_list: Concept[Seq[Expression]]                         = concept
  val function_definition: Concept[FunctionDefinition]             = concept
  val context: Concept[Context]                                    = concept

  val s1ab: Syntax[Expression] = syntax(expression)(
    boxed_expression | textual_expression map {
      case (_, Left(e))  =>
        EBoxed(e)
      case (_, Right(e)) =>
        ETextual(e)
    }
  )

  val s2a1: Syntax[TextualExpression] = syntax(textual_expression)(
    for_expression map {
      case (_, e) => TEFor(e)
    }
  )

  val s2a2: Syntax[TextualExpression] = syntax(textual_expression)(
    if_expression map {
      case (_, e) => TEIf(e)
    }
  )

  val s2a3: Syntax[TextualExpression] = syntax(textual_expression)(
    quantified_expression map {
      case (_, e) => TEQuant(e)
    }
  )

//  val s2a: Syntax[TextualExpression] = syntax(textual_expression)(
//    for_expression | if_expression | quantified_expression map {
//      case (_,(Left(Left(forExpression)))) =>
//        TEFor(forExpression)
//      case (_,(Left(Right(ifExpression)))) =>
//        TEIf(ifExpression)
//      case (_,(Right(quantifiedExpression))) =>
//        TEQuant(quantifiedExpression)
//    }
//  )

  val s2b: Syntax[TextualExpression] = syntax(textual_expression)(
    disjunction map { (_, e) => TEDisj(e) }
  )

  val s2c: Syntax[TextualExpression] = syntax(textual_expression)(
    conjunction map { (_, e) => TEConj(e) }
  )

  val s2d: Syntax[TextualExpression] = syntax(textual_expression)(
    comparison map { (_, e) => TEComp(e) }
  )

  val s2e: Syntax[TextualExpression] = syntax(textual_expression)(
    arithmetic_expression map { (_, e) => TEArith(e) }
  )

  val s2f: Syntax[TextualExpression] = syntax(textual_expression)(
    instance_of map { (_, e) => TEInstanceOf(e) }
  )

  val s2g_1: Syntax[TextualExpression] = syntax(textual_expression)(
    path_expression map {
      case (_, x) =>
        TEPath(x)
    }
  )

  val s2g_2: Syntax[TextualExpression] = syntax(textual_expression)(
    filter_expression map {
      case (_, x) =>
        TEFilter(x)
    }
  )

  val s2g_3: Syntax[TextualExpression] = syntax(textual_expression)(
    function_invocation map {
      case (_, x) =>
        TEFuncInv(x)
    }
  )

//  val s2g: Syntax[TextualExpression] = syntax(textual_expression)(
//    path_expression | filter_expression | function_invocation map {
//      case (_, Left(Left(pathExpression))) =>
//        TEPath(pathExpression)
//      case (_, Left(Right(filterExpression))) =>
//        TEFilter(filterExpression)
//      case (_, Right(functionInvocation)) =>
//        TEFuncInv(functionInvocation)
//    }
//  )

  val s2h_1: Syntax[TextualExpression] = syntax(textual_expression)(
    literal map {
      case (_, x) =>
        TELiteral(x)
    }
  )

  val s2h_2: Syntax[TextualExpression] = syntax(textual_expression)(
    simple_positive_unary_test map {
      case (_, x) =>
        TESPUT(x)
    }
  )

  val s2h_3: Syntax[TextualExpression] = syntax(textual_expression)(
    name map {
      case (c, x) =>
//        val name = x.s
//        // TODO add error if variable doesn't exist
//        if (foo.find(s => s == name).isEmpty) {
//          c.addMarkers(Marker(Errors.SemanticError, c.offset, c.length, "toto biloute"))
//          println("BLAH")
//        }
        TEName(x)
    }
  )

  val s2h_4: Syntax[TextualExpression] = syntax(textual_expression)(
    "(" ~ expression ~ ")" map {
      case (_, (_, e, _)) =>
        TEParens(e)
    }
  )

  val s3: Syntax[Seq[TextualExpression]] = syntax(
    textual_expression ~ ("," ~ textual_expression).rep(true) map {
      case (_, (e, rest)) =>
        Seq(e) ++ rest.map(_._2)
    }
  )

  val s4a_1: Syntax[ArithmeticExpression] = syntax(arithmetic_expression)(
    expression ~ "+".leftAssoc(10) ~ expression map {
      case (_, (e, _, e2)) =>
        Addition(e, e2)
    }
  )

  val s4a_2: Syntax[ArithmeticExpression] = syntax(arithmetic_expression)(
    expression ~ "-".leftAssoc(10) ~ expression map {
      case (_, (e, _, e2)) =>
        Subtraction(e, e2)
    }
  )

  //  val s4a: Syntax[ArithmeticExpression] = syntax(arithmetic_expression)(
//    expression ~ ("+" | "-").leftAssoc(10) ~ expression map {
//      case (_,(e, Left(_), e2)) =>
//        Addition(e, e2)
//      case (_,(e, Right(_), e2)) =>
//        Subtraction(e, e2)
//    }
//  )

  val s4b: Syntax[ArithmeticExpression] = syntax(arithmetic_expression)(
    expression ~ ("*" | "/").leftAssoc(20) ~ expression map {
      case (_, (e, Left(_), e2))  =>
        Multiplication(e, e2)
      case (_, (e, Right(_), e2)) =>
        Division(e, e2)
    }
  )

  val s4c: Syntax[ArithmeticExpression] = syntax(arithmetic_expression)(
    expression ~ "**" ~ expression map {
      case (_, (e, _, e2)) =>
        Exponentiation(e, e2)
    }
  )

  val s4d: Syntax[ArithmeticExpression] = syntax(arithmetic_expression)(
    "-" ~ expression map {
      case (_, (_, e)) =>
        ArithmeticNegation(e)
    }
  )

  val s5: Syntax[SimpleExpression] = syntax(simple_expression)(
    arithmetic_expression | simple_value map {
      case (_, Left(e))  =>
        SEArith(e)
      case (_, Right(e)) =>
        SEVal(e)
    }
  )

  val s6: Syntax[Seq[SimpleExpression]] = syntax(
    simple_expression ~ ("," ~ simple_expression).rep(true) map {
      case (_, (e, rest)) =>
        Seq(e) ++ rest.map(_._2)
    }
  )

  val s7a: Syntax[SimplePositiveUnaryTest] = syntax(simple_positive_unary_test)(
    (">" | "<" | "<=" | ">=") ~ endpoint map {
      case (_, (t, e)) =>
        val op = t match {
          case Left(Left(Left(_)))  =>
            Gt
          case Left(Left(Right(_))) =>
            Lt
          case Left(Right(_))       =>
            Lte
          case Right(_)             =>
            Gte
        }
        SPUTEndpoint(op, e)
    }
  )

  val s7b: Syntax[SimplePositiveUnaryTest] = syntax(simple_positive_unary_test)(
    interval map { (_, i) => SPUTInterval(i) }
  )

  val s8: Syntax[Interval] = syntax(interval)(
    "[" ~ endpoint ~ ".." ~ endpoint ~ "]" map {
      case (_, (start, e1, _, e2, end)) =>
        Interval(start.text, e1, e2, end.text)
    }
  )

  // TODO fix interval bounds, s9 -> s12

  val s13: Syntax[PositiveUnaryTest] = syntax(positive_unary_test)(
    expression map { (_, e) => PositiveUnaryTest(e) }
  )

  val s14: Syntax[Seq[PositiveUnaryTest]] = syntax(positive_unary_tests)(
    positive_unary_test ~ ("," ~ positive_unary_test).rep(true) map {
      case (_, (x, rest)) =>
        Seq(x) ++ rest.map(_._2)
    }
  )

  val s15a: Syntax[UnaryTests] = syntax(unary_tests)(
    positive_unary_tests map { (_, ts) => UTPUT(ts) }
  )

  val s15b: Syntax[UnaryTests] = syntax(unary_tests)(
    "not" ~ "(" ~ positive_unary_tests ~ ")" map {
      case (_, (_, _, puts, _)) =>
        UTPUT(puts, not = true)
    }
  )

  val i15c: Instance[UnaryTests] = instance(unary_tests)("-") map (_ => UTMinus)

  val s16: Syntax[Endpoint] = syntax(endpoint)(
    simple_value map { (_, s) => Endpoint(s) }
  )

  val s17: Syntax[SimpleValue] = syntax(simple_value)(
    qualified_name | simple_literal map {
      case (_, Left(qualifiedName))  =>
        SVQualifiedName(qualifiedName)
      case (_, Right(simpleLiteral)) =>
        SVSimpleLiteral(simpleLiteral)
    }
  )

  val s18: Syntax[QualifiedName] = syntax(qualified_name)(
    name ~ ("." ~ name).rep(true) map {
      case (_, (n, rest)) =>
        QualifiedName(Seq(n) ++ rest.map(_._2))
    }
  )

  // 18 -> 24 already covered above in s4x

  // TODO proper handling of names : s25 -> s30
  // for now we do with IDs
  val s25: Syntax[Name] = syntax(name)(
    id map {
      case (_, i) =>
        Name(i.text)
    }
//    id ~ ("+" | id).rep(true) map {
//      case (_,(i,is)) =>
//        val s = is.map {
//          case Left(_) =>
//            "+"
//          case Right(i) =>
//            i.text
//        }.mkString
//        Name(i.text + s)
//    }
  )

  val s31: Syntax[Literal] = syntax(literal)(
    simple_literal | "null" map {
      case (_, Left(simpleLiteral)) =>
        LSimple(simpleLiteral)
      case (_, Right(_))            =>
        LNull
    }
  )

  val s32: Syntax[SimpleLiteral] = syntax(simple_literal)(
    numeric_literal | string_literal | boolean_literal | date_time_literal map {
      case (_, Left(Left(Left(numericLiteral)))) =>
        SLNumeric(numericLiteral)
      case (_, Left(Left(Right(stringLiteral)))) =>
        SLString(stringLiteral)
      case (_, Left(Right(b)))                   =>
        SLBool(b)
      case (_, Right(dateTimeLiteral))           =>
        SLDateTime(dateTimeLiteral)
    }
  )

  // s33 -> s37 covered by concepts and true/false here below
  val i_boolean_true: Instance[BooleanLiteral]  =
    instance(boolean_literal)("true") map (_ => BooleanLiteral(true))
  val i_boolean_false: Instance[BooleanLiteral] =
    instance(boolean_literal)("false") map (_ => BooleanLiteral(false))

  // 39 -> 42 : functions
  val named_param: Syntax[(Name, Expression)] = syntax(
    name ~ ":" ~ expression map {
      case (_, (n, _, e)) =>
        (n, e)
    }
  )

  val named_parameters: Syntax[PNamed] = syntax(
    named_param ~ ("," ~ named_param).rep(true) map {
      case (_, (p, rest)) =>
        PNamed(Seq(p) ++ rest.map(_._2))
    }
  )

  val positional_parameters: Syntax[PPositional] = syntax(
    expression ~ ("," ~ expression).rep(true) map {
      case (_, (e, rest)) =>
        PPositional(Seq(e) ++ rest.map(_._2))
    }
  )

  val parameters: Syntax[Parameters] = syntax(
    named_parameters | positional_parameters map {
      case (_, Left(named))       =>
        named
      case (_, Right(positional)) =>
        positional
    }
  )

  val s38: Syntax[FunctionInvocation] = syntax(function_invocation)(
    expression ~ "(" ~ (named_parameters | positional_parameters) ~ ")" map {
      case (_, (e, _, Left(p), _))  =>
        FunctionInvocation(e, p)
      case (_, (e, _, Right(p), _)) =>
        FunctionInvocation(e, p)
    }
  )

  val pathName: Concept[Name] = concept(idRegex, Name("x")) map ((_, t) => Name(t.text))

  val s43: Syntax[PathExpression] = syntax(path_expression)(
    expression ~ "." ~ pathName map {
      case (_, (e, _, n)) =>
        PathExpression(e, n)
    }
  )

  val nameInCtx: Syntax[(Name, IterationContext)] = syntax(
    variable_declaration ~ "in" ~ iteration_context map {
      case (_, (n, _, c)) =>
        (n, c)
    }
  )

  val s44: Syntax[ForExpression] = syntax(for_expression)(
    "for" ~ nameInCtx ~ ("," ~ nameInCtx).rep(true) ~ "return".rightAssoc(4) ~ expression map {
      case (_, (_, ni1, rest, _, e)) =>
        ForExpression(
          Seq(ni1) ++ rest.map(_._2),
          e
        )
    }
  )

  val s45: Syntax[IfExpression] = syntax(if_expression)(
    "if" ~ expression ~ "then" ~ expression ~ "else" ~ expression map {
      case (_, (_, cond, _, th, _, el)) =>
        IfExpression(cond, th, el)
    }
  )

  val nameInExpr: Syntax[(Name, Expression)] = syntax(
    variable_declaration ~ "in" ~ expression map {
      case (_, (n, _, e)) =>
        (n, e)
    }
  )

  val s46: Syntax[QuantifiedExpression] = syntax(quantified_expression)(
    ("some" | "every") ~ nameInExpr ~ ("," ~ nameInExpr).rep(true) ~ "satisfies".rightAssoc(
      4
    ) ~ expression map {
      case (_, (someEvery, ne, rest, _, e)) =>
        QuantifiedExpression(
          someEvery.isLeft,
          Seq(ne) ++ rest.map(_._2),
          e
        )
    }
  )

  val s47: Syntax[Disjunction] = syntax(disjunction)(
    expression ~ "or".leftAssoc(10) ~ expression map {
      case (_, (e1, _, e2)) =>
        Disjunction(e1, e2)
    }
  )

  val s48: Syntax[Conjunction] = syntax(conjunction)(
    expression ~ "and".leftAssoc(20) ~ expression map {
      case (_, (e1, _, e2)) =>
        Conjunction(e1, e2)
    }
  )

  val s49a: Syntax[Comparison] = syntax(comparison)(
    expression ~ ("=" | "!=" | "<" | "<=" | ">" | ">=").noneAssoc(5) ~ expression map {
      case (_, (e1, t, e2)) =>
        val op = t match {
          case Left(Left(Left(Left(Left(_)))))  =>
            CEq
          case Left(Left(Left(Left(Right(_))))) =>
            CNeq
          case Left(Left(Left(Right(_))))       =>
            CLt
          case Left(Left(Right(_)))             =>
            CLte
          case Left(Right(_))                   =>
            CGt
          case Right(_)                         =>
            CGte
        }
        CCompare(op, e1, e2)
    }
  )

  val s49b: Syntax[Comparison] = syntax(comparison)(
    expression ~ "between" ~ expression ~ "and" ~ expression map {
      case (_, (e1, _, e2, _, e3)) =>
        CBetween(e1, e2, e3)
    }
  )

  val s49c: Syntax[Comparison] = syntax(comparison)(
    expression ~ "in" ~ positive_unary_test map {
      case (_, (e, _, p)) =>
        CIn(e, Seq(p))
    }
  )

  val s49d: Syntax[Comparison] = syntax(comparison)(
    expression ~ "in" ~ "(" ~ positive_unary_tests ~ ")" map {
      case (_, (e, _, _, ps, _)) =>
        CIn(e, ps)
    }
  )

  val s50: Syntax[FilterExpression] = syntax(filter_expression)(
    expression ~ "[" ~ expression ~ "]" map {
      case (_, (e1, _, e2, _)) =>
        FilterExpression(e1, e2)
    }
  )

  val s51: Syntax[InstanceOf] = syntax(instance_of)(
    expression ~ "instance" ~ "of" ~ c_type map {
      case (_, (e, _, _, t)) =>
        InstanceOf(e, t)
    }
  )

  val s52_1: Syntax[Type] = syntax(c_type)(
    qualified_name map {
      case (_, n) =>
        TQualified(n)
    }
  )

  val s52_2: Syntax[Type] = syntax(c_type)(
    "list" ~ "<" ~ c_type ~ ">" map {
      case (_, (_, _, t, _)) =>
        TList(t)
    }
  )

  val nameAndType: Syntax[(Name, Type)] = syntax(
    name ~ ":" ~ c_type map {
      case (_, (n, _, t)) =>
        (n, t)
    }
  )

  val s52_3: Syntax[Type] = syntax(c_type)(
    "context" ~ "<" ~ nameAndType ~ ("," ~ nameAndType).rep(true) ~ ">" map {
      case (_, (_, _, nt, rest, _)) =>
        TContext(
          Seq(nt) ++ rest.map(_._2)
        )
    }
  )

  val s52_4: Syntax[Type] = syntax(c_type)(
    "function" ~ "<" ~ (c_type ~ ("," ~ c_type).rep(true)).? ~ ">" ~ "->" ~ c_type map {
      case (_, (_, _, args, _, _, retType)) =>
        TFunction(
          args match {
            case Some((x, rest)) =>
              Seq(x) ++ rest.map(_._2)
            case None            =>
              Seq()
          },
          retType
        )
    }
  )

  val s53_1: Syntax[BoxedExpression] = syntax(boxed_expression)(
    boxed_list map {
      case (_, x) =>
        BEList(x)
    }
  )

  val s53_2: Syntax[BoxedExpression] = syntax(boxed_expression)(
    function_definition map {
      case (_, x) =>
        BEFunDef(x)
    }
  )

  val s53_3: Syntax[BoxedExpression] = syntax(boxed_expression)(
    context map {
      case (_, x) =>
        BEContext(x)
    }
  )

//  val s53: Syntax[BoxedExpression] = syntax(boxed_expression)(
//    boxed_list | function_definition | context map {
//      case (_,Left(Left(expressions))) =>
//        BEList(expressions)
//      case (_,Left(Right(functionDefinition))) =>
//        BEFunDef(functionDefinition)
//      case (_,Right(context)) =>
//        BEContext(context)
//    }
//  )

  val s54: Syntax[Seq[Expression]] = syntax(boxed_list)(
    "[" ~ (expression ~ ("," ~ expression).rep(true)).? ~ "]" map {
      case (_, (_, Some((e, rest)), _)) =>
        Seq(e) ++ rest.map(_._2)
      case (_, (_, None, _))            =>
        Seq()
    }
  )

  // s56
  val formalParameter: Syntax[FormalParameter] = syntax(
    variable_declaration ~ (":" ~ c_type).? map {
      case (_, (n, t)) =>
        FormalParameter(n, t.map(_._2))
    }
  )

  val s55: Syntax[FunctionDefinition] = syntax(function_definition)(
    "function" ~ "(" ~ (formalParameter ~ ("," ~ formalParameter).rep(
      true
    )).? ~ ")" ~ "external".? ~ expression map {
      case (_, (_, _, params, _, ext, e)) =>
        FunctionDefinition(
          params match {
            case Some((p, rest)) =>
              Seq(p) ++ rest.map(_._2)
            case None            =>
              Seq()
          },
          e,
          ext.isDefined
        )
    }
  )

//  var foo: List[String] = List.empty

  // 58
  val contextEntry: Syntax[ContextEntry] = syntax(
    (variable_declaration | string_literal) ~ ":" ~ expression map {
      case (_, (nameOrLiteral, _, e)) =>
        ContextEntry(nameOrLiteral, e)
    }
  )

  val s57: Syntax[Context] = syntax(context)(
    "{" ~ (contextEntry ~ ("," ~ contextEntry).rep(true)).? ~ "}" map {
      case (_, (_, Some((e, rest)), _)) =>
        Context(
          Seq(e) ++ rest.map(_._2)
        )
      case (_, (_, None, _))            => Context(
          Seq.empty
        )
    }
  )

  val s60_1: Syntax[DateTimeLiteral] = syntax(date_time_literal)(
    "@" ~ string_literal map {
      case (_, (_, s)) =>
        DTAt(s)
    }
  )

  // TODO
//  val s60_2: Syntax[DateTimeLiteral] = syntax(date_time_literal)(
//    function_invocation map {
//      case (_,f) =>
//        DTFun(f)
//    }
//  )

  val s63: Syntax[IterationContext] = syntax(iteration_context)(
    expression ~ (".." ~ expression).? map {
      case (_, (e, x)) =>
        IterationContext(e, x.map(_._2))
    }
  )

  val a_expression: Axiom[Expression] = axiom(expression)

  val a_unary_tests: Axiom[UnaryTests] = axiom(unary_tests)

}
