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

import diesel.AstHelpers
import diesel.Bnf
import diesel.Dsl
import diesel.Dsl._
import diesel.Lexer.ConceptId
import diesel.Lexer.Eos
import diesel.Lexer.Input
import munit.FunSuite
import diesel.BnfHtml
import diesel.Bnf.Rule
import diesel.Bnf.Token
import java.io.PrintStream
import java.io.ByteArrayOutputStream

class Dslify2Test extends FunSuite {

  object Ast {
    trait Expr
    case class Number(v: String)     extends Expr
    sealed trait Operation           extends Expr
    case class Add(a: Expr, b: Expr) extends Operation
    case class Mul(a: Expr, b: Expr) extends Operation

    trait Person
    case object Bob extends Person
  }

  import Ast._

  // TODO
  // - the age of <>
  // - variable with ''
  // -

  object MyDsl extends Dsl {

    val cPerson: Concept[Person] = concept[Person]

    val cExpr: Concept[Expr]  = concept[Expr]
    val cInt: Concept[Number] = concept("\\d+".r, Number("0"), Some(cExpr)) map { case (_, t) =>
      Number(t.text)
    }

    val cOp: Concept[Operation] = concept(cExpr)
    val sAge                    = syntax(cInt)("the" ~ "age" ~ "of" ~ cPerson map {
      case (_, _) =>
        Number("13")
    })
    val sWeight                 = syntax(cInt)("the" ~ "weight" ~ "of" ~ cPerson map {
      case (_, _) =>
        Number("42")
    })

    val sBob = syntax(cPerson)(SPStr("'Bob'") map {
      case (_, _) =>
        Bob
    })

    val sAdd: Syntax[Operation] = syntax(cOp)(cExpr ~ "add".leftAssoc(1) ~ cExpr map {
      case (_, (l, _, r)) =>
        Add(l, r)
    })

    val sMul: Syntax[Operation] = syntax(cOp)(cExpr ~ "mul".leftAssoc(2) ~ cExpr map {
      case (_, (l, _, r)) =>
        Mul(l, r)
    })

    val a: Axiom[Operation] = axiom(cOp)
  }

  test("parse") {
    AstHelpers.withAst(MyDsl)("the age of 'Bob' add 1") {
      t =>
        {
          AstHelpers.assertNoMarkers(t)
          assertEquals(
            t.value,
            Add(Number("13"), Number("1"))
          )
        }
    }
  }

  test("stemming the grammar") {
    val input   = "the age of 'Bob' add 1"
    val stemmed = stemming(input)
    assertEquals(stemmed, "age 'Bob' add 1")
  }

  test("stemming the input") {
    val input   = "the age Bob add 1"
    val stemmed = stemming(input)
    assertEquals(stemmed, "age 'Bob' add 1")
  }

  def stemming(text: String): String =
    text.split(" ")
      .flatMap(stemToken)
      .mkString(" ")

  def stemToken(t: String): Option[String] = {
    val drops = Set("the", "of")
    Some(t).filterNot(drops.contains).map(w => if ("Bob" == w) "'Bob'" else w)
  }

  test("stemmed grammar") {
    // System.setProperty("diesel.dumpbnf", "true")
    val bnf      = Bnf(MyDsl)
    val grammar  = dumpGrammar(bnf)
    assert(grammar.contains("| the age of expr[cPerson,SINGLE,_,_,_].default"))
    assert(grammar.contains("| the weight of expr[cPerson,SINGLE,_,_,_].default"))
    val bnf_     = stemBnf(bnf)
    val grammar_ = dumpGrammar(bnf_)
    // assertEquals(grammar_, "")
    assert(grammar_.contains("| age expr[cPerson,SINGLE,_,_,_].default"))
    assert(grammar_.contains("| weight expr[cPerson,SINGLE,_,_,_].default"))
  }

  def dumpGrammar(bnf: Bnf): String = {
    val bos = new ByteArrayOutputStream()
    val ps  = new PrintStream(bos)
    bnf.rules.foreach {
      case r: Rule      =>
        r.dump(ps)
        ps.println()
      case a: Bnf.Axiom =>
        a.dump(ps)
        ps.println()
      case _            =>
    }
    bos.toString()
  }

  def stemBnf(bnf: Bnf): Bnf = {
    val (rules, _) =
      bnf.rules.foldLeft((Seq.empty[Bnf.NonTerminal], StemState(Map()))) {
        case ((nts, state), nt) =>
          val (nt_, state_) = stemNonTerminal(nt, state)
          (nts :+ nt_, state_)
      }
    Bnf(bnf.lexer, rules)
  }

  case class StemState(rules: Map[Bnf.Rule, Bnf.Rule]) {
    def getStemmedRule(rule: Bnf.Rule): Option[Bnf.Rule] =
      rules.get(rule)

    def setStemmedRule(rule: Bnf.Rule, stemmed: Bnf.Rule): StemState =
      this.copy(rules = rules + ((rule, stemmed)))
  }

  def map1[A, B, C](t: (A, B), f: A => C): (C, B) = {
    val (a, b) = t
    (f(a), b)
  }

  def stemNonTerminal(nt: Bnf.NonTerminal, state: StemState): (Bnf.NonTerminal, StemState) = {
    nt match {
      case Bnf.Axiom(rule) => map1(stemRule(rule, state), Bnf.Axiom(_))
      case r: Rule         => stemRule(r, state)
    }
  }

  def stemRule(rule: Bnf.Rule, state: StemState): (Bnf.Rule, StemState) = {
    state.getStemmedRule(rule)
      //   .filterNot(_ == rule)
      .map((_, state))
      .getOrElse {
        val state1                = state.setStemmedRule(rule, rule)
        val (productions, state_) =
          rule.productions.foldLeft((Seq.empty[Bnf.Production], state1)) {
            case ((ps, state), p) =>
              val (p_, state_) = stemProduction(p, state)
              (ps :+ p_, state_)
          }
        val stemmed               = Bnf.Rule(rule.name, productions)
        (stemmed, state_.setStemmedRule(rule, stemmed))
      }
  }

  def stemProduction(p: Bnf.Production, state: StemState): (Bnf.Production, StemState) = {
    val (symbols, state_) =
      p.symbols.foldLeft((Seq.empty[Bnf.Symbol], state)) {
        case ((vs, state), v) =>
          val (v_, state_) = stemSymbol(v, state)
          v_.map(v => (vs :+ v, state_)).getOrElse((vs, state_))
      }
    (new Bnf.Production(p.rule, symbols, p.action, p.element, p.feature), state_)
  }

  def stemSymbol(s: Bnf.Symbol, state: StemState): (Option[Bnf.Symbol], StemState) = {
    s match {
      case Bnf.Axiom(rule) => throw new RuntimeException("boom")
      case r: Rule         => map1(stemRule(r, state), (r: Bnf.Symbol) => Some(r))
      case t: Token        => (stemToken(t), state)
    }
  }

  def stemToken(t: Bnf.Token): Option[Bnf.Token] = {
    stemToken(t.name).map(stemmed => t.copy(name = stemmed))
  }

  // the age of Bob add 1 mul 2
  // age Bob add 1 mul 2

  // Goal
  // - dsl
  // - bnf
  // - stemmed bnf: bnf'
  // - parse using bnf' into tree'
  // - print text for tree' using bnf

}
