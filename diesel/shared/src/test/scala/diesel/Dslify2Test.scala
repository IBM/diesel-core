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
import diesel.Bnf.Rule
import diesel.Bnf.Token
import diesel.Dsl
import diesel.Dsl._
import diesel.Earley
import diesel.GenericTree
import diesel.Lexer
import diesel.Navigator
import munit.FunSuite

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import diesel.GenericNode
import diesel.GenericTerminal

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

  object MyDslStemmed extends Dsl {

    val cPerson: Concept[Person] = concept[Person]

    val cExpr: Concept[Expr]  = concept[Expr]
    val cInt: Concept[Number] = concept("\\d+".r, Number("0"), Some(cExpr)) map { case (_, t) =>
      Number(t.text)
    }

    val cOp: Concept[Operation] = concept(cExpr)
    val sAge                    = syntax(cInt)("age" ~ cPerson map {
      case (_, _) =>
        Number("13")
    })
    val sWeight                 = syntax(cInt)("weight" ~ cPerson map {
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

  test("parse 2") {
    AstHelpers.withAst(MyDslStemmed)("age 'Bob' add 1") {
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
    val bnf     = Bnf(MyDsl)
    val grammar = dumpGrammar(bnf)
    assert(grammar.contains("| the age of expr[cPerson,SINGLE,_,_,_].default"))
    assert(grammar.contains("| the weight of expr[cPerson,SINGLE,_,_,_].default"))
    assert(grammar.contains("| 'Bob'"))

    val bnf_     = stemBnf(bnf)
    val grammar_ = dumpGrammar(bnf_)
    // assertEquals(grammar_, "")
    assert(grammar_.contains("| age expr[cPerson,SINGLE,_,_,_].default"))
    assert(grammar_.contains("| weight expr[cPerson,SINGLE,_,_,_].default"))
    assert(grammar_.contains("| 'Bob'"))
  }

  test("parse stemmed") {
    val input = "the age of 'Bob' add 1"
    val bnf   = Bnf(MyDsl)

    // val tree = parseWithGrammar(bnf, input)
    // assertEquals(tree, null)

    val input_  = stemming(input)
    val bnf_    = stemBnf(bnf)
    val tree_   = parseWithGrammar(bnf_, input_)
    val printed = printTree(tree_)
    assertEquals(printed, "age 'Bob' add 1")
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

  case class StemState(rules: Map[Bnf.Rule, Bnf.Rule]) {
    def getStemmedRule(rule: Bnf.Rule): Option[Bnf.Rule] =
      rules.get(rule)

    def setStemmedRule(rule: Bnf.Rule, stemmed: Bnf.Rule): StemState =
      this.copy(rules = rules + ((rule, stemmed)))
  }

  type Stem[T] = StemState => (T, StemState)
  object Stem {
    def unit[A](v: A): Stem[A] = state => (v, state)

    def map[A, B](stem: Stem[A], f: A => B): Stem[B] = { state =>
      map1(stem(state), f)
    }

    def flatMap[A, B](stem: Stem[A], f: A => Stem[B]): Stem[B] = { state =>
      val (v, state_) = stem(state)
      f(v)(state_)
    }

    def sequence[A](stems: Seq[Stem[A]]): Stem[Seq[A]] =
      stems.foldLeft(Stem.unit(Seq.empty[A])) { case (acc, v) =>
        Stem.flatMap(acc, (acc: Seq[A]) => Stem.map(v, (v: A) => acc :+ v))
      }

    def mapAll[A, B](vs: Seq[A], f: A => Stem[B]): Stem[Seq[B]] =
      Stem.sequence(vs.map(Stem.unit).map(Stem.flatMap(_, f)))
  }

  def map1[A, B, C](t: (A, B), f: A => C): (C, B) = {
    val (a, b) = t
    (f(a), b)
  }

  def stemBnf(bnf: Bnf): Bnf = {
    val stem            = Stem.mapAll(bnf.rules, stemNonTerminal)
    val (rules, state_) = stem(StemState(Map()))
    fixRecursions(rules, state_)
    Bnf(bnf.lexer, rules)
  }

  def fixRecursions(nts: Seq[Bnf.NonTerminal], state: StemState): Unit = {
    nts.foreach {
      case r: Rule => fixRecursionsInRule(r, state)
      case _       =>
    }
  }

  def fixRecursionsInRule(r: Bnf.Rule, state: StemState, dejaVue: Set[Bnf.Rule] = Set()): Unit = {
    r.productions.foreach { p =>
      var updates = p.symbols.zipWithIndex.flatMap {
        case (r: Rule, i) =>
          val stemmed = state.getStemmedRule(r)
          if (stemmed.isEmpty && !dejaVue.contains(r)) {
            fixRecursionsInRule(r, state, dejaVue + r)
          }
          stemmed.map((_, i))
        case _            => None
      }
      updates.foreach { case (r, i) => p.symbols.update(i, r) }
      p.rule match {
        case Some(r: Bnf.Rule) =>
          val stemmed = state.getStemmedRule(r)
          if (stemmed.nonEmpty) {
            p.rule = stemmed
          }
        case _                 =>
      }
    }
  }

  def stemNonTerminal(nt: Bnf.NonTerminal): Stem[Bnf.NonTerminal] = {
    Stem.unit(nt)
    nt match {
      case Bnf.Axiom(r) => Stem.map(stemRule(r), Bnf.Axiom(_))
      case r: Rule      => stemRule(r)
    }
  }

  def stemRule(rule: Bnf.Rule): Stem[Bnf.Rule] = state =>
    state.getStemmedRule(rule)
      //   .filterNot(_ == rule)
      .map(r =>
        (r, state)
      )
      .getOrElse {
        val state1                = state.setStemmedRule(rule, rule)
        val stem                  = Stem.mapAll(rule.productions, stemProduction)
        val (productions, state_) = stem(state1)
        val stemmed               = Bnf.Rule(rule.name, productions)
        (stemmed, state_.setStemmedRule(rule, stemmed))
      }

  def stemProduction(p: Bnf.Production): Stem[Bnf.Production] = {
    val stem = Stem.mapAll(p.symbols.toSeq, stemSymbol)
    Stem.flatMap(
      stem,
      { symbols: Seq[Option[Bnf.Symbol]] => state =>
        val action: Bnf.Action = (_, _) => {
          13
        }
        val rule               = p.rule match {
          case Some(r: Bnf.Rule) => state.getStemmedRule(r)
          case Some(nt)          => Some(nt) // TODO exists? replace, too?
          case None              => None
        }
        (new Bnf.Production(rule, symbols.flatten, action, p.element, p.feature), state)
      }
    )
  }

  def stemSymbol(s: Bnf.Symbol): Stem[Option[Bnf.Symbol]] = {
    s match {
      case Bnf.Axiom(rule) => throw new RuntimeException("boom")
      case r: Rule         => Stem.map(stemRule(r), (r: Bnf.Symbol) => Some(r))
      case t: Token        => Stem.unit(stemToken(t))
    }
  }

  def stemToken(t: Bnf.Token): Option[Bnf.Token] = {
    stemToken(t.name).map(stemmed => t.copy(name = stemmed))
  }

  def parseWithGrammar(
    bnf: Bnf,
    text: String
  ): GenericTree = {
    val parser: Earley = Earley(bnf)
    val a              = AstHelpers.getBnfAxiomOrThrow(bnf, None)
    val result         = parser.parse(new Lexer.Input(text), a)
    val navigator      = Navigator(result)
    navigator.next()
  }

  def printTree(tree: GenericTree): String = {
    printNode(tree.root)
  }

  def printNode(node: GenericNode): String = {
    node.getChildren.map {
      case t: GenericTerminal => t.token.text
      case n: GenericNode     => printNode(n)
    }.mkString(" ")
  }

  // the age of Bob add 1 mul 2
  // age Bob add 1 mul 2

  // Goal
  // - dsl
  // - bnf
  // - stemmed bnf: bnf'
  // - parse using bnf' into tree'
  // - print text for tree' using bnf

  // Next Approach
  // - stem Dsl directly and generate stemmed Bnf from there

}
