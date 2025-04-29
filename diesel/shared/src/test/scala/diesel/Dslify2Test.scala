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
import diesel.Dsl
import diesel.Dsl._
import diesel.Earley
import diesel.GenericNode
import diesel.GenericTerminal
import diesel.GenericTree
import diesel.Lexer
import diesel.Navigator
import diesel.Stemming._
import diesel.Stemming.stemming
import munit.FunSuite

import java.io.ByteArrayOutputStream
import java.io.PrintStream

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
  // - OK the age of <>
  // - variable with ''
  // -

  object MyDsl extends Dsl with Identifiers {

    override def identScanner: Lexer.Scanner = "[a-zA-Z][a-zA-Z0-9]*".r

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

    val a: Axiom[Expr] = axiom(cExpr)
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

    val a: Axiom[Expr] = axiom(cExpr)
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

  test("stemmed grammar") {
    // System.setProperty("diesel.dumpbnf", "true")
    val bnf     = Bnf(MyDsl)
    val grammar = dumpGrammar(bnf)
    assert(grammar.contains("| the age of expr[cPerson,SINGLE,_,_,_].default"))
    assert(grammar.contains("| the weight of expr[cPerson,SINGLE,_,_,_].default"))
    assert(grammar.contains("| 'Bob'"))

    val (bnf_, _) = stemBnf(bnf)
    val grammar_  = dumpGrammar(bnf_)
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

    val input_    = stemming(input)
    val (bnf_, _) = stemBnf(bnf)
    val tree_     = parseWithGrammar(bnf_, input_)
    val printed   = printTree(tree_)
    assertEquals(printed, "age 'Bob' add 1")
  }

  test("unstemm parse stemmed") {
    val input = "the age of 'Bob' add 1"
    val bnf   = Bnf(MyDsl)

    // val tree = parseWithGrammar(bnf, input)
    // assertEquals(tree, null)

    val input_        = stemming(input)
    val (bnf_, state) = stemBnf(bnf)
    val tree_         = parseWithGrammar(bnf_, input_)
    val printed_      = printTree(tree_)
    assertEquals(printed_, "age 'Bob' add 1")

    val unstemmed = tree_.root.value.asInstanceOf[Unstemmed].s.mkString(" ")
    assertEquals(unstemmed, "the age of 'Bob' add 1")
  }

  test("fix out of order") {
    // order!
    val input = "the 'Bob' age add 1"

    val bnf           = Bnf(MyDsl)
    val input_        = stemming(input)
    val (bnf_, state) = stemBnf(bnf)
    val trees         = parseWithGrammarAll(bnf_, input_)
    val printed       = trees.map(printTree).toList
    assertEquals(printed, Seq("age 'Bob' add 1"))

    val unstemmed = trees.map(_.root.value.asInstanceOf[Unstemmed].s.mkString(" "))
    assertEquals(unstemmed, Seq("the age of 'Bob' add 1"))
  }

  test("fix first typo") {
    // typo! (prefix of correct?)
    val input = "agee 'Bob' add 1"

    val bnf           = Bnf(MyDsl)
    val input_        = stemming(input)
    val (bnf_, state) = stemBnf(bnf)
    val trees         = parseWithGrammarAll(bnf_, input_)
    val printed       = trees.map(printTree).toList
    assertEquals(printed, Seq("age 'Bob' add 1"))

    val unstemmed = trees.map(_.root.value.asInstanceOf[Unstemmed].s.mkString(" "))
    assertEquals(unstemmed, Seq("the age of 'Bob' add 1"))
  }

  test("fix two") {
    val input = "agee 'Bob' add 'Bob' weight"

    val bnf           = Bnf(MyDsl)
    val input_        = stemming(input)
    val (bnf_, state) = stemBnf(bnf)
    val trees         = parseWithGrammarAll(bnf_, input_)
    val printed       = trees.map(printTree).toList
    assertEquals(printed, Seq("age 'Bob' add weight 'Bob'"))

    val unstemmed = trees.map(_.root.value.asInstanceOf[Unstemmed].s.mkString(" "))
    assertEquals(unstemmed, Seq("the age of 'Bob' add the weight of 'Bob'"))
  }

  test("fix typo") {
    // typo!
    val input = "weigttt 'Bob'"

    val bnf           = Bnf(MyDsl)
    val input_        = stemming(input)
    val (bnf_, state) = stemBnf(bnf)
    val trees         = parseWithGrammarAll(bnf_, input_)
    val printed       = trees.map(printTree).toList
    assertEquals(printed, Seq("weight 'Bob'"))

    val unstemmed = trees.map(_.root.value.asInstanceOf[Unstemmed].s.mkString(" "))
    assertEquals(unstemmed, Seq("the weight of 'Bob'"))
  }

  test("more typos") {
    // typo!
    val input = "the weigt 'Bob' adn 13 mult 'Bob' agge"

    val bnf           = Bnf(MyDsl)
    val input_        = stemming(input)
    val (bnf_, state) = stemBnf(bnf)
    val trees         = parseWithGrammarAll(bnf_, input_)
    val printed       = trees.map(printTree).toList
    assertEquals(printed, Seq("weight 'Bob' add 13 add age 'Bob'"))

    val unstemmed = trees.map(_.root.value.asInstanceOf[Unstemmed].s.mkString(" "))
    assertEquals(unstemmed, Seq("the weight of 'Bob' add 13 add the age of 'Bob'"))
  }

  test("two typos") {
    // typo!
    val input = "1 mult wihegt 'Bob'"

    val bnf           = Bnf(MyDsl)
    val input_        = stemming(input)
    val (bnf_, state) = stemBnf(bnf)
    val trees         = parseWithGrammarAll(bnf_, input_)
    val printed       = trees.map(printTree).toList
    assertEquals(printed, Seq("1 mul weight 'Bob'"))

    val unstemmed = trees.map(_.root.value.asInstanceOf[Unstemmed].s.mkString(" "))
    assertEquals(unstemmed, Seq("1 mul the weight of 'Bob'"))
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

  private def calcDistance(a: String, b: String): Float = {
    (a.toSet.intersect(b.toSet).size + 0f) / a.size
  }

  private def closeEnough(a: String, b: String): Boolean = {
    calcDistance(a, b) > 0.75
  }

  def parseWithGrammarAll(
    bnf: Bnf,
    text: String
  ): Seq[GenericTree] = {
    val parser: Earley = Earley(bnf, closeEnough = Some(closeEnough))
    val a              = AstHelpers.getBnfAxiomOrThrow(bnf, None)
    val result         = parser.parse(new Lexer.Input(text), a)
    val navigator      = Navigator(result)
    LazyList.from(navigator.toIterator)
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
