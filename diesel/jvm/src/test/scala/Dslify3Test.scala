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
import java.io.FileInputStream
import opennlp.tools.parser.ParserModel
import java.io.IOException
import scala.util.Try
import opennlp.tools.cmdline.parser.ParserTool
import opennlp.tools.parser.ParserFactory
import scala.util.Success
import scala.util.Failure
import opennlp.tools.parser.Parse
import diesel.Bnf.Token

class Dslify3Test extends FunSuite {

  object Ast {
    trait Expr
    case class Number(v: String)     extends Expr
    sealed trait Operation           extends Expr
    case class Add(a: Expr, b: Expr) extends Operation
    case class Mul(a: Expr, b: Expr) extends Operation

    trait Person
    case object Bob extends Person
    case object Var extends Person
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

    val sBob = syntax(cPerson)(SPStr("Bob") map {
      case (_, _) =>
        Bob
    })

    val sVar = syntax(cPerson)(idOrKeyword map {
      case (_, _) =>
        Var
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

  test("get nlp ast") {
    val topParses = doParse("the age of john")
    assertEquals(1, topParses.length)
    printParse(topParses(0))
  }

  def printParse(p: Parse, depth: Int = 0): Unit = {
    val indent: String = "  " * depth
    println(s"$indent${p.getType()}:${p.getCoveredText()}")
    for (child <- p.getChildren()) {
        printParse(child, depth+1)
    }
  }

  test("type inference") {

    val vars = Map("john" -> MyDsl.cPerson)
    val bnf = Bnf(MyDsl)
    val topParses = doParse("the age of john")
    assertEquals(1, topParses.length)
    val p = topParses(0)
    val inferred = inferTypes(p, vars, bnf)
    assertEquals(inferred, null)
  }

  private def inferTypes(p: Parse, vars: Map[String,Concept[_]], bnf: Bnf): InferNode = {
    val children = p.getChildren().toSeq.map(p2 => inferTypes(p2, vars, bnf))    
    val prods = 
        if (p.getType() == "NN") {
            findProductions(bnf, p, children)
        } else {
            Seq()
        }
    InferNode(children, prods)
  }

  def findProductions(bnf: Bnf, p: Parse, children: Seq[InferNode]): Seq[Bnf.Production] = {
    bnf.rules
        .flatMap {
            case a:Bnf.Axiom => 
                Seq(a.production)                
            case Rule(name, productions) =>
                productions
        }
        .filter { production => 
            production.symbols.exists {
                case Bnf.Axiom(rule) => 
                    false
                case Rule(name, productions) =>
                    false
                case Token(name, tokenId, style) =>
                    name == p.getCoveredText()
            }                                
        }
  }

  case class InferNode(children: Seq[InferNode], prods: Seq[Bnf.Production]) {}

  private def doParse(s: String): Seq[Parse] = {
    val modelIn = new FileInputStream("/home/remi/Downloads/en-parser-chunking.bin")
    Try(new ParserModel(modelIn)) match {
        case Success(parserModel) => 
            val parser = ParserFactory.create(parserModel)
            val sentence = "the age of john"
            ParserTool.parseLine(sentence, parser, 1).toSeq
        case Failure(exception) => 
            throw new RuntimeException(exception)
    }
  }


}
