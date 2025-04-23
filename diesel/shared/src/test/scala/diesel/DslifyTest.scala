import munit.FunSuite
import diesel.Dsl
import diesel.Dsl._
import diesel.AstHelpers
import diesel.Bnf
import diesel.Lexer.TokenId
import diesel.Lexer.Input
import diesel.Lexer.Eos
import diesel.Lexer.ConceptId

class DslifyTest extends FunSuite {

  object Ast {
    case class Number(v: String)
    sealed trait Operation
    case class Add(a: Number, b: Number) extends Operation
    case class Mul(a: Number, b: Number) extends Operation
  }

  import Ast._

  object MyDsl extends Dsl {

    val cInt: Concept[Number] = concept("\\d+".r, Number("0")) map { case (_, t) => Number(t.text) }

    val cOp: Concept[Operation] = concept[Operation]

    val sAdd: Syntax[Operation] = syntax(cOp)(cInt ~ "add" ~ cInt map {
      case (_, (l, _, r)) =>
        Add(l, r)
    })

    val sMul: Syntax[Operation] = syntax(cOp)(cInt ~ "mul" ~ cInt map {
      case (_, (l, _, r)) =>
        Mul(l, r)
    })

    val a: Axiom[Operation] = axiom(cOp)
  }

  test("parse") {
    AstHelpers.withAst(MyDsl)("1 add 2") {
      t =>
        {
          AstHelpers.assertNoMarkers(t)
          assertEquals(
            t.value,
            Add(Number("1"), Number("2"))
          )
        }
    }
  }

  test("first score and generate: add") {
    val bnf        = Bnf(MyDsl)
    val scoringFun = (name: String) => if (name == "add") 1d else 0d;
    val scores     = scoreAxioms(bnf, scoringFun)
    val text       = generateDsl(bnf, scores)
    assertEquals(text, Some("cInt add cInt"))
  }

  test("score and generate") {
    val bnf        = Bnf(MyDsl)
    val scoringFun = scoreWords("13 mul 14");
    val scores     = scoreAxioms(bnf, scoringFun)
    val text       = generateDsl(bnf, scores)
    assertEquals(text, Some("cInt mul cInt"))
  }

  test("score and generate by overlap") {
    val bnf        = Bnf(MyDsl)
    val scoringFun = scoreWordsLetters("13 multiply 14");
    val scores     = scoreAxioms(bnf, scoringFun)
    val text       = generateDsl(bnf, scores)
    assertEquals(text, Some("cInt mul cInt"))
  }

  test("list literals in text") {
    val bnf                    = Bnf(MyDsl)
    val input                  = new Input("13 multiply 14")
    val tokens                 = Seq.unfold(bnf.lexer) { lexer =>
      Option(lexer.next(input)).filterNot(_.id == Eos).map((_, lexer))
    }
    val concepts: Set[TokenId] =
      MyDsl.getConcepts.collect { case c: Concept[_] if c.data.isDefined => ConceptId(c) }.toSet
    val literals               = tokens.filter(t => concepts.contains(t.id)).map(_.text)
    assertEquals(literals, Seq("13", "14"))
  }

  // TODO
  // - recursivive grammar
  // - fill literals into concepts
  // - grammar with precedences / associativity

//   test("fw") {
//     val bnf        = Bnf(MyDsl)
//     val scoringFun = scoreWordsLetters("13 mult 14 add 15");
//     val scores     = scoreAxioms(bnf, scoringFun)
//     val text       = generateDsl(bnf, scores)
//     assertEquals(text, Some("cInt mul cInt add cInt"))
//   }

  def scoreWordsLetters(text: String): String => Double = {
    var letterSets = text.split(" ").map(_.toCharArray().toSet)
    token =>
      val letters    = token.toCharArray().toSet
      val maxOverlap = letterSets.map(ls => ls.intersect(letters).size).max
      (0d + maxOverlap) / token.size
  }

  def scoreWords(text: String): String => Double = {
    var words = text.split(" ").toSet
    token => if (words.contains(token)) 1d else 0d
  }

  def scoreAxioms(bnf: Bnf, scoringFun: String => Double): Map[Bnf.Production, Double] =
    bnf.axioms.foldLeft(Map[Bnf.Production, Double]()) {
      case (acc, a) =>
        println(s"Axiom ${a.name}")
        val (score, acc2) = scoreProduction(a.production, acc, scoringFun)
        acc2 + ((a.production, score))
    }

  def scoreProduction(
    p: Bnf.Production,
    scores: Map[Bnf.Production, Double],
    scoringFun: String => Double
  ): (Double, Map[Bnf.Production, Double]) = {
    val (total, scores2) = p.symbols.foldLeft((0d, scores)) {
      case (_, Bnf.Axiom(_))                         => {
        throw new RuntimeException("fuck");
      }
      case ((total, scores), Bnf.Token(name, id, _)) => {
        println(name)
        val score = scoringFun(id.name)
        (total + score, scores)
      }
      case ((total, scores), r: Bnf.Rule)            => {
        val (score, scores2) = scoreRule(r, scores, scoringFun)
        (total + score, scores2)
      }
    }
    (total / p.symbols.length, scores2)
  }

//   def scoreToken(id: TokenId): Double = if (id.name == "add") 1d else 0d

  def scoreRule(
    r: Bnf.Rule,
    scores: Map[Bnf.Production, Double],
    scoringFun: String => Double
  ): (Double, Map[Bnf.Production, Double]) = {
    val (total, scores2) = r.productions.foldLeft((0d, scores)) {
      case ((total, scores), p) => {
        // TODO stop recursion
        val (score, scores2) = scoreProduction(p, scores, scoringFun)
        (Math.max(total, score), scores2 + ((p, score)))
      }
    }
    (total, scores2)
  }

  def generateDsl(bnf: Bnf, scores: Map[Bnf.Production, Double]): Option[String] = {
    val axiom = bnf.axioms.reduceOption[Bnf.Axiom] { case (max, a) =>
      val score1 = scores(max.production)
      val score  = scores(a.production)
      if (score > score1) a else max
    }
    axiom.map(a => generateProduction(a.production, scores))
  }

  def generateProduction(p: Bnf.Production, scores: Map[Bnf.Production, Double]): String = {
    p.symbols.flatMap {
      case r: Bnf.Rule  => generateRule(r, scores)
      case t: Bnf.Token => Some(t.tokenId.name)
      case _            => throw new RuntimeException("boom")
    }.mkString(" ")
  }

  def generateRule(r: Bnf.Rule, scores: Map[Bnf.Production, Double]): Option[String] = {
    val p = r.productions.reduceOption[Bnf.Production] { case (max, p) =>
      val score1 = scores(max)
      val score  = scores(p)
      if (score > score1) p else max
    }
    p.map(p => generateProduction(p, scores))
  }

  // test("simple") {
  //     val dsl = dslify(bnf, "1 add 2")
  //     assertEquals(dsl, "1 add 2")
  // }

}
