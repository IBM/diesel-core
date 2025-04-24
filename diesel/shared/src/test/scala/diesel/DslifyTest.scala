import munit.FunSuite
import diesel.Dsl
import diesel.Dsl._
import diesel.AstHelpers
import diesel.Bnf
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

  object Ast2 {
    trait Expr
    case class Number(v: String)     extends Expr
    sealed trait Operation           extends Expr
    case class Add(a: Expr, b: Expr) extends Operation
    case class Mul(a: Expr, b: Expr) extends Operation
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

  object MyDsl2 extends Dsl {
    import Ast2._

    val cExpr: Concept[Expr]  = concept[Expr]
    val cInt: Concept[Number] = concept("\\d+".r, Number("0"), Some(cExpr)) map { case (_, t) =>
      Number(t.text)
    }

    val cOp: Concept[Operation] = concept(cExpr)

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

  test("parse 2") {
    import Ast2._
    AstHelpers.withAst(MyDsl2)("1 add 2 mul 3") {
      t =>
        {
          AstHelpers.assertNoMarkers(t)
          assertEquals(
            t.value,
            Add(Number("1"), Mul(Number("2"), Number("3")))
          )
        }
    }
  }

  test("first score and generate: add") {
    val bnf        = Bnf(MyDsl)
    val scoringFun = (name: String) => if (name == "add") 1d else 0d;
    val scores     = scoreAxioms(bnf, scoringFun)
    val text       = generateDsl(bnf, scores, Seq("cInt", "cInt"))
    assertEquals(text, Some("cInt add cInt"))
  }

  test("MyDsl2: first score and generate: add") {
    val bnf        = Bnf(MyDsl2)
    val scoringFun = (name: String) => if (name == "add") 1d else 0d;
    val scores     = scoreAxioms(bnf, scoringFun)
    val text       = generateDsl(bnf, scores, Seq("13", "14"))
    assertEquals(text, Some("13 add 14 add ?"))
  }

  test("score and generate") {
    val bnf        = Bnf(MyDsl)
    val scoringFun = scoreWords("13 mul 14");
    val scores     = scoreAxioms(bnf, scoringFun)
    val text       = generateDsl(bnf, scores, Seq("cInt", "cInt"))
    assertEquals(text, Some("cInt mul cInt"))
  }

  test("score and generate by overlap") {
    val bnf        = Bnf(MyDsl)
    val scoringFun = scoreWordsLetters("13 multiply 14");
    val scores     = scoreAxioms(bnf, scoringFun)
    val text       = generateDsl(bnf, scores, Seq("cInt", "cInt"))
    assertEquals(text, Some("cInt mul cInt"))
  }

  test("list literals in text") {
    val bnf      = Bnf(MyDsl)
    val text     = "13 multiply 14"
    val literals = extractLiterals(bnf, text)
    assertEquals(literals, Seq("13", "14"))
  }

  def extractLiterals(bnf: Bnf, text: String): Seq[String] = {
    val input                 = new Input(text)
    val tokens                = Seq.unfold(bnf.lexer) { lexer =>
      Option(lexer.next(input)).filterNot(_.id == Eos).map((_, lexer))
    }
    val concepts: Set[String] =
      MyDsl.getConcepts.collect { case c: Concept[_] if c.data.isDefined => ConceptId(c).name }.toSet
    val literals              = tokens.filter(t => concepts.contains(t.id.name)).map(_.text)
    println("FW", concepts, tokens.map(_.id.name))
    literals
  }

  test("score and generate by overlap and fill literals") {
    val bnf        = Bnf(MyDsl)
    val input      = "13 multiply 14"
    val scoringFun = scoreWordsLetters(input);
    val scores     = scoreAxioms(bnf, scoringFun)
    val literals   = extractLiterals(bnf, input)
    val text       = generateDsl(bnf, scores, literals)
    assertEquals(text, Some("13 mul 14"))
  }

  test("MyDsl2: score and generate: add/mul") {
    val bnf        = Bnf(MyDsl2)
    val input      = "13 add 14 mul 15"
    val scoringFun = scoreWordsLetters(input);
    val scores     = scoreAxioms(bnf, scoringFun)
    val literals   = extractLiterals(bnf, input)
    assertEquals(literals, Seq("13", "14", "15"))
    val text       = generateDsl(bnf, scores, literals)
    assertEquals(text, Some("13 mul 14 mul 15 mul ?"))
  }

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
        // println(name)
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

  def scoreRule(
    r: Bnf.Rule,
    scores: Map[Bnf.Production, Double],
    scoringFun: String => Double
  ): (Double, Map[Bnf.Production, Double]) = {
    val (total, scores2) = r.productions.foldLeft((0d, scores)) {
      case ((total, scores), p) => {
        scores.get(p).map { score =>
          (Math.max(total, score), scores)
        }.getOrElse {
          val (score, scores2) = scoreProduction(p, scores + ((p, -1d)), scoringFun)
          (Math.max(total, score), scores2 + ((p, score)))
        }
      }
    }
    (total, scores2)
  }

  def generateDsl(
    bnf: Bnf,
    scores: Map[Bnf.Production, Double],
    literals: Seq[String] = Seq()
  ): Option[String] = {
    val axiom = bnf.axioms.reduceOption[Bnf.Axiom] { case (max, a) =>
      val score1 = scores(max.production)
      val score  = scores(a.production)
      if (score > score1) a else max
    }
    axiom.map(a => generateProduction(a.production, scores, literals, Set())._1)
  }

  def generateProduction(
    p: Bnf.Production,
    scores: Map[Bnf.Production, Double],
    literals: Seq[String],
    dejaVue: Set[Bnf.Production]
  ): (String, Seq[String]) = {
    val (ts, literals1) = p.symbols.foldLeft((Seq.empty[String], literals)) {
      case ((acc, Seq()), r: Bnf.Rule)                      => (acc :+ "?", literals)
      case ((acc, literals), r: Bnf.Rule)                   =>
        generateRule(r, scores, literals, dejaVue + p).map { case (t, literals) =>
          (acc :+ t, literals)
        }
          .getOrElse((acc, literals))
      case ((acc, literals), Bnf.Token(_, _: ConceptId, _)) => literals.headOption.map { t =>
          (acc :+ t, literals.tail)
        }.getOrElse((acc, literals))
      case ((acc, literals), t: Bnf.Token)                  => (acc :+ t.tokenId.name, literals)
      case _                                                => throw new RuntimeException("boom")
    }
    (ts.mkString(" "), literals1)
  }

  def generateRule(
    r: Bnf.Rule,
    scores: Map[Bnf.Production, Double],
    literals: Seq[String],
    dejaVue: Set[Bnf.Production]
  ): Option[(String, Seq[String])] = {
    var candidates = r.productions.filterNot(p => dejaVue.contains(p))
    if (candidates.size == 1) {
      candidates.headOption.map(p => generateProduction(p, scores, literals, dejaVue))
    } else {
      val p = candidates.reduceOption[Bnf.Production] {
        case (max, p) =>
          val score1 = scores(max)
          val score  = scores(p)
          if (score > score1) p else max
      }
      p.map(p => generateProduction(p, scores, literals, dejaVue))
    }
  }

  // test("simple") {
  //     val dsl = dslify(bnf, "1 add 2")
  //     assertEquals(dsl, "1 add 2")
  // }

}
