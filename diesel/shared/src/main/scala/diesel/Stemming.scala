package diesel

import diesel.Bnf.Rule
import diesel.Bnf.Token
import diesel.Lexer.ConceptId
import scala.annotation.tailrec
import diesel.Lexer.IdentifierOrKeywordToken

object Stemming {
  def stemming(text: String): String =
    text.split(" ")
      .flatMap(stemToken)
      .mkString(" ")

  def stemToken(t: String): Option[String] = {
    val drops = Set("the", "of")
    Some(t).filterNot(drops.contains).map(w => if ("Bob" == w) "'Bob'" else w)
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

  def stemBnf(bnf: Bnf): (Bnf, StemState) = {
    val stem            = Stem.mapAll(bnf.rules, stemNonTerminal)
    val (rules, state_) = stem(StemState(Map()))
    fixRecursions(rules, state_)
    (Bnf(bnf.lexer, rules), state_)
  }

  def fixRecursions(nts: Seq[Bnf.NonTerminal], state: StemState): Unit = {
    nts.foreach {
      case r: Rule => fixRecursionsInRule(r, state)
      case _       =>
    }
  }

  def fixRecursionsInRule(r: Bnf.Rule, state: StemState): Unit = {
    @tailrec
    def go(
      rules: Seq[Bnf.Rule],
      state: StemState,
      collect: Set[Bnf.Rule] = Set()
    ): Seq[Bnf.Rule] = {
      val subrules = rules.flatMap(_.productions).flatMap { p =>
        p.symbols.flatMap {
          case r: Rule =>
            val stemmed = state.getStemmedRule(r)
            if (stemmed.isEmpty && !collect.contains(r)) {
              Some(r)
            } else None
          case _       => None
        }
      }
      if (subrules.nonEmpty) go(subrules, state, collect ++ rules ++ subrules)
      else collect.toSeq
    }
    val all = go(Seq(r), state, Set())
    all.foreach { r =>
      r.productions.foreach { p =>
        var updates = p.symbols.zipWithIndex.flatMap {
          case (r: Rule, i) =>
            val stemmed = state.getStemmedRule(r)
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
        val action: Bnf.Action = (ctx, vs) => {
          unstem(p.symbols.toSeq, vs)
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

  case class Unstemmed(s: Seq[String])

  def unstem(symbols: Seq[Bnf.Symbol], vs: Seq[Any]): Unstemmed = {
    def go(symbols: List[Bnf.Symbol], vs: List[Any], current: Unstemmed): Unstemmed = {
      symbols match {
        case (t: Bnf.Token) :: next =>
          t.tokenId match {
            case ConceptId(c)                    => vs match {
                case Lexer.Token(_, text, _) :: vrest =>
                  go(next, vrest, Unstemmed(current.s :+ text))
                case _ :: vrest                       => go(symbols, vrest, current)
                case _                                => go(next, List(), Unstemmed(current.s :+ "?"))
              }
            case id: IdentifierOrKeywordToken[_] => vs match {
                case Lexer.Token(_, text, _) :: vrest =>
                  go(next, vrest, Unstemmed(current.s :+ text))
                case _ :: vrest                       => go(symbols, vrest, current)
                case _                                => go(next, List(), Unstemmed(current.s :+ "?"))
              }
            case _                               => go(next, vs, Unstemmed(current.s :+ t.name))
          }
        case (r: Rule) :: next      =>
          vs match {
            case Unstemmed(s) :: vrest => go(next, vrest, Unstemmed(current.s ++ s))
            // case Token(s) :: vrest     => go(next, vrest, Unstemmed(current.s ++ s))
            case _ :: vrest            => go(symbols, vrest, current)
            case _                     => go(next, List(), Unstemmed(current.s :+ "?"))
          }
        case _                      => current
      }
    }

    go(symbols.toList, vs.toList, Unstemmed(Seq()))
  }

  def stemSymbol(s: Bnf.Symbol): Stem[Option[Bnf.Symbol]] = {
    s match {
      case Bnf.Axiom(rule) => throw new RuntimeException("boom")
      case r: Rule         => Stem.map(stemRule(r), (r: Bnf.Symbol) => Some(r))
      case t: Token        => Stem.unit(stemToken(t))
    }
  }

  def stemToken(t: Bnf.Token): Option[Bnf.Token] = {
    Stemming.stemToken(t.name).map(stemmed => t.copy(name = stemmed))
  }

}
