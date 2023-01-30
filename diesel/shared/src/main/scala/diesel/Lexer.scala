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

package diesel

import diesel.Bnf.NonTerminal
import diesel.Dsl._
import diesel.Lexer.{Rule, TokenId}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.matching.Regex

object Lexer {

  sealed trait TokenId {
    def name: String

    def accept(tokenId: TokenId, identifiersOpt: Option[Identifier]): Boolean = this == tokenId
  }

  case object Error extends TokenId {
    def name: String = "Error"
  }

  case object Skip extends TokenId {
    def name: String = "Skip"
  }

  case object Eos extends TokenId {
    def name: String = "$"
  }

  case class IdentifiedToken[T](t: T) extends TokenId {
    def name: String = t.toString
  }

  case class IdentifierToken[T](t: T) extends TokenId {
    def name: String = t.toString
  }

  case class IdentifierOrKeywordToken[T](t: T) extends TokenId {
    def name: String = t.toString

    override def accept(tokenId: TokenId, identifiersOpt: Option[Identifier]): Boolean = {
      identifiersOpt match {
        case Some(identifiers) =>
          identifiers.identId == tokenId || identifiers.keywordIds.contains(tokenId)
        case _                 => false
      }
    }
  }

  case class ConceptId(c: Concept[_ <: Any]) extends TokenId {
    def name: String = c.name
  }

  class Input(var s: String) {
    private var pos: Int = 0

    def offset: Int = pos

    def scanPrefix(rule: Rule): Option[String] = rule.findPrefixOf(s)

    def eat(i: Int): String = {
      val (l, r) = s.splitAt(i)
      s = r
      pos += i
      l
    }

    def eos: Boolean = s.isEmpty
  }

  trait Scanner {
    def name: String
    def findPrefixOf(source: CharSequence): Option[String]
  }

  case class RegexScanner(regex: Regex) extends Scanner {
    override def name: String = regex.regex

    override def findPrefixOf(source: CharSequence): Option[String] = regex.findPrefixOf(source)
  }

  abstract class Rule(val scanner: Scanner, val priority: Int = 0) {
    def tokenId(text: String): TokenId
    def stylesOf(text: String): Seq[Style]
    def findPrefixOf(source: CharSequence): Option[String] = scanner.findPrefixOf(source)
    def isSkip: Boolean                                    = false
  }

  case class SimpleRule(
    override val scanner: Scanner,
    tokenId: TokenId,
    override val priority: Int = 0,
    var styles: Seq[Style] = Seq()
  ) extends Rule(scanner, priority) {
    override def tokenId(text: String): TokenId = tokenId

    override def stylesOf(text: String): Seq[Style] = styles

    override def isSkip: Boolean = tokenId == Skip
  }

  case class Identifier(
    override val scanner: Scanner,
    identId: TokenId,
    keywords: Map[String, TokenId],
    keywordIds: Set[TokenId],
    styles: Map[String, Seq[Style]],
    override val priority: Int = 0
  ) extends Rule(scanner, priority) {
    override def tokenId(text: String): TokenId = keywords.getOrElse(text, identId)

    override def stylesOf(text: String): Seq[Style] = styles.getOrElse(text, Seq())
  }

  case class Token(offset: Int, text: String, id: TokenId) {
    def length: Int = text.length
  }

  private def computeRules(
    dsl: Dsl,
    bnfRules: Seq[NonTerminal]
  ): (Seq[Rule], Map[TokenId, Rule]) = {
    val identScanner = dsl match {
      case identifiers: Identifiers =>
        Some(identifiers.identScanner)
      case _                        => None
    }

    val keywords: mutable.Map[String, TokenId]           = mutable.Map()
    val keywordToStyles: mutable.Map[String, Seq[Style]] = mutable.Map()

    var scanners: Map[Scanner, SimpleRule] = Map()
    var tokens: Map[TokenId, Rule]         = Map()
    var rules: Seq[Rule]                   = Seq(
      dsl match {
        case ws: Whitespaces => SimpleRule(ws.whitespacesScanner, Skip)
        case _               => SimpleRule(RegexScanner("\\s+".r), Skip)
      }
    )

    val identifiers: Option[Identifiers] = dsl match {
      case i: Identifiers => Some(i)
      case _              => None
    }

    def computeStr(text: String, styleOpt: Option[Style]): Seq[Rule] = {
      var isKeyword = false
      identifiers.foreach { i =>
        i.keywordScanner.findPrefixOf(text).foreach { prefix =>
          if (prefix == text) {
            if (!keywords.contains(prefix)) {
              keywords.put(prefix, IdentifiedToken(prefix))
              styleOpt.foreach(style => keywordToStyles.put(prefix, Seq(style)))
            } else {
              styleOpt.foreach(style => {
                val styles = keywordToStyles.get(prefix)
                if (styles.isEmpty)
                  keywordToStyles.put(prefix, Seq(style))
                else
                  keywordToStyles.put(prefix, styles.get ++ Seq(style))
              })
            }
            isKeyword = true
          }
        }
      }
      if (!isKeyword) {
        computeScanner(RegexScanner(Regex.quote(text).r), IdentifiedToken(text), styleOpt)
      } else
        Seq()
    }

    def computeScanner(re: Scanner, tokenId: Lexer.TokenId, styleOpt: Option[Style]): Seq[Rule] = {
      if (!scanners.contains(re)) {

        if (re.findPrefixOf("").isDefined) {
          throw new IllegalArgumentException(
            s"found scanner matching empty string ${re}, tokenId=${tokenId.name}"
          )
        }

        val rule = SimpleRule(re, tokenId)
        styleOpt.foreach { style =>
          rule.styles = rule.styles ++ Seq(style)
        }
        scanners = scanners ++ Map(re -> rule)
        tokens = tokens ++ Map(tokenId -> rule)
        Seq(rule)
      } else {
        val ruleOpt = scanners.get(re)
        ruleOpt.foreach { rule =>
          styleOpt.foreach { style =>
            rule.styles = rule.styles ++ Seq(style)
          }
        }
        Seq()
      }
    }

    dsl.getConcepts foreach {
      case concept: Concept[_] =>
        rules = concept.data match {
          case Some(ConceptData(ScannerRepr(s), _, _, _, styleOpt)) =>
            rules ++ computeScanner(s, ConceptId(concept), styleOpt)
          case _                                                    =>
            rules
        }
    }

    bnfRules foreach {
      case Bnf.Rule(_, productions) =>
        productions foreach { production =>
          production.symbols foreach {
            case Bnf.Token(_, tokenId, style) =>
              tokenId match {
                case tk: IdentifiedToken[_] =>
                  tk.t match {
                    case re: Regex => rules = rules ++ computeScanner(RegexScanner(re), tk, style)
                    case _         => rules = rules ++ computeStr(tk.name, style)
                  }
                case _                      =>
              }

            case _ =>
          }
        }
      case _                        =>
    }

    identScanner.foreach { scanner =>
      val identId          = IdentifierToken(scanner.name)
      val identOrKeywordId = IdentifierOrKeywordToken(scanner.name)
      val identRule        =
        Identifier(scanner, identId, keywords.toMap, keywords.values.toSet, keywordToStyles.toMap)
      rules = rules ++ Seq(identRule)
      tokens = tokens ++ Map(identId -> identRule)
      tokens = tokens ++ Map(identOrKeywordId -> identRule)
      keywords.foreach { case (_, keywordId) => tokens = tokens ++ Map(keywordId -> identRule) }
    }

    dsl match {
      case comments: Comments =>
        comments.commentScanners.foreach { regex =>
          rules = rules ++ Seq(SimpleRule(regex, Skip))
        }
      case _                  => ()
    }

    (rules, tokens)
  }

  def apply[T](dsl: Dsl, bnfRules: Seq[NonTerminal]): Lexer = {
    val (rules, tokens) = computeRules(dsl, bnfRules)
    new Lexer(rules, tokens)
  }
}

case class Lexer(lexerRules: Seq[Rule], tokenRules: Map[TokenId, Rule]) {

  import diesel.Lexer._

  private val skipRules = lexerRules.filter(_.isSkip)

  val identifiers: Option[Identifier] = lexerRules.collectFirst { case id: Identifier => id }

  def next(input: Input): Token = {
    nextWithStyles(input)._1
  }

  def nextWithStyles(input: Input): (Token, Seq[Style]) = {
    next(input, lexerRules)
  }

  def next(input: Input, tokens: Seq[TokenId]): Token = {
    val lexerRules = tokens.map(tokenId => tokenRules(tokenId)) ++ skipRules
    var res        = next(input, lexerRules, eatOnError = false)._1
    if (res.id == Error) {
      res = next(input)
    }
    res
  }

  def skip(input: Input): Token = {
    next(input, skipRules, eatOnError = false)._1
  }

  @tailrec
  private def next(
    input: Input,
    lexerRules: Seq[Rule],
    eatOnError: Boolean = true
  ): (Token, Seq[Style]) = {
    if (input.eos) {
      (Token(input.offset, "", Eos), Seq())
    } else {
      val initialAcc: Option[((Token, Seq[Style]), Int)] = None // (Token, Priority)
      val a                                              = lexerRules.foldLeft(initialAcc) { (acc, rule) =>
        val prefix = input.scanPrefix(rule)
        prefix match {
          case Some(s) =>
            acc match {
              case None =>
                Some(((Token(input.offset, s, rule.tokenId(s)), rule.stylesOf(s)), rule.priority))

              case Some(r @ ((t, _), p)) =>
                if (
                  (s.length > t.text.length) || ((s.length == t.text.length) && (rule.priority > p))
                ) {
                  Some(((Token(input.offset, s, rule.tokenId(s)), rule.stylesOf(s)), rule.priority))
                } else {
                  Some(r)
                }
            }

          case None =>
            acc
        }
      }
      a match {
        case None =>
          (Token(input.offset, if (eatOnError) input.eat(1) else "", Error), Seq())

        case Some(((Token(_, text, Skip), _), _)) =>
          input.eat(text.length)
          next(input, lexerRules, eatOnError)

        case Some((res @ (Token(_, text, _), _), _)) =>
          input.eat(text.length)
          res
      }
    }
  }

}
