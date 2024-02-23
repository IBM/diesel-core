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

import diesel.Bnf.Constraints.{Feature, Propagate}
import diesel.Dsl._
import diesel.Lexer._

import java.io.PrintStream
import scala.Console.println
import scala.collection.mutable
import scala.language.implicitConversions

object Bnf {

  object Implicits {
    implicit def dslValue[T](concept: Concept[T]): DslValue[T] = DslValue[T](concept)

    implicit def dslInstance[T](instance: Instance[T]): DslInstance[T] = DslInstance[T](instance)

    implicit def dslSyntax[T](syntax: Syntax[T]): DslSyntax[T] = DslSyntax[T](syntax)
  }

  sealed trait Symbol {
    val name: String

    def isRule: Boolean = false

    def isToken: Boolean = false
  }

  object Constraints {

    trait Feature {
      def merge(dot: Int, other: Feature): Feature

      def shift(prefix: Int): Feature

      def canPropagate: Boolean = false
    }

    object None extends Feature {
      override def merge(dot: Int, other: Feature): Feature = other

      override def shift(prefix: Int): Feature = this

      override def toString: String = "None"
    }

    object Incompatible extends Feature {
      override def merge(dot: Int, other: Feature): Feature = this

      override def shift(prefix: Int): Feature = this

      override def toString: String = "Incompatible"
    }

    case class Precedence(from: Int, to: Int, associativity: Associativity.Value, level: Int)
        extends Feature {

      override def merge(dot: Int, other: Feature): Feature = {
        other match {
          case None => this

          case Precedence(_, _, associativity, level) =>
            if (this.level > level)
              Incompatible
            else if (this.level < level)
              this
            else {
              if (dot < from) {
                if (this.associativity == Associativity.Left)
                  return this
              } else if (dot > to) {
                if (this.associativity == Associativity.Right)
                  return this
              }
              Incompatible
            }

          case Propagate(_) => this
        }
      }

      override def shift(prefix: Int): Feature =
        Precedence(prefix + from, prefix + to, associativity, level)

      override def toString: String = s"Precedence($from, $to, $associativity, $level)"
    }

    case class Propagate(from: Int) extends Feature {

      override def merge(dot: Int, other: Feature): Feature = {
        if (from == dot) other else None
      }

      override def shift(prefix: Int): Feature = Propagate(prefix + from)

      override def canPropagate: Boolean = true

      override def toString: String = s"Propagate($from)"
    }

  }

  case class Token(name: String, tokenId: Lexer.TokenId, style: Option[Style] = None)
      extends Symbol {
    def defaultValue: String =
      tokenId match {
        case ConceptId(concept) =>
          concept.data match {
            case Some(data) => data.defaultValueAsString
            case _          => name
          }
        case _                  => name
      }

    override def isToken: Boolean = true

    def accept(tokenId: Lexer.TokenId, ident: Option[Identifier]): Boolean =
      this.tokenId.accept(tokenId, ident)
  }

  type Action = (Context, Seq[Any]) => Any

  sealed trait DslElement

  case class DslAxiom[T](axiom: Dsl.Axiom[T]) extends DslElement

  case class DslValue[T](concept: Concept[T]) extends DslElement

  case class DslTarget[T](concept: Concept[T]) extends DslElement

  case class DslInstance[T](instance: Instance[T]) extends DslElement

  case class DslSyntax[T](syntax: Syntax[T]) extends DslElement

  case class DslBody(element: DslElement) extends DslElement

  class Production(
    var rule: Option[NonTerminal] = None,
    symbols0: Seq[Symbol],
    val action: Action,
    val element: Option[DslElement] = None,
    val feature: Feature = Constraints.None
  ) {
    val symbols: Array[Symbol] = symbols0.toArray
    val length: Int            = symbols.length
    def isEmpty: Boolean       = length == 0

    def getElement: Option[DslElement] = {
      element match {
        case Some(value) => value match {
            case Bnf.DslBody(element) => Some(element)
            case _                    => element
          }
        case None        => None
      }
    }

    def isDslElement: Boolean = {
      element match {
        case Some(value) => value match {
            case Bnf.DslBody(_) => false
            case _              => true
          }
        case None        => false
      }
    }
  }

  trait NonTerminal extends Symbol {
    def isAxiom: Boolean = false
  }

  case class Rule(name: String, var productions: Seq[Production] = Seq()) extends NonTerminal {
    override def isRule: Boolean = true

    def dump(ps: PrintStream): Unit = {
      ps.print(name)
      ps.print(" => ")
      ps.println()
      productions.foreach(p => {
        ps.print("\t|")
        p.symbols.foreach(s => {
          ps.print(" ")
          ps.print(s.name)
        })
        ps.println()
      })
    }
  }

  case class Axiom(rule: Rule) extends NonTerminal {
    val name: String           = rule.name ++ ".axiom"
    val production: Production = new Production(Some(this), Seq(rule), { (_, args) => args.head })

    override def isAxiom: Boolean = true

    def dump(ps: PrintStream): Unit = {
      ps.print(name)
      ps.print(" => ")
      ps.println()
      ps.print("\t|")
      production.symbols.foreach(s => {
        ps.print(" ")
        ps.print(s.name)
      })
      ps.println()
    }
  }

  implicit class RuleBuilder(rule: Rule) {

    def `>>`(other: Production): Rule = {
      rule.productions = rule.productions ++ Seq(other)
      other.rule = Some(rule)
      rule
    }

    def `>>`(other: Symbol, action: Action): Rule = {
      rule.productions = rule.productions ++ Seq(new Production(Some(rule), Seq(other), action))
      rule
    }

  }

  case class LeftAssoc(symbol: Symbol, level: Int)

  case class RightAssoc(symbol: Symbol, level: Int)

  case class NonAssoc(symbol: Symbol, level: Int)

  class ProductionBuilder(val symbols: Seq[Symbol], val feature: Feature) {

    def `~`(action: Action): Production = {
      new Production(None, symbols, action, None, feature)
    }
  }

  implicit class SymbolBuilder(symbol: Symbol) {

    private[diesel] var symbols: Seq[Symbol] = Seq(symbol)

    private[diesel] var feature: Feature = Constraints.None

    def `+`(other: Symbol): SymbolBuilder = {
      symbols = symbols ++ Seq(other)
      this
    }

    def `+`(other: LeftAssoc): SymbolBuilder = {
      symbols = symbols ++ Seq(other.symbol)
      addAssoc(Associativity.Left, other.level)
      this
    }

    def `+`(other: NonAssoc): SymbolBuilder = {
      symbols = symbols ++ Seq(other.symbol)
      addAssoc(Associativity.None, other.level)
      this
    }

    def `+`(other: RightAssoc): SymbolBuilder = {
      symbols = symbols ++ Seq(other.symbol)
      addAssoc(Associativity.Right, other.level)
      this
    }

    private def addAssoc(associativity: Associativity.Value, level: Int): SymbolBuilder = {
      val to      = symbols.length - 1
      var from    = to
      var current = symbols.reverse.tail
      while (current.nonEmpty && current.head.isToken) {
        from -= 1
        current = current.tail
      }
      feature = Constraints.Precedence(from, to, associativity, level)
      this
    }

    def build: ProductionBuilder = new ProductionBuilder(this.symbols, this.feature)
  }

  implicit def symbolBuilderToProductionBuilder(builder: SymbolBuilder): ProductionBuilder =
    builder.build

  implicit def stringToRuleBuilder(name: String): RuleBuilder = new RuleBuilder(Rule(name))

  implicit def stringToRule(name: String): Rule = Rule(name)

  private def computeRules(dsl: Dsl): Seq[NonTerminal] = {

    var rules: Map[String, NonTerminal] = Map()

    case class Partial(symbols: Seq[Symbol], feature: Feature = Constraints.None) {

      def `++`(other: Partial): Partial = {
        Partial(symbols ++ other.symbols, feature).merge(symbols.length, other.feature)
      }

      def merge(prefix: Int, feature: Feature): Partial = {
        Partial(
          symbols,
          this.feature match {
            case Constraints.None                   => feature.shift(prefix)
            case Constraints.Propagate(_)           =>
              feature match {
                case Constraints.None                   => this.feature
                case Constraints.Propagate(_)           => Constraints.None
                case Constraints.Precedence(_, _, _, _) => feature.shift(prefix)
                case Constraints.Incompatible           => Constraints.Incompatible
              }
            case Constraints.Precedence(_, _, _, _) =>
              feature match {
                case Constraints.None                   => this.feature
                case Constraints.Propagate(_)           => this.feature
                case Constraints.Precedence(_, _, _, _) => this.feature
                case Constraints.Incompatible           => Constraints.Incompatible
              }
            case Constraints.Incompatible           => Constraints.Incompatible
          }
        )
      }
    }

    def mapInstanceProduction(
      owner: Rule,
      production: InstanceProduction,
      style: Option[Style]
    ): Partial = {
      production match {
        case IPStr(s) =>
          Partial(Seq(Bnf.Token(s, IdentifiedToken(s), style)))

        case IPAnd2(i1, i2) =>
          mapInstanceProduction(owner, i1, style) ++ mapInstanceProduction(owner, i2, style)

        case IPAnd3(i1, i2, i3) =>
          mapInstanceProduction(owner, i1, style) ++ mapInstanceProduction(
            owner,
            i2,
            style
          ) ++ mapInstanceProduction(
            owner,
            i3,
            style
          )
      }
    }

    def addProduction(
      rule: Rule,
      partial: Partial,
      action: Action,
      element: Option[DslElement] = None
    ): Unit = {
      rule >> new Production(Some(rule), partial.symbols, action, element, partial.feature)
    }

    def mapAction[T](
      rule: Rule,
      applicable: Applicable[T],
      partial: Partial,
      element: Option[DslElement] = None
    ) = {
      val symbols = partial.symbols
      symbols match {
        case List(_) =>
          rule >> new Production(
            Some(rule),
            symbols,
            { (context, args) =>
              applicable.applyDynamic(context, args.head)
            },
            element,
            Propagate(0)
          )

        case List(_, _) =>
          rule >> new Production(
            Some(rule),
            symbols,
            { (context, args) =>
              args match {
                case Seq(a1, a2) =>
                  applicable.applyDynamic(context, (a1, a2))
              }
            },
            element,
            partial.feature
          )

        case List(_, _, _) =>
          rule >> new Production(
            Some(rule),
            symbols,
            { (context, args) =>
              args match {
                case Seq(a1, a2, a3) =>
                  applicable.applyDynamic(context, (a1, a2, a3))
              }
            },
            element,
            partial.feature
          )

        case List(_, _, _, _) =>
          rule >> new Production(
            Some(rule),
            symbols,
            { (context, args) =>
              args match {
                case Seq(a1, a2, a3, a4) =>
                  applicable.applyDynamic(context, (a1, a2, a3, a4))
              }
            },
            element,
            partial.feature
          )

        case List(_, _, _, _, _) =>
          rule >> new Production(
            Some(rule),
            symbols,
            { (context, args) =>
              args match {
                case Seq(a1, a2, a3, a4, a5) =>
                  applicable.applyDynamic(context, (a1, a2, a3, a4, a5))
              }
            },
            element,
            partial.feature
          )

        case List(_, _, _, _, _, _) =>
          rule >> new Production(
            Some(rule),
            symbols,
            { (context, args) =>
              args match {
                case Seq(a1, a2, a3, a4, a5, a6) =>
                  applicable.applyDynamic(context, (a1, a2, a3, a4, a5, a6))
              }
            },
            element,
            partial.feature
          )

        case List(_, _, _, _, _, _, _) =>
          rule >> new Production(
            Some(rule),
            symbols,
            { (context, args) =>
              args match {
                case Seq(a1, a2, a3, a4, a5, a6, a7) =>
                  applicable.applyDynamic(context, (a1, a2, a3, a4, a5, a6, a7))
              }
            },
            element,
            partial.feature
          )

        case List(_, _, _, _, _, _, _, _) =>
          rule >> new Production(
            Some(rule),
            symbols,
            { (context, args) =>
              args match {
                case Seq(a1, a2, a3, a4, a5, a6, a7, a8) =>
                  applicable.applyDynamic(context, (a1, a2, a3, a4, a5, a6, a7, a8))
              }
            },
            element,
            partial.feature
          )
        case _                            =>
          rule >> new Production(
            Some(rule),
            symbols,
            { (context, args) =>
              applicable.applyDynamic(context, args)
            },
            element,
            partial.feature
          )
      }
    }

    case class GrammarContext(
      concept: Option[Dsl.Concept[_ <: Any]],
      multiple: Option[Boolean],
      article: Option[Article],
      plural: Option[Boolean],
      partitive: Option[Boolean]
    ) {

      def derive(concept: Concept[_ <: Any], multiple: Boolean): GrammarContext =
        GrammarContext(Some(concept), Some(multiple), article, plural, partitive)

      def derive(
        concept: Concept[_ <: Any],
        multiple: Boolean,
        vc: SPVerbalizationContext
      ): GrammarContext =
        GrammarContext(
          Some(concept),
          Some(multiple),
          vc.article match {
            case Some(value) => Some(value)
            case None        => this.article
          },
          vc.plural match {
            case Some(value) => Some(value)
            case None        => this.plural
          },
          vc.partitive match {
            case Some(value) => Some(value)
            case None        => this.partitive
          }
        )

      def propagate(first: Boolean): GrammarContext =
        if (first) this else GrammarContext(concept, multiple, None, None, None)

      override def toString: String = s"${concept match {
          case Some(value) => value.name
          case None        => "_"
        }},${multiple match {
          case Some(true)  => "MULTIPLE"
          case Some(false) => "SINGLE"
          case None        => "_"
        }},${article match {
          case Some(value) => value.name
          case None        => "_"
        }},${plural match {
          case Some(value) => if (value) "PLURAL" else "SINGULAR"
          case None        => "_"
        }},${partitive match {
          case Some(value) => if (value) "PARTITIVE" else "NON-PARTITIVE"
          case None        => "_"
        }}"
    }

    def ruleNameOf(prefix: String, ctx: GrammarContext, suffix: String): String = {
      s"$prefix[$ctx]${if (suffix.isEmpty) "" else s".$suffix"}"
    }

    def getOrCreateRule(name: String, suffix: String = ""): Rule = {
      val ruleName = s"$name${if (suffix.isEmpty) "" else s".$suffix"}"
      rules.getOrElse(
        ruleName, {
          val rule = Rule(ruleName)
          rules += ruleName -> rule
          rule
        }
      ).asInstanceOf[Rule]
    }

    def getOrCreateRuleWithContext(name: String, ctx: GrammarContext, suffix: String = ""): Rule = {
      getOrCreateRule(ruleNameOf(name, ctx, suffix))
    }

    val processingQueue: mutable.Queue[() => Unit] = mutable.Queue()
    var generated: mutable.Set[String]             = mutable.Set()

    def forwardGeneration(rule: Rule, task: () => Unit): Unit = {
      if (!generated.contains(rule.name)) {
        processingQueue.enqueue(task)
        generated = generated ++ Seq(rule.name)
      }
    }

    def verbalizations(dsl: Dsl): Option[Verbalizations] = {
      dsl match {
        case dsl: Verbalizations => Some(dsl)
        case _                   => None
      }
    }

    var counter: Int = 0

    def verbalizeSubject[T](
      owner: Rule,
      ctx: GrammarContext,
      text: String,
      v: Verbalizer
    ): Partial = {
      val context     = VerbalizationContext(
        article = ctx.article.getOrElse(NoArticle),
        plural = ctx.plural.orElse(ctx.multiple).getOrElse(false),
        partitive = ctx.partitive.getOrElse(false)
      )
      val verbalized  = v.verbalize(context, text).split(" ")
        .map { token =>
          Partial(Seq(Bnf.Token(token, IdentifiedToken(token))))
        }
        .reduce((p1, p2) => p1 ++ p2)
      val subjectRule = getOrCreateRule(owner.name, "subject")
      mapAction(
        subjectRule,
        (_: Context, args: Any) => {
          args
        },
        verbalized
      )
      Partial(Seq(subjectRule))
    }

    def mapSyntaxProduction[T](
      owner: Rule,
      production: SyntaxProduction[T],
      ctx: GrammarContext,
      element: Option[DslElement] = None,
      first: Boolean = true
    ): Partial = {
      production match {
        case SPStr(s, style) =>
          Partial(Seq(Bnf.Token(s, IdentifiedToken(s), style)))

        case SPRegex(r, style) =>
          Partial(Seq(Bnf.Token(r.regex, IdentifiedToken(r), style)))

        case SPIdentifier(s, false) =>
          Partial(Seq(Bnf.Token(s, IdentifierToken(s))))

        case SPIdentifier(s, true) =>
          Partial(Seq(Bnf.Token(s, IdentifierOrKeywordToken(s))))

        case SPSubject(str) =>
          // verbalize str and split
          verbalizations(dsl) match {
            case Some(v) =>
              verbalizeSubject(owner, ctx, str.text, v.verbalizer)
            case None    =>
              splitText(str.text)
          }

        case SPAnd2(i1, i2) =>
          mapSyntaxProduction(owner, i1, ctx.propagate(first), None, first) ++ mapSyntaxProduction(
            owner,
            i2,
            ctx.propagate(false),
            None,
            first = false
          )

        case SPAnd3(i1, i2, i3) =>
          mapSyntaxProduction(owner, i1, ctx.propagate(first), None, first) ++ mapSyntaxProduction(
            owner,
            i2,
            ctx.propagate(false),
            None,
            first = false
          ) ++ mapSyntaxProduction(
            owner,
            i3,
            ctx.propagate(false),
            None,
            first = false
          )

        case SPAnd4(i1, i2, i3, i4) =>
          mapSyntaxProduction(owner, i1, ctx.propagate(first), None, first) ++ mapSyntaxProduction(
            owner,
            i2,
            ctx.propagate(false),
            None,
            first = false
          ) ++ mapSyntaxProduction(
            owner,
            i3,
            ctx.propagate(false),
            None,
            first = false
          ) ++ mapSyntaxProduction(owner, i4, ctx.propagate(false), None, first = false)

        case SPAnd5(i1, i2, i3, i4, i5) =>
          mapSyntaxProduction(owner, i1, ctx.propagate(first), None, first) ++ mapSyntaxProduction(
            owner,
            i2,
            ctx.propagate(false),
            None,
            first = false
          ) ++ mapSyntaxProduction(
            owner,
            i3,
            ctx.propagate(false),
            None,
            first = false
          ) ++ mapSyntaxProduction(
            owner,
            i4,
            ctx.propagate(false),
            None,
            first = false
          ) ++ mapSyntaxProduction(
            owner,
            i5,
            ctx.propagate(false),
            None,
            first = false
          )

        case SPAnd6(i1, i2, i3, i4, i5, i6) =>
          mapSyntaxProduction(owner, i1, ctx.propagate(first), None, first) ++ mapSyntaxProduction(
            owner,
            i2,
            ctx.propagate(false),
            None,
            first = false
          ) ++ mapSyntaxProduction(
            owner,
            i3,
            ctx.propagate(false),
            None,
            first = false
          ) ++ mapSyntaxProduction(
            owner,
            i4,
            ctx.propagate(false),
            None,
            first = false
          ) ++ mapSyntaxProduction(
            owner,
            i5,
            ctx.propagate(false),
            None,
            first = false
          ) ++ mapSyntaxProduction(
            owner,
            i6,
            ctx.propagate(false),
            None,
            first = false
          )

        case SPAnd7(i1, i2, i3, i4, i5, i6, i7) =>
          mapSyntaxProduction(owner, i1, ctx.propagate(first), None, first) ++ mapSyntaxProduction(
            owner,
            i2,
            ctx.propagate(false),
            None,
            first = false
          ) ++ mapSyntaxProduction(
            owner,
            i3,
            ctx.propagate(false),
            None,
            first = false
          ) ++ mapSyntaxProduction(
            owner,
            i4,
            ctx.propagate(false),
            None,
            first = false
          ) ++ mapSyntaxProduction(
            owner,
            i5,
            ctx.propagate(false),
            None,
            first = false
          ) ++ mapSyntaxProduction(
            owner,
            i6,
            ctx.propagate(false),
            None,
            first = false
          ) ++ mapSyntaxProduction(owner, i7, ctx.propagate(false), None, first = false)

        case SPAnd8(i1, i2, i3, i4, i5, i6, i7, i8) =>
          mapSyntaxProduction(owner, i1, ctx.propagate(first), None, first) ++ mapSyntaxProduction(
            owner,
            i2,
            ctx.propagate(false),
            None,
            first = false
          ) ++ mapSyntaxProduction(
            owner,
            i3,
            ctx.propagate(false),
            None,
            first = false
          ) ++ mapSyntaxProduction(
            owner,
            i4,
            ctx.propagate(false),
            None,
            first = false
          ) ++ mapSyntaxProduction(
            owner,
            i5,
            ctx.propagate(false),
            None,
            first = false
          ) ++ mapSyntaxProduction(
            owner,
            i6,
            ctx.propagate(false),
            None,
            first = false
          ) ++ mapSyntaxProduction(
            owner,
            i7,
            ctx.propagate(false),
            None,
            first = false
          ) ++ mapSyntaxProduction(
            owner,
            i8,
            ctx.propagate(false),
            None,
            first = false
          )

        case SPAndN(ps) =>
          ps.zipWithIndex.map { case (p, n) =>
            val mustBePropagated = n == 0 && first
            mapSyntaxProduction(owner, p, ctx.propagate(mustBePropagated), None, mustBePropagated)
          }
            .foldLeft(Partial(Seq())) { (acc, p) => acc ++ p }

        case SPExprRef(concept, exprTypes) =>
          val newCtx = ctx.derive(concept, multiple = false)
          val item   = getOrCreateRuleWithContext("expr", newCtx, exprTypes.toString)
          forwardGeneration(item, () => generateExpr(item, exprTypes, newCtx))
          Partial(Seq(item), Propagate(0))

        case SPMultiple(ref, vc) =>
          val newCtx = vc
            .map { c => ctx.derive(ref.c, multiple = true, c) }
            .getOrElse(ctx.derive(ref.c, multiple = true))
          val item   = getOrCreateRuleWithContext("expr", newCtx, ref.exprTypes.toString)
          forwardGeneration(item, () => generateExpr(item, ref.exprTypes, newCtx))
          Partial(Seq(item), Propagate(0))

        case SPRefWithContext(ref, vc) =>
          val newCtx = ctx.derive(ref.c, multiple = false, vc)
          val item   = getOrCreateRuleWithContext("expr", newCtx, ref.exprTypes.toString)
          forwardGeneration(item, () => generateExpr(item, ref.exprTypes, newCtx))
          Partial(Seq(item), Propagate(0))

        case SPConceptRef(c, _, m, vc) =>
          val newCtx = ctx.derive(c, multiple = m, vc)
          val target = getOrCreateRuleWithContext("target", newCtx)
          forwardGeneration(target, () => generateTarget(target, newCtx))
          Partial(Seq(target), Propagate(0))

        case SPRuleRef(syntax) =>
          val newCtx = syntax match {
            case SyntaxTyped(_, concept, _, _, _, _) => ctx.derive(concept, multiple = false)
            case SyntaxMulti(_, concept, _, _, _)    => ctx.derive(concept, multiple = true)
            case _                                   => ctx
          }
          val item   = getOrCreateRuleWithContext("syntax", newCtx, syntax.name)
          forwardGeneration(item, () => generateSyntax(syntax, item, newCtx))
          Partial(Seq(item), Propagate(0))

        case SPLazyRuleRef(_, r) =>
          val syntax = r()
          val newCtx = syntax match {
            case SyntaxTyped(_, concept, _, _, _, _) => ctx.derive(concept, multiple = false)
            case SyntaxMulti(_, concept, _, _, _)    => ctx.derive(concept, multiple = true)
            case _                                   => ctx
          }
          val item   = getOrCreateRuleWithContext("syntax", newCtx, syntax.name)
          forwardGeneration(item, () => generateSyntax(syntax, item, newCtx))
          Partial(Seq(item), Propagate(0))

        case SPMapped(p, _) =>
          val mapRule = getOrCreateRule(owner.name, "map." + counter)
          counter += 1
          mapAction(
            mapRule,
            p,
            mapSyntaxProduction(mapRule, p, ctx, None, first),
            element match {
              case Some(value) => Some(DslBody(value))
              case None        => None
            }
          )
          Partial(Seq(mapRule))

        case SPAssoc(p, associativity, level) =>
          val partial = mapSyntaxProduction(owner, p, ctx, None, first)
          val to      = partial.symbols.length - 1
          partial.merge(0, Constraints.Precedence(0, to, associativity, level))

        case SPOpt(p) =>
          val postfix  = "opt." + counter
          counter += 1
          val optRule  = getOrCreateRule(owner.name, postfix)
          val itemRule =
            mapAction(
              getOrCreateRule(owner.name, postfix + ".item"),
              p,
              mapSyntaxProduction(optRule, p, ctx, None, first)
            )
          addProduction(
            optRule,
            Partial(Seq(itemRule), Propagate(0)),
            { (c, args) => Some(args.head) }
          )
          optRule >> new Production(Some(optRule), Seq(), { (_, _) => None })
          Partial(Seq(optRule))

        case SPRep(p, zeroIncluded) =>
          val postfix  = "rep." + counter
          counter += 1
          val repRule  = getOrCreateRule(owner.name, postfix)
          val itemRule =
            mapAction(
              getOrCreateRule(owner.name, postfix + ".item"),
              p,
              mapSyntaxProduction(repRule, p, ctx, None, first)
            )
          addProduction(
            repRule,
            Partial(Seq(itemRule) ++ Seq(repRule)),
            { (_, args) =>
              Seq(args.head) ++ args.tail.head.asInstanceOf[Seq[Any]]
            }
          )
          if (zeroIncluded)
            repRule >> new Production(
              Some(repRule),
              Seq(),
              { (_, _) =>
                Seq()
              }
            )
          else
            addProduction(
              repRule,
              Partial(Seq(itemRule)),
              { (_, args) =>
                Seq(args.head)
              }
            )
          Partial(Seq(repRule))

        case SPOr(e1, e2) =>
          val choiceRule = getOrCreateRule(owner.name, "choice")

          val left  = mapSyntaxProduction(choiceRule, e1, ctx, None, first)
          val right = mapSyntaxProduction(choiceRule, e2, ctx, None, first)

          mapAction(
            choiceRule,
            new Applicable[Any] {
              override def applyDynamic(c: Context, args: Any): Any = {
                Left(e1.applyDynamic(c, args))
              }
            },
            left,
            None
          )
          mapAction(
            choiceRule,
            new Applicable[Any] {
              override def applyDynamic(c: Context, args: Any): Any = {
                Right(e2.applyDynamic(c, args))
              }
            },
            right,
            None
          )

          Partial(Seq(choiceRule), Propagate(0))
      }
    }

    def splitText(text: String): Partial = {
      Partial(
        text.split(" ")
          .toList
          .map(token => Bnf.Token(token, IdentifiedToken(token)))
      )
    }

    def generateAxioms(): Unit = {
      dsl.getAxioms.foreach {
        case a: Dsl.Axiom[_] =>
          val ctx   = GrammarContext(None, None, None, None, None)
          val rule  = getOrCreateRuleWithContext(a.name, ctx)
          mapAction(
            rule,
            a.production,
            mapSyntaxProduction(rule, a.production, ctx),
            Some(DslAxiom(a))
          )
          val axiom = Axiom(rule)
          rules += axiom.name -> axiom
        case _               =>
      }
    }

    def generateValue(rule: Rule, ctx: GrammarContext): Boolean = {
      ctx.multiple foreach { multiple =>
        if (!multiple) {
          ctx.concept foreach { concept =>
            if (dsl.acceptExpr(Expressions.Values, concept, multiple)) {
              concept.data foreach { data =>
                rule >> new Production(
                  Some(rule),
                  Seq(Bnf.Token(concept.name, ConceptId(concept), data.style)),
                  { (context, args) =>
                    try {
                      data.valueOf(context, args.head.asInstanceOf[Lexer.Token])
                    } catch {
                      case e: Throwable =>
                        // TODO Provide a way to add an error on context
                        e.printStackTrace()
                        data.defaultValue
                    }
                  },
                  Some(DslValue(concept))
                )
              }
            }
            // Hierarchy
            dsl.subConceptsOf(concept) foreach {
              case subConcept: Concept[_] =>
                val newCtx   = ctx.derive(subConcept, multiple = false)
                val subValue = getOrCreateRuleWithContext("value", newCtx)
                forwardGeneration(subValue, () => generateValue(subValue, newCtx))
                rule >> new Production(
                  Some(rule),
                  Seq(subValue),
                  { (_, args) => args.head },
                  None,
                  Propagate(0)
                )
            }
          }
        }
      }
      rule.productions.nonEmpty
    }

    def generateInstances(rule: Bnf.Rule, ctx: GrammarContext): Boolean = {
      ctx.multiple foreach { multiple =>
        if (!multiple) {
          ctx.concept foreach { concept =>
            if (dsl.acceptExpr(Expressions.Instances, concept, multiple)) {
              dsl.getInstances.foreach(instance =>
                if (instance.concept == concept) {
                  instance match {
                    case instance: Instance[_] =>
                      addProduction(
                        rule,
                        mapInstanceProduction(rule, instance.production, instance.style),
                        { (context, _) => instance.value(context) },
                        Some(DslInstance(instance))
                      )
                  }
                }
              )
            }
            // Hierarchy
            dsl.subConceptsOf(concept) foreach {
              case subConcept: Concept[_] =>
                val newCtx    = ctx.derive(subConcept, multiple = false)
                val instances = getOrCreateRuleWithContext("instances", newCtx)
                forwardGeneration(instances, () => generateInstances(instances, newCtx))
                rule >> new Production(
                  Some(rule),
                  Seq(instances),
                  { (_, args) => args.head },
                  None,
                  Propagate(0)
                )
            }
          }
        }
      }
      rule.productions.nonEmpty
    }

    def generateHierarchicalSyntaxes(
      rule: Bnf.Rule,
      exprTypes: Expressions.Types,
      ctx: GrammarContext
    ): Boolean = {

      def addSyntax(syntax: Syntax[_]): Rule = {
        val syntaxRule = getOrCreateRuleWithContext("syntax", ctx, syntax.name + ".hierarchical")
        forwardGeneration(syntaxRule, () => generateSyntax(syntax, syntaxRule, ctx))
        rule >> new Production(
          Some(rule),
          Seq(syntaxRule),
          { (_, args) => args.head },
          None,
          Propagate(0)
        )
      }

      ctx.multiple foreach { multiple =>
        ctx.concept foreach { concept =>
          if (!multiple) {
            dsl.getSyntaxes.collect { case s: SyntaxTyped[_] => s }.filter(s =>
              s.expression && s.hierarchical
            ) foreach {
              syntax =>
                if (syntax.concept == concept) {
                  if (dsl.acceptExpr(Expressions.Syntaxes, syntax.concept, multiple = false))
                    addSyntax(syntax)
                }
            }
          } else {
            dsl.getSyntaxes.collect { case s: SyntaxMulti[_, _] => s }.filter(s =>
              s.hierarchical
            ) foreach {
              syntax =>
                if (syntax.concept == concept) {
                  if (dsl.acceptExpr(Expressions.Syntaxes, syntax.concept, multiple = true))
                    addSyntax(syntax)
                }
            }
          }

          // Hierarchy
          dsl.subConceptsOf(concept) foreach {
            case subConcept: Concept[_] =>
              val newCtx   = ctx.derive(subConcept, multiple)
              val syntaxes = getOrCreateRuleWithContext("syntaxes", newCtx, "hierarchical")
              forwardGeneration(
                syntaxes,
                () => generateHierarchicalSyntaxes(syntaxes, exprTypes, newCtx)
              )
              rule >> new Production(
                Some(rule),
                Seq(syntaxes),
                { (_, args) => args.head },
                None,
                Propagate(0)
              )
          }
        }
      }
      rule.productions.nonEmpty
    }

    def generateSyntaxes(
      rule: Bnf.Rule,
      exprTypes: Expressions.Types,
      ctx: GrammarContext
    ): Boolean = {
      def addSyntax(syntax: Syntax[_]): Rule = {
        val syntaxRule = getOrCreateRuleWithContext("syntax", ctx, syntax.name)
        forwardGeneration(syntaxRule, () => generateSyntax(syntax, syntaxRule, ctx))
        rule >> new Production(
          Some(rule),
          Seq(syntaxRule),
          { (_, args) => args.head },
          None,
          Propagate(0)
        )
      }

      ctx.multiple foreach { multiple =>
        ctx.concept foreach { concept =>
          if (!multiple) {
            dsl.getSyntaxes.collect { case s: SyntaxTyped[_] => s }.filter(s =>
              s.expression && !s.hierarchical
            ) foreach {
              syntax =>
                if (dsl.isSubtypeOf(syntax.concept, concept)) {
                  if (dsl.acceptExpr(Expressions.Syntaxes, syntax.concept, multiple = false))
                    addSyntax(syntax)
                }
            }
          } else {
            dsl.getSyntaxes.collect { case s: SyntaxMulti[_, _] => s }.filter(s =>
              !s.hierarchical
            ) foreach {
              syntax =>
                if (dsl.isSubtypeOf(syntax.concept, concept)) {
                  if (dsl.acceptExpr(Expressions.Syntaxes, syntax.concept, multiple = true))
                    addSyntax(syntax)
                }
            }
          }

          // Generic syntaxes
          dsl.getGenericSyntaxes.foreach {
            case genericSyntax: SyntaxGeneric[_]         =>
              if (!multiple) {
                if (dsl.acceptExpr(Expressions.Syntaxes, concept, multiple = false))
                  genericSyntax.apply(concept, exprTypes, dsl, addSyntax)
              }
            case genericSyntax: SyntaxGenericMulti[_, _] =>
              if (multiple) {
                if (dsl.acceptExpr(Expressions.Syntaxes, concept, multiple = true))
                  genericSyntax.apply(concept, exprTypes, dsl, addSyntax)
              }
          }
        }
      }
      rule.productions.nonEmpty
    }

    def generateTarget(rule: Rule, ctx: GrammarContext): Boolean = {
      ctx.multiple foreach { multiple =>
        ctx.concept foreach { concept =>
          if (dsl.acceptExpr(Expressions.Targets, concept, multiple)) {
            val p = verbalizations(dsl).map { v =>
              val verbCtx = VerbalizationContext(
                article = ctx.article.getOrElse(NoArticle),
                plural = ctx.plural.getOrElse(multiple),
                partitive = ctx.partitive.getOrElse(false)
              )
              splitText(v.verbalizer.verbalize(verbCtx, concept))
            }.getOrElse(splitText(concept.name))
            rule >> new Production(
              Some(rule),
              p.symbols,
              { (_, _) => concept },
              Some(DslTarget(concept))
            )
          }
          // Hierarchy
          dsl.subConceptsOf(concept) foreach {
            case subConcept: Concept[_] =>
              val newCtx   = ctx.derive(subConcept, multiple)
              val subValue = getOrCreateRuleWithContext("target", newCtx)
              forwardGeneration(subValue, () => generateTarget(subValue, newCtx))
              rule >> new Production(
                Some(rule),
                Seq(subValue),
                { (_, args) => args.head },
                None,
                Propagate(0)
              )
          }
        }
      }
      rule.productions.nonEmpty
    }

    def generateExpr(rule: Rule, exprTypes: Expressions.Types, ctx: GrammarContext): Boolean = {
      ctx.multiple foreach { multiple =>
        ctx.concept foreach { _ =>
          if (!multiple) {
            // Value
            if (exprTypes.has(Expressions.Values)) {
              val value = getOrCreateRuleWithContext("value", ctx)
              forwardGeneration(value, () => generateValue(value, ctx))
              rule >> new Production(
                Some(rule),
                Seq(value),
                { (_, args) => args.head },
                None,
                Propagate(0)
              )
            }
            // Instances
            if (exprTypes.has(Expressions.Instances)) {
              val instances = getOrCreateRuleWithContext("instances", ctx)
              forwardGeneration(instances, () => generateInstances(instances, ctx))
              rule >> new Production(
                Some(rule),
                Seq(instances),
                { (_, args) => args.head },
                None,
                Propagate(0)
              )
            }
          }
          // Syntaxes
          if (exprTypes.has(Expressions.Syntaxes)) {
            val syntaxes = getOrCreateRuleWithContext("syntaxes", ctx)
            forwardGeneration(syntaxes, () => generateSyntaxes(syntaxes, exprTypes, ctx))
            rule >> new Production(
              Some(rule),
              Seq(syntaxes),
              { (_, args) => args.head },
              None,
              Propagate(0)
            )
          }
          // Hierarchical Syntaxes
          if (exprTypes.has(Expressions.Syntaxes)) {
            val syntaxes = getOrCreateRuleWithContext("syntaxes", ctx, suffix = "hierarchical")
            forwardGeneration(
              syntaxes,
              () => generateHierarchicalSyntaxes(syntaxes, exprTypes, ctx)
            )
            rule >> new Production(
              Some(rule),
              Seq(syntaxes),
              { (_, args) => args.head },
              None,
              Propagate(0)
            )
          }
        }
      }
      rule.productions.nonEmpty
    }

    def generateUntypedSyntax[T](syntax: Syntax[T], rule: Rule, ctx: GrammarContext): Unit = {
      val element = Some(DslSyntax(syntax))
      mapAction(
        rule,
        syntax.production,
        mapSyntaxProduction(rule, syntax.production, ctx, element),
        element
      )
    }

    def generateTypedSyntax[T](syntax: Syntax[T], rule: Rule, ctx: GrammarContext): Unit = {
      val element = Some(DslSyntax(syntax))
      mapAction(
        rule,
        syntax.production,
        mapSyntaxProduction(rule, syntax.production, ctx, element),
        element
      )
    }

    def generateSyntax(syntax: Syntax[_], rule: Rule, ctx: GrammarContext): Unit = {
      syntax match {
        case _: SyntaxUntyped[_]  =>
          generateUntypedSyntax(syntax, rule, ctx)
        case _: SyntaxTyped[_]    =>
          generateTypedSyntax(syntax, rule, ctx)
        case _: SyntaxMulti[_, _] =>
          generateTypedSyntax(syntax, rule, ctx)
      }
    }

    def cleanGrammar(): Unit = {
      var rulesToRemove: Set[String] = Set()
      var cont                       = false
      do {
        rules.values.foreach {
          case rule: Rule   =>
            if (rule.productions.isEmpty)
              rulesToRemove = rulesToRemove ++ Seq(rule.name)
          case axiom: Axiom =>
            if (axiom.rule.productions.isEmpty)
              rulesToRemove = rulesToRemove ++ Seq(axiom.name)
          case _            =>
        }
        cont = false;
        if (rulesToRemove.nonEmpty) {
          rules.values.foreach {
            case rule: Rule =>
              var productionsToRemove: Seq[Production] = Seq()
              rule.productions.foreach(p => {
                if (
                  p.symbols.exists {
                    case r: Rule => rulesToRemove.contains(r.name)
                    case _       => false
                  }
                ) {
                  productionsToRemove = productionsToRemove ++ Seq(p)
                }
              })
              if (productionsToRemove.nonEmpty) {
                rule.productions = rule.productions.filter(p => !productionsToRemove.contains(p))
                cont = true
              }

            case _ =>
          }
          rulesToRemove.foreach(ruleName => rules = rules - ruleName)
          rulesToRemove = Set()
        }
      } while (cont)
    }

    def dumpGrammar(): Unit = {
      rules.values.foreach {
        case r: Rule  =>
          r.dump(Console.out)
          println()
        case a: Axiom =>
          a.dump(Console.out)
          println()
        case _        =>
      }
    }

    def generateGrammar(): Seq[NonTerminal] = {
      generateAxioms()
      while (processingQueue.nonEmpty) {
        val task = processingQueue.dequeue()
        task()
      }
      cleanGrammar()
      if (System.getProperty("diesel.dumpbnf") == "true") {
        dumpGrammar()
      }

      rules.values.toSeq
    }

    generateGrammar()
  }

  def apply[T](dsl: Dsl): Bnf = {
    val rules: Seq[NonTerminal] = computeRules(dsl)
    val bnf                     = Bnf(Lexer(dsl, rules), rules)
    Option(System.getProperty("diesel.dumpbnf.html")) foreach { path =>
      BnfHtml.dump(bnf, path)
    }
    bnf
  }

}

import diesel.Bnf._

case class Bnf(lexer: Lexer, rules: Seq[NonTerminal]) {

  def axioms: Seq[Bnf.Axiom] = {
    rules.foldLeft(Seq[Bnf.Axiom]()) { (acc, r) =>
      r match {
        case a @ Bnf.Axiom(_) =>
          acc ++ Seq(a)
        case _                =>
          acc
      }
    }
  }
}
