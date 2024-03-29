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

import diesel.Bnf.Constraints.Feature
import diesel.Bnf.{Constraints, DslAxiom, DslBody, DslElement, DslSyntax, Production, Token}

import scala.collection.mutable

case class CompletionProposal(
  element: Option[DslElement],
  text: String,
  replace: Option[(Int, Int)] = None,
  userData: Option[Any] = None,
  documentation: Option[String] = None
)

trait CompletionFilter {
  def filterProposals(
    tree: GenericTree,
    offset: Int,
    node: Option[GenericNode],
    proposals: Seq[CompletionProposal]
  ): Seq[CompletionProposal]
}

trait CompletionProvider {
  def getProposals(
    element: Option[DslElement],
    tree: GenericTree,
    offset: Int,
    node: Option[GenericNode]
  ): Seq[CompletionProposal]
}

// TODO CompletionComputeFilter[T]
trait CompletionComputeFilter {
  def beginVisit(predictionState: PredictionState): Boolean
  def continueVisit(element: DslElement): Boolean
  def endVisit(candidates: Seq[CompletionProposal]): Seq[CompletionProposal]
}

object CompletionConfiguration {
  val defaultDelimiters: Set[Char] = ":(){}.,+-*/[];".toSet
}

class CompletionConfiguration {

  private val providers: mutable.Map[DslElement, CompletionProvider] = mutable.Map()
  private var filter: Option[CompletionFilter]                       = None
  private var delimiters: Set[Char]                                  = CompletionConfiguration.defaultDelimiters
  private var computeFilter: Option[CompletionComputeFilter]         = None

  def setProvider(dslElement: DslElement, p: CompletionProvider): Unit = {
    providers(dslElement) = p
  }

  def getProvider(dslElement: DslElement): Option[CompletionProvider] = providers.get(dslElement)

  def setFilter(f: CompletionFilter): Unit = {
    filter = Some(f)
  }

  def getFilter: Option[CompletionFilter] = filter

  def setDelimiters(delimiters: Set[Char]): Unit = {
    this.delimiters = delimiters
  }

  def getDelimiters: Set[Char] = delimiters

  def setComputeFilter(filter: CompletionComputeFilter): Unit = {
    this.computeFilter = Some(filter)
  }

  def getComputeFilter: Option[CompletionComputeFilter] =
    this.computeFilter
}

case class PredictionState(private val state: State, private val result: Result) {
  def isAxiom: Boolean =
    state.production.element match {
      case Some(element) => element match {
          case DslAxiom(_) => true
          case _           => false
        }
      case None          => false
    }

  val subIndex: Option[Int] =
    if (state.production.symbols(state.dot).isToken)
      None
    else
      Some(state.production.symbols.take(state.dot + 1).count(_.isRule) - 1)

  val leftSubIndex: Option[Int] =
    if (state.dot == 0)
      None
    else {
      val rulesLeftOfDot = state.production.symbols.take(state.dot).filter(_.isRule)
      if (rulesLeftOfDot.isEmpty) None else Some(rulesLeftOfDot.length - 1)
    }

  def element: Option[DslElement] = state.production.getElement

  private def toIndex(subIndex: Int): Int =
    state.production.symbols.zipWithIndex.filter(_._1.isRule).drop(subIndex).head._2

  def elementsAt(subIndex: Int, recurse: Dsl.Syntax[_] => Boolean = _ => false): Seq[DslElement] =
    elementsAt(state, toIndex(subIndex), recurse)

  private def elementsAt(
    state: State,
    index: Int,
    recurse: Dsl.Syntax[_] => Boolean
  ): Seq[DslElement] =
    if (index < state.dot) {
      elementsAt(result.backPtrsOf(state), index, state.dot, recurse)
    } else
      Seq.empty

  private def elementsAt(
    backPtrs: Seq[BackPtr],
    index: Int,
    dot: Int,
    recurse: Dsl.Syntax[_] => Boolean
  ): Seq[DslElement] =
    if (backPtrs.nonEmpty) {
      if (index + 1 == dot) {
        backPtrs flatMap { bp =>
          bp.causal match {
            case _: TerminalItem                        => Seq.empty
            case causal @ State(production, _, _, _, _) => production.element match {
                case Some(value) =>
                  value match {
                    case syntax: DslSyntax[_] =>
                      if (recurse(syntax.syntax)) elementsAt(causal, 0, recurse) else Seq(value)
                    case _: DslBody           =>
                      elementsAt(causal, 0, recurse)
                    case _                    => Seq(value)
                  }
                case None        => elementsAt(causal, 0, recurse)
              }
          }
        }
      } else {
        backPtrs flatMap {
          bp => elementsAt(result.backPtrsOf(bp.predecessor), index, bp.predecessor.dot, recurse)
        }
      }
    } else Seq.empty

  def textsAt(subIndex: Int): Seq[String] = textsAt(state, toIndex(subIndex))

  private def textsAt(state: State, index: Int): Seq[String] =
    if (index < state.dot) {
      textsAt(result.backPtrsOf(state), index, state.dot)
    } else
      Seq.empty

  private def textsAt(backPtrs: Seq[BackPtr], index: Int, dot: Int): Seq[String] =
    if (backPtrs.nonEmpty) {
      if (index + 1 == dot) {
        backPtrs flatMap { bp =>
          bp.causal match {
            case terminal: TerminalItem => Seq(terminal.token.text)
            case causal: State          =>
              Seq(Seq.range(causal.begin, causal.end).flatMap(i =>
                result.chartAt(i).token.map(_.text)
              ).mkString(" "))
          }
        }
      } else {
        backPtrs flatMap {
          bp => textsAt(result.backPtrsOf(bp.predecessor), index, bp.predecessor.dot)
        }
      }
    } else Seq.empty
}

class CompletionProcessor(
  val result: Result,
  val text: String,
  val navigatorFactory: Result => Navigator,
  val config: Option[CompletionConfiguration] = None
) {

  def computeCompletionProposal(offset: Int): Seq[CompletionProposal] = {

    val delimiters =
      config.map(_.getDelimiters).getOrElse(CompletionConfiguration.defaultDelimiters)

    val previousChar =
      if (offset >= 1 && offset <= text.length)
        Some(text.charAt(offset - 1))
      else
        None

    val afterDelimiter = previousChar.exists(delimiters.contains)

    def computeProposalFor(production: Bnf.Production): Boolean = {
      val continueVisit = for {
        element       <- production.element
        c             <- config
        computeFilter <- c.getComputeFilter
      } yield computeFilter.continueVisit(element)
      continueVisit.getOrElse(true)
    }

    def beginCompute(predictionState: PredictionState): Boolean = {
      config.flatMap(_.getComputeFilter).forall(_.beginVisit(predictionState))
    }

    def endCompute(candidates: Seq[CompletionProposal]): Seq[CompletionProposal] = {
      config.flatMap(_.getComputeFilter).map(_.endVisit(candidates)).getOrElse(candidates)
    }

    def findTokenTextForProduction(production: Bnf.Production, dot: Int): CompletionProposal = {
      val text = production.symbols
        .drop(dot)
        .takeWhile(_.isToken)
        .map(_.asInstanceOf[Token])
        .map(_.defaultValue)
        .filterNot(_.isEmpty)
        .mkString(" ")
      CompletionProposal(
        production.getElement,
        text
      )
    }

    def computeAllProposals(
      production: Bnf.Production,
      dot: Int,
      visited: Set[Bnf.NonTerminal],
      stack: Seq[Bnf.NonTerminal],
      from: Int,
      feature: Feature,
      tree: GenericTree,
      offset: Int,
      node: Option[GenericNode]
    ): Seq[CompletionProposal] = {
      if (dot < production.length) {
        production.symbols(dot) match {
          case _: Token       => Seq(findTokenTextForProduction(production, dot))
          case _: Bnf.Axiom   => Seq.empty // not possible
          case rule: Bnf.Rule =>
            if (!visited.contains(rule)) {
              val newVisited = visited + rule
              rule.productions.flatMap { p =>
                val newFeature = feature.merge(from, p.feature)
                if (newFeature != Constraints.Incompatible) {
                  val continueVisit = computeProposalFor(p)
                  if (continueVisit) {
                    val provided =
                      (for {
                        element  <- p.element
                        c        <- config
                        provider <- c.getProvider(element)
                      } yield provider.getProposals(Some(element), tree, offset, node))

                    provided.getOrElse(computeAllProposals(
                      p,
                      0,
                      newVisited,
                      stack :+ rule,
                      from,
                      newFeature,
                      tree,
                      offset,
                      node
                    ))
                  } else Seq.empty
                } else Seq.empty
              }
            } else Seq.empty
        }
      } else Seq.empty
    }

    def isAxiom(production: Production): Boolean =
      production.element match {
        case Some(element) => element match {
            case DslAxiom(_) => true
            case _           => false
          }
        case None          => false
      }

    def isPredictionState(s: State): Boolean = {
      (s.dot == 0 && isAxiom(s.production)) || s.dot > 0
    }

    val navigator = navigatorFactory(result)
    navigator.toIterator
      .toSeq
      .foldLeft(Seq.empty[CompletionProposal]) { case (acc, tree) =>
        var node: Option[GenericNode]          = None
        var defaultReplace: Option[(Int, Int)] = None
        val treeProposals                      = result.chartAndPrefixAtOffset(offset, afterDelimiter)
          .map({ case (chart, prefix) =>
            defaultReplace = prefix.map(p => (offset - p.length, p.length))
            node = tree.root.findNodeAtIndex(chart.index)
            chart.notCompletedStates
              .filterNot(_.kind(result) == StateKind.ErrorRecovery)
              .filter(isPredictionState)
              .flatMap { s =>
                if (beginCompute(PredictionState(s, result))) {
                  val candidates = computeAllProposals(
                    s.production,
                    s.dot,
                    Set.empty,
                    Seq.empty,
                    s.dot,
                    s.feature,
                    tree,
                    offset,
                    node
                  )
                  endCompute(candidates)
                } else {
                  Seq.empty
                }
              }
          })
          .getOrElse(Seq.empty)
        acc ++ config
          .flatMap(c => c.getFilter)
          .map(f => f.filterProposals(tree, offset, node, treeProposals))
          .getOrElse(treeProposals)
          .map(proposal => proposal.copy(replace = proposal.replace.orElse(defaultReplace)))
      }
      .distinct
  }
}
