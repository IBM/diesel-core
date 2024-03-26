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

import diesel.Bnf.{DslElement, Token}

import scala.collection.mutable

case class CompletionProposal(
  element: Option[DslElement],
  text: String,
  replace: Option[(Int, Int)] = None,
  userData: Option[Any] = None,
  documentation: Option[String] = None,
  predictorPaths: Seq[Seq[DslElement]] = Seq.empty
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

object CompletionConfiguration {
  val defaultDelimiters: Set[Char] = ":(){}.,+-*/[];".toSet

  trait CompletionProposalFactory {
    def createProposal(result: Result, state: State, proto: CompletionProposal): CompletionProposal
  }
}

class CompletionConfiguration {

  private val providers: mutable.Map[DslElement, CompletionProvider] = mutable.Map()
  private var filter: Option[CompletionFilter]                       = None
  private var delimiters: Set[Char]                                  = CompletionConfiguration.defaultDelimiters
  private var includePaths: Boolean                                  = false

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

  def setIncludePaths(include: Boolean): Unit = {
    this.includePaths = include
  }

  def isIncludePaths: Boolean = this.includePaths
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

    val c              =
      if (offset >= 1 && offset <= text.length)
        Some(text.charAt(offset - 1))
      else
        None
    val afterDelimiter = c.exists(delimiters.contains)

    def hasProvider(state: State) =
      state.production.getElement
        .filter(_ => state.dot == 0)
        .flatMap { elem => config.flatMap(_.getProvider(elem)) }.isDefined

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
      tree: GenericTree,
      offset: Int,
      node: Option[GenericNode]
    ): Seq[CompletionProposal] = {
      if (dot < production.length) {
        production.symbols(dot) match {
          case _: Token       =>
            Seq(findTokenTextForProduction(production, dot))
          case _: Bnf.Axiom   => Seq.empty // not possible
          case rule: Bnf.Rule =>
            if (!visited.contains(rule)) {
              rule.productions.flatMap { p =>
                val provided =
                  (for {
                    element  <- p.element
                    c        <- config
                    provider <- c.getProvider(element)
                  } yield provider.getProposals(Some(element), tree, offset, node))

                provided.getOrElse(computeAllProposals(
                  p,
                  0,
                  visited + rule,
                  stack :+ rule,
                  tree,
                  offset,
                  node
                )).concat(if (result.bnf.emptyRules.contains(rule))
                  computeAllProposals(
                    production,
                    dot + 1,
                    visited,
                    stack,
                    tree,
                    offset,
                    node
                  )
                else Seq.empty)
              }
            } else Seq.empty
        }
      } else
        Seq.empty
    }

    def isPredictionState(s: State): Boolean = {
      s.kind(result) == StateKind.Kernel
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
                computeAllProposals(s.production, s.dot, Set.empty, Seq.empty, tree, offset, node)
              }
          // state prediction root : axiom or state with a token on left of dot
          // rule after dot, predict all for that rule filter while predicting
          // .filter(s => s.nextSymbol.isToken || hasProvider(s))
//              .flatMap(state => {
//                val defaultProvider: CompletionProvider = (
//                  element: Option[DslElement],
//                  tree: GenericTree,
//                  offset: Int,
//                  node: Option[GenericNode]
//                ) => {
//                  val token = findTokenTextAfterDot(state)
//                  token
////                    .filter(text => prefix.forall(text.startsWith))
//                    .map { text =>
//                      CompletionProposal(
//                        element,
//                        text,
//                        prefix.map(p => (offset - p.length, p.length)),
//                        predictorPaths =
//                          if (config.exists(_.isIncludePaths)) {
//                            result.getPredictorPaths(state).map(_.flatMap(_.production.getElement))
//                          } else {
//                            Seq.empty
//                          }
//                      )
//                    }
//                    .toSeq
//                }
//                val element: Option[DslElement]         = state.production.getElement
//                element
//                  .filter(_ => state.dot == 0)
//                  .flatMap { elem => config.flatMap(_.getProvider(elem)) }
//                  .getOrElse(defaultProvider)
//                  .getProposals(element, tree, offset, node)
//              })
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

  private def findTokenTextAfterDot(state: State): Option[String] = {
    Some(state.production.symbols
      .drop(state.dot)
      .takeWhile(_.isToken)
      .map(_.asInstanceOf[Token])
      .map(_.defaultValue)
      .filterNot(_.isEmpty)
      .mkString(" "))
      .filterNot(_.isEmpty)
  }
}
