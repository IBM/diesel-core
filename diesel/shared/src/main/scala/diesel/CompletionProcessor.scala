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

object CompletionConfiguration {
  val defaultDelimiters: Set[Char] = ":(){}.,+-*/[];".toSet
}

class CompletionConfiguration {

  private val providers: mutable.Map[DslElement, CompletionProvider] = mutable.Map()
  private var filter: Option[CompletionFilter]                       = None
  private var delimiters: Set[Char]                                  = CompletionConfiguration.defaultDelimiters

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
    val afterDelimiter = c.exists(delimiters.contains(_))

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
              .flatMap(state => {
                val defaultProvider: CompletionProvider = (
                  element: Option[DslElement],
                  tree: GenericTree,
                  offset: Int,
                  node: Option[GenericNode]
                ) => {
                  val token = findTokenTextAfterDot(state)
                  token
//                    .filter(text => prefix.forall(text.startsWith))
                    .map(text =>
                      CompletionProposal(
                        element,
                        text,
                        prefix.map(p => (offset - p.length, p.length))
                      )
                    )
                    .toSeq
                }
                val element: Option[DslElement]         = state.production.getElement
                element
                  .filter(_ => state.dot == 0)
                  .flatMap { elem => config.flatMap(_.getProvider(elem)) }
                  .getOrElse(defaultProvider)
                  .getProposals(element, tree, offset, node)
              })
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
