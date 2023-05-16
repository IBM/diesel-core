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
import diesel.CompletionProcessor.findPrefix

import scala.collection.mutable

case class CompletionProposal(
  element: Option[DslElement],
  text: String,
  replace: Option[(Int, Int)] = None,
  userData: Option[Any] = None
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

trait CompletionLookback {
  def isCompletionDelimiter(c: Char): Boolean
}

object DefaultCompletionLookback extends CompletionLookback {
  override def isCompletionDelimiter(c: Char): Boolean = c.isWhitespace
}

case class SimpleCompletionLookback(chars: String) extends CompletionLookback {
  override def isCompletionDelimiter(c: Char): Boolean =
    DefaultCompletionLookback.isCompletionDelimiter(c) || chars.contains(c)
}

class CompletionConfiguration {

  private val providers: mutable.Map[DslElement, CompletionProvider] = mutable.Map()
  private var filter: Option[CompletionFilter]                       = None
  private var lookback: Option[CompletionLookback]                   = None

  def setProvider(dslElement: DslElement, p: CompletionProvider): Unit = {
    providers(dslElement) = p
  }

  def getProvider(dslElement: DslElement): Option[CompletionProvider] = providers.get(dslElement)

  def setFilter(f: CompletionFilter): Unit = {
    filter = Some(f)
  }

  def getFilter: Option[CompletionFilter] = filter

  def getLookback: Option[CompletionLookback] = lookback

  def setLookback(l: CompletionLookback): Unit = {
    lookback = Some(l)
  }

}

object CompletionProcessor {
  def findPrefix(text: String, offset: Int, lookback: CompletionLookback): String = {
    if (text.isEmpty || offset == 0) {
      ""
    } else {
      val leftOffset = offset - 1
      val c          = text.charAt(leftOffset)
      if (lookback.isCompletionDelimiter(c)) {
        ""
      } else {
        findPrefix(text, leftOffset, lookback) + c
      }
    }
  }

}

class CompletionProcessor(
  val result: Result,
  val text: String,
  val config: Option[CompletionConfiguration] = None,
  val userDataProvider: Option[UserDataProvider] = None
) {

  def computeCompletionProposal(offset: Int): Seq[CompletionProposal] = {

    val lookback = config
      .flatMap(_.getLookback)
      .getOrElse(DefaultCompletionLookback)

    // adjust offset (prefix)
    val prefix = findPrefix(text, offset, lookback)

    val navigator = new Navigator(result, Seq(), userDataProvider)
    navigator.toIterator
      .toSeq
      .foldLeft(Seq.empty[CompletionProposal]) { case (acc, tree) =>
        var node: Option[GenericNode]          = None
        var defaultReplace: Option[(Int, Int)] = None
        val treeProposals                      = result.chartAtOffset(offset - prefix.length)
          .map { chart =>
            defaultReplace =
              if (prefix.isEmpty) {
                None
              } else {
                Some((offset - prefix.length, prefix.length))
              }
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
                        defaultReplace
                      )
                    )
                    .toSeq
                }
                val element: Option[DslElement]         = state.production.element
                element
                  .filter(_ => state.dot == 0)
                  .flatMap { elem => config.flatMap(_.getProvider(elem)) }
                  .getOrElse(defaultProvider)
                  .getProposals(element, tree, offset, node)
              })
          }
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
