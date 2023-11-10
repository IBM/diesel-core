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
import diesel.Errors.{InsertedToken, MissingToken, TokenMutation, UnknownToken}
import diesel.Lexer.{Eos, Token}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

private[diesel] trait Item {

  def syntacticErrors(ctx: Result): Int
}

private[diesel] trait TerminalItem extends Item {

  def begin: Int

  def end: Int = begin + 1

  def token: Token

  def tokenId: Lexer.TokenId = token.id

  def style: Option[Style]

  def isErrorRecovery: Boolean = false

  override def syntacticErrors(ctx: Result): Int = if (isErrorRecovery) 1 else 0

  def reportErrors(): Seq[Marker]
}

private[diesel] case class TokenValue(
  index: Int,
  override val token: Token,
  override val style: Option[Style]
) extends TerminalItem {

  override def begin: Int = index

  override def reportErrors(): Seq[Marker] = Seq()
}

private[diesel] case class InsertedTokenValue(
  index: Int,
  override val token: Token,
  override val style: Option[Style]
) extends TerminalItem {

  override def begin: Int = index

  override def isErrorRecovery: Boolean = true

  override def reportErrors(): Seq[Marker] = Seq(InsertedToken(token.offset, token))
}

private[diesel] case class DeletedTokenValue(
  index: Int,
  override val token: Token,
  override val style: Option[Style]
) extends TerminalItem {

  override def begin: Int = index

  override def end: Int = index

  override def isErrorRecovery: Boolean = true

  override def reportErrors(): Seq[Marker] = Seq(MissingToken(token.offset, token))
}

private[diesel] case class MutationTokenValue(
  index: Int,
  override val token: Token,
  actualToken: Token,
  override val style: Option[Style]
) extends TerminalItem {

  override def begin: Int = index

  override def isErrorRecovery: Boolean = true

  override def reportErrors(): Seq[Marker] = Seq(TokenMutation(token.offset, actualToken, token))
}

private[diesel] object State {
  def apply(production: Bnf.Production, begin: Int, end: Int, dot: Int): State =
    State(production, begin, end, dot, production.feature)
}

private[diesel] case class State(
  production: Bnf.Production,
  begin: Int,
  end: Int,
  dot: Int,
  feature: Feature
) extends Item {

  val rule: Bnf.NonTerminal = production.rule.get

  val isCompleted: Boolean = dot == production.length

  lazy val nextSymbol: Bnf.Symbol = production.symbols(dot)

  override def toString: String = {
    toString(Map.empty)
  }

  override def syntacticErrors(ctx: Result): Int =
    ctx.contextOf(this).map(_.syntacticErrors).getOrElse(0)

  private[diesel] def kind(ctx: Result): StateKind.Value =
    ctx.contextOf(this).map(_.kind).getOrElse(StateKind.Kernel)

  private[diesel] def toString(states: Map[State, StateContext]): String = {
    val builder = new mutable.StringBuilder()
    builder.append(rule.name).append(" -> ")
    production.symbols.zipWithIndex.foreach { case (symbol: Bnf.Symbol, index: Int) =>
      if (index == dot)
        builder.append(". ")
      builder.append(symbol.name).append(" ")
    }
    if (production.symbols.length == dot)
      builder.append(". ")
    builder.append("[").append(begin).append(", ").append(end).append("] ").append(feature)
    //    states.get(this).foreach(ctx => {
    //        builder.append("{ ")
    //
    //        builder.append("}")
    //      }
    //    )
    builder.toString
  }

}

private[diesel] class Chart(
  val index: Int,
  var token: Option[Lexer.Token] = None,
  private val states: ArrayBuffer[State] = ArrayBuffer()
) {
  def setToken(token: Lexer.Token): Unit = {
    this.token = Some(token)
  }

  private val _activeStates: ArrayBuffer[State]       = ArrayBuffer()
  private val _notCompletedStates: ArrayBuffer[State] = ArrayBuffer()

  def `+=`(state: State): Chart = {
    states += state
    if (!state.isCompleted && state.nextSymbol.isRule) {
      _activeStates += state
    }
    if (!state.isCompleted) {
      _notCompletedStates += state
    }
    this
  }

  def size: Int = states.size

  def toQueue(processingQueue: mutable.Queue[State]): Unit = {
    states.foreach(state => processingQueue.enqueue(state))
  }

  def activeStates(predicate: State => Boolean): Seq[State] = {
    _activeStates.filter(predicate).toSeq
  }

  def notCompletedStates: Seq[State] = {
    _notCompletedStates.toSeq
//    states.filter(s => !s.isCompleted).toSeq
  }

  private[diesel] def offset: Int = token.map(_.offset).getOrElse(-1)

  private[diesel] def endsAtOffset(offset: Int): Boolean = {
    token
      .map(token =>
        token.offset + token.text.length == offset
      )
      .getOrElse(false)
  }

  private[diesel] def isAtOffset(offset: Int): Boolean = {
    token
      .map(token =>
        token.offset <= offset && (offset <= token.offset + token.text.length || token.id == Eos)
      )
      .getOrElse(false)
  }

  private[diesel] def isAtOffsetExclusive(offset: Int): Boolean = {
    token
      .map(token =>
        token.offset <= offset && (offset < token.offset + token.text.length)
      )
      .getOrElse(false)
  }

  private[diesel] def isAfterOffset(offset: Int): Boolean = {
    token
      .map(offset <= _.offset)
      .getOrElse(false)
  }

  override def toString: String = {
    val builder = new mutable.StringBuilder()

    builder.append(token.map(_.text).getOrElse(""))
    builder.append(": ").append(states.size).append(" states\n")
    states.foreach(builder.append(_).append("\n"))
    builder.append("\n")

    builder.toString
  }
}

private[diesel] case class BackPtr(predecessor: State, causal: Item) {
  def syntacticErrors(ctx: Result): Int = {
    predecessor.syntacticErrors(ctx) + causal.syntacticErrors(ctx)
  }
}

private[diesel] object StateKind extends Enumeration {
  val Kernel, Processed, ErrorRecovery = Value

  def next(value: Value): Value =
    if (value.id < ErrorRecovery.id) values.iteratorFrom(value).next() else value
}

private[diesel] class StateContext(
  val id: Int,
  var kind: StateKind.Value,
  var syntacticErrors: Int,
  val backPtrs: ArrayBuffer[BackPtr] = ArrayBuffer()
) {

  def mergeBackPtr(kind: StateKind.Value, backPtr: BackPtr, ctx: Result): StateContext = {
    if (this.kind != kind) {
      if (this.kind.id > kind.id) {
        this.kind = kind;
      }
    }
    val syntacticErrors = backPtr.syntacticErrors(ctx)
    if (backPtrs.isEmpty) {
      this.syntacticErrors = syntacticErrors
      backPtrs += backPtr
    } else if (syntacticErrors < this.syntacticErrors) {
      this.syntacticErrors = syntacticErrors
      backPtrs.clear()
      backPtrs += backPtr
    } else if (syntacticErrors == this.syntacticErrors && syntacticErrors == 0) {
      if (!backPtrs.contains(backPtr)) // TODO: use better implementation O(n)
        backPtrs += backPtr
    }
    this
  }
}

class Result(val axiom: Bnf.Axiom) {

  private val states: mutable.Map[State, StateContext] = mutable.Map()
  private val charts: ArrayBuffer[Chart]               = ArrayBuffer()
  private val markers: ArrayBuffer[Marker]             = ArrayBuffer()
  private val errorTokens: ArrayBuffer[Token]          = ArrayBuffer()

  val axiomState: State = State(axiom.production, 0, 0, 0)

  private def successState(length: Int): State = State(axiom.production, 0, length, 1)

  private var currentChart: Option[Chart]                   = None
  private[diesel] val processingQueue: mutable.Queue[State] = mutable.Queue()

  pushChart += axiomState

  private[diesel] def contextOf(state: State): Option[StateContext] = states.get(state)

  private[diesel] def addLexicalError(token: Token): Unit = {
    markers += UnknownToken(token.offset, token)
    errorTokens += token
  }

  private[diesel] def reportErrors(): Seq[Marker] = markers.toSeq

  private[diesel] def beginChart(index: Int): Chart = {
    val chart = if (index == charts.size) pushChart else chartAt(index)
    chart.toQueue(processingQueue)
    currentChart = Some(chart)
    chart
  }

  private def getTokenPrefix(offset: Int, token: Token): String = {
    val diff = token.offset + token.length - offset
    token.text.dropRight(diff)
  }

  private[diesel] def chartAndPrefixAtOffset(
    offset: Int,
    afterDelimiter: Boolean
  ): Option[(Chart, Option[String])] = {

    def tokenEndsAt(offset: Int) = {
      t: Token => t.offset + t.length == offset
    }

    def useNextChart = charts
      .find(chart => chart.isAfterOffset(offset))
      .map(chart => (chart, None))

    if (afterDelimiter) {
      // we are just after the delimiter, 2 cases :
      // 1. we are at the end of a token : no lookback needed
      // 2. we are in a token : look back to the token start
      charts
        .find(chart => chart.isAtOffset(offset))
        .flatMap(chart => chart.token.map(t => (chart, t))) match {
        case Some((chart, token)) =>
          if (offset == token.offset + token.length) {
            useNextChart
          } else {
            val text   = token.text
            val prefix = text.dropRight(text.length - offset)
            Some((chart, Some(prefix)))
          }
        case None                 =>
          useNextChart
      }

    } else {

      val c      = charts.find { chart =>
        chart.isAtOffset(offset)
      }.orElse(charts.find { chart =>
        chart.isAfterOffset(offset)
      })
      val prefix = errorTokens
        .find(tokenEndsAt(offset))
        .orElse(c.flatMap(_.token))
        .map(getTokenPrefix(offset, _))
        .filter(_.nonEmpty)
      //      .map(_.text)

      c.map((_, prefix))
    }
  }

  private[diesel] def chartAt(index: Int) = {
    while (index >= charts.size)
      pushChart
    charts(index)
  }

  private[diesel] def tokenAt(index: Int): Option[Lexer.Token] = {
    chartAt(index).token
  }

  private def pushChart: Chart = {
    val res = new Chart(charts.length)
    charts += res
    res
  }

  private[diesel] def addState(
    state: State,
    kind: StateKind.Value,
    backPtr: Option[BackPtr]
  ): Unit = {
    val ctx = states.getOrElseUpdate(
      state, {
        val res = new StateContext(states.size, kind, if (state.dot == 0) 0 else Int.MaxValue)
        states.put(state, res)
        enqueue(state)
        res
      }
    )
    backPtr.foreach(ptr => ctx.mergeBackPtr(kind, ptr, this))
  }

  private def enqueue(state: State): Unit = {
    val chart = chartAt(state.end)
    chart += state
    currentChart
      .filter(_ == chart)
      .foreach(_ => processingQueue.enqueue(state))
  }

  private[diesel] def beginErrorRecovery(): Unit = {
    currentChart.foreach(chart => chart.toQueue(processingQueue))
  }

  private[diesel] def endErrorRecovery(): Unit = {}

  private[diesel] def endChart(): Unit = {
    currentChart = None
  }

  private def success(length: Int): Boolean = states.get(successState(length)).isDefined

  def success: Boolean = success(length - 1)

  def successState: State = successState(length - 1)

  def length: Int = charts.size

  private[diesel] def hasNext: Boolean = chartAt(length - 1).size > 0 || !success

  override def toString: String = {
    val builder = new mutable.StringBuilder()
    charts.foreach(builder.append(_))
    builder.toString
  }
}
