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

import diesel.Bnf.Constraints
import diesel.Lexer.Eos

case class Earley(bnf: Bnf, dynamicLexer: Boolean = false) {

  private def lexer = bnf.lexer

  def parse(input: Lexer.Input, axiom: Bnf.Axiom): Result = {
    buildCharts(input, axiom)
  }

  private def scan(input: Lexer.Input, context: Result): Lexer.Token = {
    scan(input, Seq(), context)
  }

  private def scan(input: Lexer.Input, tokens: Seq[Lexer.TokenId], context: Result): Lexer.Token = {
    var token = if (tokens.isEmpty) lexer.next(input) else lexer.next(input, tokens)
    if (token.id == Lexer.Error) {
      var errorToken = Lexer.Token(token.offset, "", Lexer.Error)
      do {
        if (errorToken.offset + errorToken.length == token.offset) {
          errorToken = Lexer.Token(errorToken.offset, errorToken.text ++ token.text, Lexer.Error)
        } else {
          context.addLexicalError(errorToken)
          errorToken = token
        }
        token = if (tokens.isEmpty) lexer.next(input) else lexer.next(input, tokens)
      } while (token.id == Lexer.Error)
      context.addLexicalError(token = errorToken)
    }
    token
  }

  private def skip(input: Lexer.Input): Lexer.Token = {
    lexer.skip(input)
  }

  private def dynamicScan(input: Lexer.Input, context: Result): Lexer.Token = {
    skip(input)
    val scanQueue    = closure(context)
    val tokens       = scanQueue.map(_.nextSymbol.asInstanceOf[Bnf.Token].tokenId)
    val lexicalValue = scan(input, tokens, context)
    scanQueue.foreach(s => context.processingQueue.enqueue(s))
    lexicalValue
  }

  private def buildCharts(input: Lexer.Input, axiom: Bnf.Axiom): Result = {
    val context      = new Result(bnf, axiom)
    var index        = 0
    var chart        = context.beginChart(index)
    var lexicalValue = if (dynamicLexer) dynamicScan(input, context) else scan(input, context)
    var length       = if (lexicalValue.id == Lexer.Eos) 0 else 1
    while (index <= length) {
      var scanned = false
      context.resetNullable()
      chart.setToken(lexicalValue)
      while (context.processingQueue.nonEmpty) {
        val state: State = context.processingQueue.dequeue()
        if (state.isCompleted)
          completer(state, context)
        else {
          val next: Bnf.Symbol = state.nextSymbol
          next match {
            case token: Bnf.Token =>
              if (scanner(state, token, lexicalValue, context))
                scanned = true

            case rule: Bnf.Rule =>
              predictor(state, rule, context)

            case _ => ()
          }
        }
      }
      context.endChart()
      if (!scanned && !succeed(length, lexicalValue, context)) {
        errorRecovery(index, lexicalValue, length, backtracking = false, context)
        if (lexicalValue.id == Lexer.Eos && !context.success(length)) {
          val backtrack = chart.getStates.minBy(_.begin).begin
          for (i <- backtrack until index + 1) {
            errorRecovery(i, context.chartAt(i).token.get, length, backtracking = true, context)
          }
          if (!context.success(length)) {
            throw new RuntimeException("internal error, unable to recover from errors")
          }
        }
      }

      index += 1
      if (!succeed(length, lexicalValue, context)) {
        chart = context.beginChart(index)
        lexicalValue = if (dynamicLexer) dynamicScan(input, context) else scan(input, context)
        if (lexicalValue.id != Lexer.Eos)
          length += 1
      }
    }
    context
  }

  private def closure(context: Result): Seq[State] = {
    context.resetNullable()
    var scanQueue: Seq[State] = Seq()
    while (context.processingQueue.nonEmpty) {
      val state: State = context.processingQueue.dequeue()
      if (state.isCompleted)
        completer(state, context)
      else {
        val next: Bnf.Symbol = state.nextSymbol
        next match {
          case _: Bnf.Token =>
            scanQueue = scanQueue ++ Seq(state)

          case rule: Bnf.Rule =>
            predictor(state, rule, context)

          case _ => ()
        }
      }
    }
    scanQueue
  }

  private def succeed(index: Int, lexicalValue: Lexer.Token, context: Result): Boolean = {
    if (lexicalValue.id == Lexer.Eos) context.success(index) else false
  }

  private def errorRecovery(
    index: Int,
    lexicalValue: Lexer.Token,
    length: Int,
    backtracking: Boolean,
    context: Result
  ): Unit = {
    context.beginErrorRecovery(index, context)
    if (context.processingQueue.isEmpty)
      throw new RuntimeException(
        "internal error, processing queue is empty while recovering from errors"
      )
    context.resetNullable()
    while (context.processingQueue.nonEmpty) {
      val state: State = context.processingQueue.dequeue()
      if (state.isCompleted) {
        if ((lexicalValue.id != Eos) && (state == context.successState(length - 1))) {
          // Insertion error hypothesis : ignore all the tokens at the end of right text
          context.addState(
            State(state.production, state.begin, length, state.dot),
            StateKind.ErrorRecovery,
            Some(BackPtr(state, InsertedTokenValue(index, lexicalValue, None)))
          )
        }
        completer(state, context, errorRecovery = true)
      } else {
        val next: Bnf.Symbol = state.nextSymbol
        next match {
          case token: Bnf.Token =>
            if (!scanner(state, token, lexicalValue, context)) {
              if (lexicalValue.id != Lexer.Eos) {
                context.addState(
                  State(state.production, state.begin, state.end + 1, state.dot),
                  StateKind.ErrorRecovery,
                  Some(BackPtr(state, InsertedTokenValue(index, lexicalValue, None)))
                )
                context.addState(
                  State(state.production, state.begin, state.end + 1, state.dot + 1),
                  StateKind.ErrorRecovery,
                  Some(BackPtr(
                    state,
                    MutationTokenValue(
                      index,
                      Lexer.Token(lexicalValue.offset, token.defaultValue, token.tokenId),
                      lexicalValue,
                      token.style
                    )
                  ))
                )
              }
              context.addState(
                State(state.production, state.begin, state.end, state.dot + 1),
                StateKind.ErrorRecovery,
                Some(BackPtr(
                  state,
                  DeletedTokenValue(
                    index,
                    Lexer.Token(lexicalValue.offset, token.defaultValue, token.tokenId),
                    token.style
                  )
                ))
              )
            }

          case rule: Bnf.Rule =>
            predictor(state, rule, context, errorRecovery = true, backtracking)

          case _ => ()
        }
      }
    }
    context.endErrorRecovery(context)
  }

  private def scanner(
    state: State,
    token: Bnf.Token,
    tokenValue: Lexer.Token,
    context: Result
  ): Boolean = {
    if (lexer.accept(token, tokenValue)) {
      context.addState(
        State(state.production, state.begin, state.end + 1, state.dot + 1),
        StateKind.Kernel,
        Some(BackPtr(state, TokenValue(state.dot, tokenValue, token.style)))
      )
      true
    } else
      false
  }

  private def predictor(
    state: State,
    rule: Bnf.Rule,
    context: Result,
    errorRecovery: Boolean = false,
    backtracking: Boolean = false
  ): Unit = {
    if (backtracking && state.kind(context) == StateKind.ErrorRecovery) {
      context.addState(
        State(rule.productions.minBy(_.length), state.end, state.end, 0),
        StateKind.ErrorRecovery,
        None
      )
    } else {
      rule.productions.foreach(production =>
        context.addState(
          State(production, state.end, state.end, 0),
          if (errorRecovery) {
            StateKind.ErrorRecovery
          } else {
            StateKind.next(state.kind(context))
          },
          None
        )
      )
    }
  }

  private def completer(state: State, context: Result, errorRecovery: Boolean = false): Unit = {
    if (bnf.emptyRules.contains(state.rule) && (state.begin == state.end)) {
      context.addToNullable(state)
    }
    val candidates = context.chartAt(state.begin).activeRules(state.rule)
    candidates.foreach(candidate => {
      val feature = candidate.feature.merge(candidate.dot, state.feature)
      if (
        feature != Constraints.Incompatible || (errorRecovery && state.syntacticErrors( // apply only when no syntactic errors
          context
        ) == 0)
      ) {
        context.addState(
          State(
            candidate.production,
            candidate.begin,
            state.end,
            candidate.dot + 1,
            if (candidate.feature.canPropagate) feature else candidate.feature
          ),
          if (errorRecovery) {
            if (feature == Constraints.Incompatible) StateKind.Incompatible
            else StateKind.ErrorRecovery
          } else {
            StateKind.next(state.kind(context))
          },
          Some(BackPtr(candidate, state))
        )
      }
    })
  }
}
