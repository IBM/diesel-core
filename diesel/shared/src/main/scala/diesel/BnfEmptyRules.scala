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

import scala.annotation.tailrec

private[diesel] case class BnfEmptyRules(bnf: Bnf) {

  private def isEmpty(production: Bnf.Production, emptyRules: Set[Bnf.NonTerminal]): Boolean =
    if (production.length == 0)
      true
    else {
      production.symbols.forall(s =>
        !s.isToken && (s.isRule && emptyRules.contains(s.asInstanceOf[Bnf.NonTerminal]))
      )
    }

  @tailrec
  private def computeEmptyRules(
    rule: Bnf.NonTerminal,
    emptyRules: Set[Bnf.NonTerminal]
  ): Set[Bnf.NonTerminal] =
    if (emptyRules.contains(rule))
      emptyRules
    else {
      rule match {
        case Bnf.Rule(_, productions) =>
          if (productions.exists(isEmpty(_, emptyRules)))
            emptyRules ++ Set(rule)
          else
            emptyRules

        case Bnf.Axiom(rule) => computeEmptyRules(rule, emptyRules)
      }
    }

  val emptyRules: Set[Bnf.NonTerminal] = {
    var emptyRules: Set[Bnf.NonTerminal] = Set()
    var cont                             = false
    do {
      cont = false
      var i = bnf.rules
      while (i.nonEmpty) {
        val newEmptyRules = computeEmptyRules(i.head, emptyRules)
        if (newEmptyRules.size > emptyRules.size) {
          cont = true
          emptyRules = newEmptyRules
        }
        i = i.tail
      }
    } while (cont)
    emptyRules
  }
}
