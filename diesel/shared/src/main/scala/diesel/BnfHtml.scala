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

import diesel.Bnf.{Axiom, Rule}

object BnfHtml {

  def dump(bnf: Bnf, path: String): Unit = {
    Platform.consoleWithFile(path) {
      dump(bnf)
    }
  }

  def dump(bnf: Bnf): Unit = {
    println("<html><body>")
    println("<title>BNF</title>")
    println("<h2>Axioms</h2>")
    println("<table>")
    bnf.axioms.sortBy(_.name).foreach(dumpAxiom)
    println("</table>")
    println("<h2>Rules</h2>")
    println("<table>")
    bnf.rules.filter(_.isRule).sortBy(_.name).map(_.asInstanceOf[Rule]).foreach(dumpRule)
    println("</table>")
    println("</body></html>")
  }

  private def dumpAxiom(a: Axiom): Unit = {
    println(s"<tr><td>${toAnchor(a.name)}</td><td></tr>")
    a.production.symbols.foreach { s =>
      println(s"<tr><td/><td>${toLink(s.name)}</td></tr>")
    }
    println()
  }

  private def dumpRule(r: Rule): Unit = {
    println(s"<tr><td>${toAnchor(r.name)}</td><td></tr>")
    r.productions.foreach { p =>
      println(s"<tr><td/><td> | ${p.symbols.map(dumpSymbol).mkString(" ")}</td></tr>")
    }
    println()
  }

  private def dumpSymbol(s: Bnf.Symbol): String = s match {
    case Bnf.Token(name, _, _) => s"""<tt>"$name"</tt>"""
    case r: Bnf.Rule           => toLink(r.name)
    case _                     => ""
  }

  private def toAnchor(name: String): String =
    s"""<a name="$name"><b>$name</b></a>"""

  private def toLink(name: String): String =
    s"""<a href="#$name">$name</a>"""

}
