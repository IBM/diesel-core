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
