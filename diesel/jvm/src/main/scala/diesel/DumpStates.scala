package diesel

import java.io.File
import java.awt.Desktop
import diesel.Lexer.Eos

object DumpStates {

  val dotChar: String        = """<span class="dot">•</span>"""
  val eos: String            = """<span class="eof">ϵ</span>"""
  val caretCollapsed: String = "›"
  val caretExpanded: String  = "⌄"

  def dumpAndOpen(text: String, result: Result): Unit = {
    val f: File = File.createTempFile("diesel-result-", ".html")
    val r       = dumpResult(text, result)
    BnfExplorer.writeToFile(f, r)
    val desktop = Desktop.getDesktop();
    desktop.browse(f.toURI());
  }

  def dumpResult(text: String, result: Result): String = {

    val tokens: Map[Int, Lexer.Token] = result.getCharts
      .flatMap { chart =>
        chart.token.map(t => (chart.index, t))
      }
      .toMap

    s"""
<html>
<head>
<style>
      .dot, .eof, .token, .keyword, .symbol {
        margin-right: 4px;
        padding-left: 2px;
        padding-right: 2px;
        border-radius: 2px;
      }

      .dot {
        background-color: #DDFFDD;
      }

      .eof {
        background-color: #FFDDDD;
      }
      
      .token {
        background-color: #F8F8FF;
      }

      .state-row.collapsed {
        display: none;
      }

      button.expand-collapse {
        width: 16px;
        border: none;
        background: none;
      }
</style>
</head>
<body>
<h2>Input</h2>
<code>
${text}
</code>
<h2>Result</h1>
<table cellspacing="0" cellpadding="4">
    <tbody>
      ${result.getCharts
        .map(dumpChart(tokens, _))
        .mkString("")}
    </tbody>
</table>
</body>
<script>

function expandCollapse(chartIndex) {
  document.querySelectorAll("button.expand-collapse")
    .forEach(btn => {
      const chartId = btn.getAttribute('data-chart-id');
      if (chartId === chartIndex) {
        const expanded = btn.classList.contains("expanded");
        if (expanded) {
          btn.classList.remove("expanded");
        } else {
          btn.classList.add("expanded");
        }
        btn.innerHTML = expanded ? "$caretCollapsed" : "$caretExpanded"; 
      }
    });

  document.querySelectorAll(".state-row")
    .forEach(stateRow => {
      const chartId = stateRow.getAttribute("data-chart-id");
      if (chartId === chartIndex) {
        const collapsed = stateRow.classList.contains("collapsed");
        if (collapsed) {
          stateRow.classList.remove("collapsed");
        } else {
          stateRow.classList.add("collapsed");
        }
      }
    });
}

document.querySelectorAll("button.expand-collapse")
  .forEach(btn => {
    btn.addEventListener('click', () => {
      const chartId = btn.getAttribute('data-chart-id');
      expandCollapse(chartId);
    })
  });

</script>
</html>
"""
  }

  private def wrapToken(s: String): String =
    s"""<span class="token">$s</span>"""

  def dumpChart(tokens: Map[Int, Lexer.Token], chart: Chart): String = {
    val tokensBefore = tokens
      .toSeq
      .filter { case (i, _) => i < chart.index }
      .map(_._2.text)
      .map(wrapToken)

    val tokensAfter = tokens
      .toSeq
      .filter { case (i, _) => i >= chart.index }
      .map { case (_, t) =>
        t.id match {
          case Eos =>
            eos
          case _   =>
            wrapToken(t.text)
        }
      }

    val chartHeader = (
      tokensBefore ++ Seq(dotChar) ++ tokensAfter
    ).mkString("")

    def expandCollapse(index: Int) = s"""
<button class="expand-collapse" data-chart-id="$index">$caretCollapsed</button>
"""

    s"""
<tr class="chart-row" data-chart-index="${chart.index}">
  <td>
    ${expandCollapse(chart.index)}
    #${chart.index}
  </td>
  <td colspan="3">
    $chartHeader
  </td> 
</tr>
${chart.getStates
        .zipWithIndex
        .map { case (s, i) => dumpState(chart.index, i, s) }
        .mkString("\n")}
"""
  }

  def dumpState(chartIndex: Int, stateIndex: Int, state: State): String = {
    val productionStr = state.production.symbols
      .zipWithIndex
      .map { case (symbol, index) =>
        val leadingDot  =
          if (index == state.dot)
            dotChar
          else
            ""
        val trailingDot =
          if (state.production.symbols.size == state.dot)
            dotChar
          else
            ""
        val symbolName  = symbol match {
          case nt: Bnf.NonTerminal   =>
            val s = nt match {
              case Bnf.Axiom(rule)   =>
                BnfHtml.sanitizeName(rule.name)
              case Bnf.Rule(name, _) =>
                BnfHtml.sanitizeName(name)
            }
            s"""<span class="symbol">$s</span>"""
          case Bnf.Token(name, _, _) =>
            wrapToken(name)
        }

        leadingDot + symbolName + trailingDot

      }.mkString

    val ruleName = state.production.rule
      .map(_.name)
      .map(BnfHtml.sanitizeName)
      .getOrElse("NO NAME ???")

    s"""<tr class="state-row collapsed" data-chart-id="$chartIndex">
  <td>
    ${stateIndex}
  </td>
  <td>${ruleName}</td>
  <td>→</td>
  <td>
    ${productionStr}
  </td>
</tr>
"""
  }

}
