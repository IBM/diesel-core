package diesel

import java.io.File
import java.awt.Desktop

object DumpStates {

  val dotChar: String        = """<span class="dot">•</span>"""
  val eos: String            = """<span class="eof">ϵ</span>"""
  val caretCollapsed: String = "[+]"
  val caretExpanded: String  = "[-]"

  def dumpAndOpen(text: String, result: Result): Unit = {
    val f: File = File.createTempFile("diesel-result-", ".html")
    val r       = dumpResult(text, result)
    BnfExplorer.writeToFile(f, r)
    val desktop = Desktop.getDesktop();
    desktop.browse(f.toURI());
  }

  def dumpResult(text: String, result: Result): String = {
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
        border: none;
        background: none;
        font-size: 1.2rem;
        font-family: monospace;
      }

      .chart-row {
        font-size: 1.2rem;
      }

      .chart-header-part {
        font-family: monospace;
      }
</style>
</head>
<body>
<h2>Input</h2>
<code>
${text}
</code>
<h2>Result</h1>
<button id="expand-all">Expand all</button>
<button id="collapse-all">Collapse all</button>
<table cellspacing="0" cellpadding="4">
    <tbody>
      ${result.getCharts
        .map(dumpChart(text, _))
        .mkString("")}
    </tbody>
</table>
</body>
<script>

function expandCollapse(chartIndex, expand) {
  document.querySelectorAll("button.expand-collapse")
    .forEach(btn => {
      const chartId = btn.getAttribute('data-chart-id');
      if (chartId === chartIndex) {
        if (expand) {
          btn.classList.add("expanded");
        } else {
          btn.classList.remove("expanded");
        }
        btn.innerHTML = expand ? "$caretExpanded" : "$caretCollapsed"; 
      }
    });

  document.querySelectorAll(".state-row")
    .forEach(stateRow => {
      const chartId = stateRow.getAttribute("data-chart-id");
      if (chartId === chartIndex) {
        if (expand) {
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
      const expanded = btn.classList.contains("expanded");
      expandCollapse(chartId, !expanded);
    })
  });

document.getElementById("expand-all").addEventListener('click', () => {
  document.querySelectorAll("button.expand-collapse")
  .forEach(btn => {
    const chartId = btn.getAttribute('data-chart-id');
    expandCollapse(chartId, true);
  });
});

document.getElementById("collapse-all").addEventListener('click', () => {
  document.querySelectorAll("button.expand-collapse")
  .forEach(btn => {
    const chartId = btn.getAttribute('data-chart-id');
    expandCollapse(chartId, false);
  });
});

</script>
</html>
"""
  }

  private def wrapToken(s: String): String =
    s"""<span class="token">$s</span>"""

  def dumpChart(text: String, chart: Chart): String = {
    val chartHeader = chart.token
      .map { token =>
        val textBefore = text.substring(0, token.offset)
        val textAfter  = text.substring(token.offset)
        s"""
<span class="chart-header-part">$textBefore</span>
$dotChar
<span class="chart-header-part">$textAfter}</span>
        """
      }
      .getOrElse("No token !")
    //    (
    //   tokensBefore ++ Seq(dotChar) ++ tokensAfter
    // ).mkString("")

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
