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

  // def dump(bnf: Bnf, path: String): Unit = {
  //   Platform.consoleWithFile(path) {
  //     dump(bnf)
  //   }
  // }

  def dump(bnf: Bnf): String = s"""<html>
<head>
<style type="text/css">
    .rule-row td {
      vertical-align: top;
      //border-top: 1px solid lightgray;
    }

    td.rule-name {
      border-left: 2px solid lightgray;
      padding-left: 4px;
    }

    .rule-row.selected td.rule-name {
      border-left: 2px solid blue;
    }

    .rule-row {
      margin-bottom: 8px;
    }
    
    .rule-row.odd {
      background-color: #F0F0F0;
    }

    .rule-row.selected, .rule-row .link.selected {
      background-color: #CCCCFF;
    }

    a.link {
      color: darkblue;
      text-decoration: none;
    }

    a.link:visited {
      color: darkblue;
    }
</style>
</head>  
<body>
<title>BNF</title>
<h2>Axioms</h2>
<table cellspacing="0" cellpadding="0">
${bnf.axioms.sortBy(_.name).map(dumpAxiom).mkString("")}
</table>
<h2>Rules <button id="clear-selection">Clear selection</button></h2>
<table cellspacing="0" cellpadding="0">
${bnf.rules
      .filter(_.isRule)
      .sortBy(_.name)
      .map(_.asInstanceOf[Rule])
      .map(dumpRule)
      .mkString("\n")}
</table>
<script type="text/javascript">
  window.addEventListener('hashchange', function() {
    const h = window.location.hash;
    if (h) {
      const name = h.substring(1);
      productionSelected(name);
    }
  });

  function productionSelected(name) {
    document.querySelectorAll(".rule-row")
      .forEach(tr => {
        tr.classList.remove("selected");
        const ruleName = tr.getAttribute("data-rule-name");
        if (ruleName === name) {
          tr.classList.add("selected");
        }
      });
    document.querySelectorAll("a.link")
      .forEach(l => {
        l.classList.remove("selected");
        const linkName = l.getAttribute("data-name");
        if (linkName === name) {
          l.classList.add("selected");
        }
      });
  }

  document.querySelectorAll(".rule-row").forEach((r,i) => {
    if (i % 2 !== 0) {
      r.classList.add("odd");
    }
  });

  document.getElementById("clear-selection").addEventListener("click", () => {
    productionSelected();
  })
</script>
</body>
</html>"""

  private def dumpAxiom(a: Axiom): String = {
    s"""<tr class="rule-row" data-rule-name="${a.name}"><td class="rule-name">${toAnchor(
        a.name
      )}${a.name}</td><td>""" ++ dumpProduction(a.production) ++
      "</td></tr>"
  }

  private def dumpRule(r: Rule): String = {
    s"""<tr class="rule-row" data-rule-name="${r.name}"><td class="rule-name">${toAnchor(
        r.name
      )}${toLink(r.name)}</td><td>""" ++
      r.productions.map(dumpProduction).mkString("") ++
      "</td></tr>"
  }

  private def dumpProduction(p: Bnf.Production): String =
    s"""<div> | ${p.symbols.map(dumpSymbol).mkString(" ~ ")}</div>"""

  private def dumpSymbol(s: Bnf.Symbol): String = s match {
    case Bnf.Token(name, _, _) => s"""<tt>"$name"</tt>"""
    case r: Bnf.Rule           => toLink(r.name)
    case _                     => ""
  }

  private def toAnchor(name: String): String =
    s"""<a name="$name"></a>"""

  private def toLink(name: String): String =
    s"""<a href="#$name" class="link" data-name="$name">$name</a>"""

}
