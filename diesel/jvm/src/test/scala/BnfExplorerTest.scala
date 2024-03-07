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

import munit.FunSuite
import java.io.File
import diesel.Bnf
import diesel.samples.calc.MyDsl
import diesel.BnfExplorer
import scala.io.Source

class DumpGrammarTest extends FunSuite {

  test("dump") {
    val f: File = File.createTempFile("bnf-", ".html")
    val bnf     = Bnf(MyDsl)
    // BnfExplorer.dumpAndOpen(bnf)
    BnfExplorer.dumpToFile(bnf, f)
    val html    = Source.fromFile(f).mkString
    assert(html.contains("value[number,SINGLE,_,_,_]"))
  }

}
