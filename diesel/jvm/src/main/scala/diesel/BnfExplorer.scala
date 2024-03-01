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

import java.io.File
import scala.util.Using
import java.io.BufferedWriter
import java.io.FileWriter
import java.awt.Desktop

object BnfExplorer {

  def dumpAndOpen(bnf: Bnf): Unit = {
    val f: File = File.createTempFile("bnf-", ".html")
    dumpToFile(bnf, f)
    val desktop = Desktop.getDesktop();
    desktop.browse(f.toURI());
  }

  def dumpToFile(bnf: Bnf, f: File): Unit = {
    println("writing to " + f.getAbsolutePath())
    val html: String = BnfHtml.dump(bnf)
    writeToFile(f, html)
  }

  def writeToFile(file: File, text: String): Unit = {
    Using(new BufferedWriter(new FileWriter(file))) { bufferedWriter =>
      bufferedWriter.write(text)
    }
  }

}
