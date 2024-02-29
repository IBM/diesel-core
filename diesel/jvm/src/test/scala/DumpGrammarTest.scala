import munit.FunSuite
import java.io.File
import diesel.BnfHtml
import diesel.Bnf
import scala.util.Using
import java.io.BufferedWriter
import java.io.FileWriter
import java.awt.Desktop
import diesel.samples.calc.MyDsl

class DumpGrammarTest extends FunSuite {

  test("dump grammar".ignore) {
    // fail("wtf")
    val f: File      = File.createTempFile("bnf-", ".html")
    println("writing to " + f.getAbsolutePath())
    val html: String = BnfHtml.dump(Bnf(MyDsl))
    Using(new BufferedWriter(new FileWriter(f))) { bufferedWriter =>
      bufferedWriter.write(html)
    }
    val desktop      = Desktop.getDesktop();
    desktop.browse(f.toURI());
  }

}
