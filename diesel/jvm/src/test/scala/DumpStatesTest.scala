import munit.FunSuite
import diesel.samples.calc.MyDsl
import diesel.AstHelpers
import diesel.DumpStates

class DumpStatesTest extends FunSuite {

  test("header".ignore) {
    val text   = "1 + 2 + cos( 12 )"
    val result = AstHelpers.parse(MyDsl, text)
    DumpStates.dumpAndOpen(text, result)
  }

}
