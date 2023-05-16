package diesel

import munit.FunSuite

class CompletionProcessorTest extends FunSuite {

  private def doCheck(text: String, offset: Int, expected: String): Unit = {
    val prefix = CompletionProcessor.findPrefix(text, offset, DefaultCompletionLookback)
    assertEquals(prefix, expected)
  }

  test("empty string") {
    doCheck("", 0, "")
  }

  test("whitespaces") {
    doCheck("   ", 1, "")
  }

  test("inside id 0") {
    doCheck("foo bar", 0, "")
  }

  test("inside id 1") {
    doCheck("foo bar", 1, "f")
  }

  test("inside id 3") {
    doCheck("foo bar", 3, "foo")
  }

  test("inside id 4") {
    doCheck("foo bar", 4, "")
  }

  test("inside id 5") {
    doCheck("foo bar", 5, "b")
  }

  test("inside id 7") {
    doCheck("foo bar", 7, "bar")
  }

  test("eos") {
    doCheck("ro", 2, "ro")
  }

}
