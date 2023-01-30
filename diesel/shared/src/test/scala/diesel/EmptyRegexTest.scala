package diesel

import diesel.Dsl.{Axiom, Concept}
import munit.FunSuite

class EmptyRegexTest extends FunSuite {

  object MyDsl extends Dsl {

    val c1: Concept[String] = concept("".r, "") map { case (_,_) => "yalla" }

    val a1: Axiom[String] = axiom(c1)

  }

  test("empty regex throws") {
    val ex = intercept[IllegalArgumentException](AstHelpers.parse(MyDsl, "1313"))
    assertEquals(ex.getMessage, "found scanner matching empty string RegexScanner(), tokenId=c1")
  }

}
