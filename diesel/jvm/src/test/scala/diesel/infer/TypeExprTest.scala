package diesel.infer

import munit.FunSuite

class TypeExprTest extends FunSuite {

  val notSubType: TypeExpr.IsSubTypeOf = (_, _) => false
  val fooSubType: TypeExpr.IsSubTypeOf = (_, p) => p == "foo"

  implicit val isSubType: TypeExpr.IsSubTypeOf = notSubType

  test("normalize and with any") {
    assertEquals(
      TypeExpr.and(TypeExpr.Any, TypeExpr.Any),
      TypeExpr.Any
    )
  }

  test("normalize and with exact") {
    assertEquals(
      TypeExpr.and(TypeExpr.Any, TypeExpr.exact("foo")),
      TypeExpr.exact("foo")
    )
    assertEquals(
      TypeExpr.and(TypeExpr.exact("foo"), TypeExpr.Any),
      TypeExpr.exact("foo")
    )
  }

  test("normalize and with nothing") {
    assertEquals(
      TypeExpr.and(TypeExpr.Nothing, TypeExpr.Any),
      TypeExpr.Nothing
    )
    assertEquals(
      TypeExpr.and(TypeExpr.Any, TypeExpr.Nothing),
      TypeExpr.Nothing
    )
    assertEquals(
      TypeExpr.and(TypeExpr.Nothing, TypeExpr.exact("foo")),
      TypeExpr.Nothing
    )
    assertEquals(
      TypeExpr.and(TypeExpr.exact("foo"), TypeExpr.Nothing),
      TypeExpr.Nothing
    )
  }

  test("normalize or with any") {
    assertEquals(
      TypeExpr.or(TypeExpr.Any, TypeExpr.Any),
      TypeExpr.Any
    )
    assertEquals(
      TypeExpr.or(TypeExpr.Any, TypeExpr.exact("foo")),
      TypeExpr.exact("foo")
    )
  }

  test("normalize or with nothing") {
    assertEquals(
      TypeExpr.or(TypeExpr.Nothing, TypeExpr.Any),
      TypeExpr.Any
    )
    assertEquals(
      TypeExpr.or(TypeExpr.Nothing, TypeExpr.exact("foo")),
      TypeExpr.exact("foo")
    )
    assertEquals(
      TypeExpr.or(TypeExpr.exact("foo"), TypeExpr.Nothing),
      TypeExpr.exact("foo")
    )
  }

  test("normalize and with no sub types") {
    assertEquals(
      TypeExpr.and(TypeExpr.exact("foo"), TypeExpr.exact("bar")),
      TypeExpr.Nothing
    )
  }

  test("normalize and with sub types") {
    implicit val isSubType = fooSubType
    assertEquals(
      TypeExpr.and(TypeExpr.exact("foo"), TypeExpr.exact("bar")),
      TypeExpr.exact("foo")
    )
    assertEquals(
      TypeExpr.and(TypeExpr.exact("bar"), TypeExpr.exact("foo")),
      TypeExpr.exact("foo")
    )
    assertEquals(
      TypeExpr.and(TypeExpr.exact("gnu"), TypeExpr.exact("bar")),
      TypeExpr.Nothing
    )
  }

  test("normalize or with sub types") {
    implicit val isSubType = fooSubType
    assertEquals(
      TypeExpr.or(TypeExpr.exact("foo"), TypeExpr.exact("bar")),
      TypeExpr.or(TypeExpr.exact("foo"), TypeExpr.exact("bar"))
    )
    assertEquals(
      TypeExpr.or(TypeExpr.exact("bar"), TypeExpr.exact("foo")),
      TypeExpr.or(TypeExpr.exact("bar"), TypeExpr.exact("foo"))
    )
    assertEquals(
      TypeExpr.or(TypeExpr.exact("gnu"), TypeExpr.exact("bar")),
      TypeExpr.or(TypeExpr.exact("gnu"), TypeExpr.exact("bar"))
    )
  }

  test("normalize nested and") {
    implicit val isSubType = fooSubType
    assertEquals(
      TypeExpr.and(
        TypeExpr.exact("gnu"),
        TypeExpr.and(TypeExpr.exact("foo"), TypeExpr.exact("bar"))
      ),
      TypeExpr.exact("foo")
    )
    assertEquals(
      TypeExpr.and(
        TypeExpr.and(TypeExpr.exact("foo"), TypeExpr.exact("bar")),
        TypeExpr.exact("gnu")
      ),
      TypeExpr.exact("foo")
    )
  }

  test("normalize same and") {
    assertEquals(
      TypeExpr.and(
        TypeExpr.exact("gnu"),
        TypeExpr.exact("gnu")
      ),
      TypeExpr.exact("gnu")
    )
  }

  test("normalize same or") {
    assertEquals(
      TypeExpr.or(
        TypeExpr.exact("gnu"),
        TypeExpr.exact("gnu")
      ),
      TypeExpr.exact("gnu")
    )
  }

  test("normalize nested or") {
    implicit val isSubType = fooSubType
    assertEquals(
      TypeExpr.or(
        TypeExpr.exact("gnu"),
        TypeExpr.or(TypeExpr.exact("foo"), TypeExpr.exact("bar"))
      ),
      TypeExpr.or(
        TypeExpr.or(TypeExpr.exact("gnu"), TypeExpr.exact("foo")),
        TypeExpr.or(TypeExpr.exact("gnu"), TypeExpr.exact("bar"))
      )
    )
    assertEquals(
      TypeExpr.or(
        TypeExpr.or(TypeExpr.exact("foo"), TypeExpr.exact("bar")),
        TypeExpr.exact("gnu")
      ),
      TypeExpr.or(
        TypeExpr.or(TypeExpr.exact("foo"), TypeExpr.exact("gnu")),
        TypeExpr.or(TypeExpr.exact("bar"), TypeExpr.exact("gnu"))
      )
    )
  }

  test("infer from signature") {
    val args      = Seq(TypeExpr.exact("gnu"))
    val signature = Signature(TypeExpr.exact("foo"), Seq(TypeExpr.exact("gnu")))
    val inferred  = TypeExpr.infer(args, signature)
    assertEquals(inferred, TypeExpr.exact("foo"))
  }

  test("infer from signature mismatch") {
    val args      = Seq(TypeExpr.exact("bar"))
    val signature = Signature(TypeExpr.exact("foo"), Seq(TypeExpr.exact("gnu")))
    val inferred  = TypeExpr.infer(args, signature)
    assertEquals(inferred, TypeExpr.Nothing)
  }

}
