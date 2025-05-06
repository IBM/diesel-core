package diesel.infer

trait TypeExpr

object TypeExpr {
  case object Any                          extends TypeExpr
  case object Nothing                      extends TypeExpr
  case class And(a: TypeExpr, b: TypeExpr) extends TypeExpr
  case class Or(a: TypeExpr, b: TypeExpr)  extends TypeExpr
  case class Exact(c: String)              extends TypeExpr

  type IsSubTypeOf = (String, String) => Boolean

  def exact(c: String): TypeExpr                                               = Exact(c)
  def and(a: TypeExpr, b: TypeExpr)(implicit isSubType: IsSubTypeOf): TypeExpr =
    normalize(And(a, b))
  def or(a: TypeExpr, b: TypeExpr)(implicit isSubType: IsSubTypeOf): TypeExpr  = normalize(Or(a, b))

  private def normalize(e: TypeExpr)(implicit isSubType: IsSubTypeOf): TypeExpr = {
    e match {
      case And(Any, b)             => b
      case And(a, Any)             => a
      case And(Nothing, _)         => Nothing
      case And(_, Nothing)         => Nothing
      case Or(Nothing, b)          => b
      case Or(a, Nothing)          => a
      case Or(Any, b)              => b
      case Or(a, Any)              => a
      case And(a: Exact, b: Exact) =>
        if (isSubType(a.c, b.c)) {
          b
        } else if (isSubType(b.c, a.c)) {
          a
        } else {
          Nothing
        }
      case And(And(a, b), c)       =>
        normalize(And(And(a, c), And(b, c)))
      case And(a, And(b, c))       =>
        normalize(And(And(a, b), And(a, c)))
      case Or(a: Exact, b: Exact)  =>
        if (a == b) {
          a
        } else {
          e
        }
      case Or(Or(a, b), c: Exact)  =>
        normalize(Or(Or(a, c), Or(b, c)))
      case Or(a: Exact, Or(b, c))  =>
        normalize(Or(Or(a, b), Or(a, c)))
      case _                       => e
    }
  }
}
