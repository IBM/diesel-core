package diesel

import diesel.Dsl.{Axiom, Concept, Identifiers, Instance, Syntax}
import diesel.Lexer.Scanner
import munit.FunSuite

class NestedRepTest extends FunSuite {

  object NestedAst {

    case class Expression(ac: AndCondition, rest: Seq[AndCondition])
    case class AndCondition(conditions: Seq[Condition])

    sealed trait Condition
    case class CIsNotNull(o: Operand, not: Boolean)                    extends Condition
    case class CNot(e: Expression)                                     extends Condition
    case class COpCompOp(o: Operand, rest: Option[(Compare, Operand)]) extends Condition

    case class Operand(sa: Summand, rest: Seq[Summand])

    sealed trait Compare
    case object CNotEq extends Compare
    case object CGtEq  extends Compare
    case object CLtEq  extends Compare

    case class Summand(f: Factor, rest: Seq[Factor])

    case class Factor(f: Term, rest: Seq[Term])

    sealed trait Term
    case class TValue(v: Value)       extends Term
    case class TColumnName(v: String) extends Term

    sealed trait Value
    case object VNull             extends Value
    case class VNumber(v: String) extends Value

    sealed trait BooleanLiteral extends Value
    case object BTrue           extends BooleanLiteral
    case object BFalse          extends BooleanLiteral
  }

  import NestedAst._

  object NestedDsl extends Dsl with Identifiers {

    override def identScanner: Scanner = "[a-zA-Z][a-zA-Z0-9]*".r

    val cExpression: Concept[Expression]     = concept
    val cAndCondition: Concept[AndCondition] = concept
    val cCondition: Concept[Condition]       = concept
    val cOperand: Concept[Operand]           = concept
    val cCompare: Concept[Compare]           = concept
    val cSummand: Concept[Summand]           = concept
    val cFactor: Concept[Factor]             = concept
    val cTerm: Concept[Term]                 = concept
    val cValue: Concept[Value]               = concept

    val cNumber: Concept[Value] = concept[Value, Value](
      "\\d+".r,
      VNumber("0"),
      Some(cValue)
    ) map {
      case (_, t) =>
        VNumber(t.text)
    }

//    val sExpression1: Syntax[Expression] = syntax(cExpression)(
//      cAndCondition map {
//        case (_, (a)) =>
//          Expression(a, Seq.empty)
//      }
//    )

    val sExpression2: Syntax[Expression] = syntax(cExpression)(
      cAndCondition ~ ("OR" ~ cAndCondition).rep(true) map {
        case (_, (a, as)) =>
          Expression(a, as.map(_._2))
      }
    )

    val sAndCondition: Syntax[AndCondition] = syntax(cAndCondition)(
      cCondition ~ ("AND" ~ cCondition).rep(true) map {
        case (_, (c, cs)) =>
          AndCondition(c +: cs.map(_._2))
      }
    )

    val sCondition1: Syntax[Condition] = syntax(cCondition)(
      cOperand ~ (cCompare ~ cOperand).? map {
        case (_, (o, rest)) =>
          COpCompOp(o, rest)
      }
    )

    val sCondition2: Syntax[Condition] = syntax(cCondition)(
      "NOT" ~ cExpression map {
        case (_, (_, e)) =>
          CNot(e)
      }
    )

    val iCompare1: Instance[Compare] = instance(cCompare)("<>") map { _ =>
      CNotEq
    }

    val iCompare2: Instance[Compare] = instance(cCompare)("<=") map { _ =>
      CLtEq
    }

    val iCompare3: Instance[Compare] = instance(cCompare)(">=") map { _ =>
      CGtEq
    }

    val sOperand: Syntax[Operand] = syntax(cOperand)(
      cSummand ~ ("||" ~ cSummand).rep(true) map {
        case (_, (csa, csas)) =>
          Operand(csa, csas.map(_._2))
      }
    )

    val sSummand: Syntax[Summand] = syntax(cSummand)(
      cFactor ~ (("+" | "-") ~ cFactor).rep(true) map {
        case (_, (f, fs)) =>
          Summand(f, fs.map(_._2))
      }
    )

    val sFactor: Syntax[Factor] = syntax(cFactor)(
      cTerm ~ (("*" | "/") ~ cTerm).rep(true) map {
        case (_, (t, ts)) =>
          Factor(t, ts.map(_._2))
      }
    )

    val sTerm1: Syntax[Term] = syntax(cTerm)(
      cValue map {
        case (_, v) =>
          TValue(v)
      }
    )

    val sTerm2: Syntax[Term] = syntax(cTerm)(
      id map {
        case (_, v) =>
          TColumnName(v.text)
      }
    )

    val iValueTrueTrue: Instance[Value] = instance(cValue)("TRUE") map { _ => BTrue }

    val iValueTrueFalse: Instance[Value] = instance(cValue)("FALSE") map { _ => BFalse }

    val iValueNull: Instance[Value] = instance(cValue)("NULL") map { _ => VNull }

    val aValue: Axiom[Value] = axiom(cValue)

    val aExpression: Axiom[Expression] = axiom(cExpression)

  }

  test("expr") {
    AstHelpers.assertAst(
      NestedDsl,
      Some(NestedDsl.aExpression)
    )("123") { tree =>
      assertEquals(tree.markers, Seq.empty)
      assertEquals(
        tree.value,
        Expression(
          ac = AndCondition(conditions =
            List(
              COpCompOp(
                o = Operand(
                  sa = Summand(
                    f = Factor(
                      f = TValue(v =
                        VNumber(v =
                          "123"
                        )
                      ),
                      rest = Nil
                    ),
                    rest = Nil
                  ),
                  rest = Nil
                ),
                rest = None
              )
            )
          ),
          rest = Nil
        )
      )
    }
  }

  test("NOT expr") {
    AstHelpers.assertAst(
      NestedDsl,
      Some(NestedDsl.aExpression)
    )("NOT 123") { tree =>
      assertEquals(tree.markers, Seq.empty)
      assertEquals(
        tree.value,
        Expression(
          ac = AndCondition(conditions =
            List(
              CNot(e =
                Expression(
                  ac = AndCondition(conditions =
                    List(
                      COpCompOp(
                        o = Operand(
                          sa = Summand(
                            f = Factor(
                              f = TValue(v =
                                VNumber(v =
                                  "123"
                                )
                              ),
                              rest = Nil
                            ),
                            rest = Nil
                          ),
                          rest = Nil
                        ),
                        rest = None
                      )
                    )
                  ),
                  rest = Nil
                )
              )
            )
          ),
          rest = Nil
        )
      )
    }
  }

  test("expr 2") {
    AstHelpers.assertAst(
      NestedDsl,
      Some(NestedDsl.aExpression)
    )("1 <= 2") { tree =>
      assertEquals(tree.markers, Seq.empty)
      assertEquals(
        tree.value,
        Expression(
          ac = AndCondition(conditions =
            List(
              COpCompOp(
                o = Operand(
                  sa = Summand(
                    f = Factor(
                      f = TValue(v =
                        VNumber(v =
                          "1"
                        )
                      ),
                      rest = Nil
                    ),
                    rest = Nil
                  ),
                  rest = Nil
                ),
                rest = Some(value =
                  Tuple2(
                    _1 = CLtEq,
                    _2 = Operand(
                      sa = Summand(
                        f = Factor(
                          f = TValue(v =
                            VNumber(v =
                              "2"
                            )
                          ),
                          rest = Nil
                        ),
                        rest = Nil
                      ),
                      rest = Nil
                    )
                  )
                )
              )
            )
          ),
          rest = Nil
        )
      )
    }
  }

  test("NOT expr 2") {
    AstHelpers.assertAst(
      NestedDsl,
      Some(NestedDsl.aExpression)
    )("NOT 1 <= 2") { tree =>
      assertEquals(tree.markers, Seq.empty)
      assertEquals(
        tree.value,
        Expression(
          AndCondition(List(CNot(Expression(
            AndCondition(List(COpCompOp(
              Operand(Summand(Factor(TValue(VNumber("1")), List()), List()), List()),
              Some((CLtEq, Operand(Summand(Factor(TValue(VNumber("2")), List()), List()), List())))
            ))),
            List()
          )))),
          List()
        )
      )
    }
  }

  test("expr 3") {
    AstHelpers.assertAst(
      NestedDsl,
      Some(NestedDsl.aExpression)
    )("1 OR 2") { tree =>
      assertEquals(tree.markers, Seq.empty)
      assertEquals(
        tree.value,
        Expression(
          ac = AndCondition(conditions =
            List(
              COpCompOp(
                o = Operand(
                  sa = Summand(
                    f = Factor(
                      f = TValue(v =
                        VNumber(v =
                          "1"
                        )
                      ),
                      rest = Nil
                    ),
                    rest = Nil
                  ),
                  rest = Nil
                ),
                rest = None
              )
            )
          ),
          rest = List(
            AndCondition(conditions =
              List(
                COpCompOp(
                  o = Operand(
                    sa = Summand(
                      f = Factor(
                        f = TValue(v =
                          VNumber(v =
                            "2"
                          )
                        ),
                        rest = Nil
                      ),
                      rest = Nil
                    ),
                    rest = Nil
                  ),
                  rest = None
                )
              )
            )
          )
        )
      )
    }
  }

}
