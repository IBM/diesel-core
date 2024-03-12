package diesel.samples.feel

import diesel.{GenericNode, Reducer}
import diesel.Reducer.Kind.{Kind, Worse}
import diesel.samples.feel.Ast._

class FeelReducer(val node: GenericNode) extends Reducer {

  private def compareBoxed(b1: BoxedExpression, b2: BoxedExpression): Kind = b1 match {
    case BEList(es)   => ???
    case BEFunDef(f)  => ???
    case BEContext(c) =>
      ???
  }

  private def compareTextual(t1: TextualExpression, t2: TextualExpression): Kind = {
    t1 match {
      case TEFor(e)   => ???
      case TEIf(e)    => ???
      case TEQuant(e) => ???
      case TEDisj(e)  => ???
      case TEConj(e)  => ???
      case TEComp(e)  => ???
      case TEArith(e) =>
        ???

      case TEInstanceOf(e) => ???
      case TEPath(e)       => ???
      case TEFilter(e)     => ???
      case TEFuncInv(e)    => ???
      case TELiteral(e)    => ???
      case TESPUT(e)       => ???
      case TEName(_)       =>
        t2 match {
          case TEName(_) =>
            ???
          case _         =>
            Worse
        }
      case TEParens(e)     => ???
    }
  }

  private def compareExpressions(e1: Expression, e2: Expression): Kind = {
    e1 match {
      case EBoxed(b1)   =>
        e2 match {
          case EBoxed(b2)  =>
            compareBoxed(b1, b2)
          case ETextual(_) =>
            ???
        }
      case ETextual(t1) =>
        e2 match {
          case EBoxed(_)    =>
            ???
          case ETextual(t2) =>
            compareTextual(t1, t2)
        }
    }
  }

  override def compare(other: GenericNode): (Kind, Reducer) = {
    node.value match {
      case ContextEntry(_, e1) =>
        other.value match {
          case ContextEntry(_, e2) =>
            (compareExpressions(e1, e2), this)
          case _                   =>
            ???
        }
      case _                   =>
        ???
    }
  }
}
