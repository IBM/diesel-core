import munit.FunSuite
import diesel.Dsl
import diesel.Dsl._
import diesel.AstHelpers
import diesel.Result
import diesel.BnfExplorer
import diesel.Bnf

class DslifyTest extends FunSuite {

    object Ast {
        case class Number(v: String)
        sealed trait Operation
        case class Add(a: Number, b: Number) extends Operation
        case class Mul(a: Number, b: Number) extends Operation
    }

    import Ast._

    object MyDsl extends Dsl {

        val cInt: Concept[Number] = concept("\\d+".r, Number("0")) map { case (_,t) => Number(t.text) }
        
        val cOp: Concept[Operation] = concept[Operation]

        val sAdd: Syntax[Operation] = syntax(cOp)(cInt ~ "add" ~ cInt map {
            case (_,(l,_,r)) => 
                Add(l, r)
        })

        val sMul: Syntax[Operation] = syntax(cOp)(cInt ~ "mul" ~ cInt map {
            case (_,(l,_,r)) => 
                Mul(l, r)
        })

        val a: Axiom[Operation] = axiom(cOp)
    }

    test("parse") {
        AstHelpers.withAst(MyDsl)("1 add 2") {
            t => {
                AstHelpers.assertNoMarkers(t)
                assertEquals(
                    t.value,
                    Add(Number("1"), Number("2"))
                )
            }
        }
    }

    // test("dump bnf") {
    //     val bnf = Bnf(MyDsl)
    //     BnfExplorer.dumpAndOpen(bnf)
    // }


    // test("simple") {
    //     val dsl = dslify(bnf, "1 add 2")
    //     assertEquals(dsl, "1 add 2")
    // }
  
}
