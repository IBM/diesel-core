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

import diesel.Dsl._
import diesel.voc.i18n.EnglishVerbalizer
import diesel.voc.{Concept => _, _}
import munit.FunSuite

class VerbalizationContextInDslTest extends FunSuite {

  trait AValue
  trait ANumber                          extends AValue
  case class ANumberValue(value: Double) extends ANumber
  case class AString(value: String)      extends AValue
  trait AVoid
  case class APrint(value: AValue)       extends AVoid

  class BootVoc extends Dsl {

    val value: Concept[AValue] = concept

    val number: Concept[ANumber] =
      concept[ANumber, AValue]("\\d+".r, ANumberValue(0.0), Some(value)) map {
        case (_, t) =>
          ANumberValue(t.text.toDouble)
      }

    val string: Concept[AString] =
      concept[AString, AValue]("\"([^\"\\\\]|\\\\.)*\"".r, AString(""), Some(value)) map {
        case (_, t) =>
          AString(t.text.drop(1).dropRight(1))
      }

    val void: Concept[AVoid] = concept

    val print: Phrase[AVoid] = phrase(void)(
      pS("print") ~ pR(value) map [AVoid] {
        case (_, (_, s)) =>
          APrint(s)
      }
    )

  }

  sealed trait AVerbalized
  case class ADefinite(value: ANumber)           extends AVerbalized
  case class AMin(left: ANumber, right: ANumber) extends ANumber
  case class AMax(left: ANumber, right: ANumber) extends ANumber
  case class AInDefinite(value: ANumber)         extends AVerbalized
  case object Pi                                 extends ANumber
  case class Statement(void: AVoid)              extends AVerbalized

  object MyDsl extends BootVoc {

    val pi: Phrase[ANumber] = phrase(number)(
      pS("pi").subject map [ANumber] {
        case (_, t) =>
          Pi
      }
    )

    // custom "user" Dsl, using voc
    // ----------------------------

    val verbalized: Concept[AVerbalized] = concept

    val definite: Syntax[AVerbalized] = syntax[AVerbalized](verbalized)(
      // references "number" and decorates with article
      "definite" ~ number.article(DefiniteArticle) map {
        case (_, (_, i)) =>
          // maps to user Ast type
          ADefinite(i)
      }
    )

    val indefinite: Syntax[AVerbalized] = syntax[AVerbalized](verbalized)(
      // references "number" and decorates with article
      "indefinite" ~ number.article(IndefiniteArticle) map {
        case (_, (_, i)) =>
          // maps to user Ast type
          AInDefinite(i)
      }
    )

    val min: Syntax[ANumber] = syntax[ANumber](number)(
      // references "number" and decorates with article
      "min" ~ "(" ~ number.article(DefiniteArticle) ~ "," ~ number ~ ")" map {
        case (_, (_, _, l, _, r, _)) =>
          // maps to user Ast type
          AMin(l, r)
      }
    )

    val max: Phrase[ANumber] = phrase[ANumber](number)(
      // references "number" and decorates with article
      pS("max").subject ~ pS("(") ~ pR(number).verbalization(
        SPVerbalizationContext(Some(DefiniteArticle))
      ) ~ pS(
        ","
      ) ~ pR(
        number
      ) ~ pS(")") map [ANumber] {
        case (_, (_, _, l, _, r, _)) =>
          // maps to user Ast type
          AMax(l, r)
      }
    )

    val s: Syntax[AVerbalized] = syntax(verbalized)(
      void map {
        case (_, v) =>
          Statement(v)
      }
    )

    val a: Axiom[AVerbalized] = axiom(verbalized)

  }

  private val verbalizer = new EnglishVerbalizer(Vocabulary.empty)

  test("definite") {
    val text =
      """
        |definite the pi
        |""".stripMargin
    AstHelpers.assertAst(MyDsl, verbalizer = Some(verbalizer))(text) { tree =>
      assert(tree.markers.isEmpty)
      assert(tree.root.value == ADefinite(Pi))
    }
  }

  test("definite bad") {
    val text =
      """
        |definite a pi
        |""".stripMargin
    AstHelpers.assertAst(MyDsl, verbalizer = Some(verbalizer))(text) { tree =>
      assert(tree.markers.length == 1)
      assert(
        tree.markers.head.message.format("en") == "The word 'the' is expected in place of 'a'."
      )
    }
  }

  test("indefinite") {
    val text =
      """
        |indefinite a pi
        |""".stripMargin
    AstHelpers.assertAst(MyDsl, verbalizer = Some(verbalizer))(text) { tree =>
      assert(tree.markers.isEmpty)
      assert(tree.root.value == AInDefinite(Pi))
    }
  }

  test("indefinite 123") {
    val text =
      """
        |indefinite 123
        |""".stripMargin
    AstHelpers.assertAst(MyDsl, verbalizer = Some(verbalizer))(text) { tree =>
      assert(tree.markers.isEmpty)
      assert(tree.root.value == AInDefinite(ANumberValue(123)))
    }
  }

  test("min") {
    val text =
      """
        |print min(the max(the pi, pi), pi)
        |""".stripMargin
    AstHelpers.assertAst(MyDsl, verbalizer = Some(verbalizer))(text) { tree =>
      assert(tree.markers.isEmpty)
      assert(tree.root.value == Statement(APrint(AMin(AMax(Pi, Pi), Pi))))
    }
  }

  test("max") {
    val text =
      """
        |print max(the max(the pi, pi), pi)
        |""".stripMargin
    AstHelpers.assertAst(MyDsl, verbalizer = Some(verbalizer))(text) { tree =>
      assert(tree.markers.isEmpty)
      assert(tree.root.value == Statement(APrint(AMax(AMax(Pi, Pi), Pi))))
    }
  }

  test("print") {
    AstHelpers.assertAst(MyDsl, verbalizer = Some(verbalizer))("print pi") { tree =>
      assert(tree.markers.isEmpty)
      assert(tree.root.value == Statement(APrint(Pi)))
    }
  }

  //  test("round") {
//    AstHelpers.assertAst(MyDsl)("""round 123""") { tree =>
//      assert(tree.markers.isEmpty)
//      assert(tree.root.value == SentenceNode("round", Seq(ConceptNode(MyVoc.number.identifier, "123"))))
//    }
//  }
//
//  test("round of round") {
//    AstHelpers.assertAst(MyDsl)("""round round 123""") { tree =>
//      assert(tree.markers.isEmpty)
//      assert(tree.root.value ==
//        SentenceNode(
//          "round",
//          Seq(
//            SentenceNode(
//              "round",
//              Seq(
//                ConceptNode(MyVoc.number.identifier, "123")
//              )
//            )
//          )
//        )
//      )
//    }
//  }

}
