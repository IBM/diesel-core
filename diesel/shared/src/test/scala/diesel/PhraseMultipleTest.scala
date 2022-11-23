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
import diesel.voc.{DefiniteArticle, Vocabulary}
import diesel.voc.i18n.EnglishVerbalizer
import munit.FunSuite

class PhraseMultipleTest extends FunSuite {

  trait ANumber
  case class ANumberValue(value: Int)            extends ANumber
  case class ALength(nm: ANumberMultiple)        extends ANumber
  case class AAdd(left: ANumber, right: ANumber) extends ANumber
  trait ANumberMultiple
  case class APrime(nm: ANumber)                 extends ANumberMultiple

  trait BootVoc extends Dsl {

    val number: Concept[ANumber] = concept[ANumber]("\\d+".r, ANumberValue(0)) map {
      case (_, t) =>
        ANumberValue(t.text.toInt)
    }

    val length: Phrase[ANumber] = phrase[ANumber](number)(
      pS("length").subject ~ pS("of") ~ pR(number).multiple[ANumberMultiple].verbalization(
        SPVerbalizationContext(Some(DefiniteArticle))
      ) map {
        case (_, (_, _, nm)) =>
          ALength(nm)
      }
    )

    val prime: Phrase[ANumberMultiple] = phraseMultiple[ANumber, ANumberMultiple](number)(
      pS("prime").subject ~ pR(number) map {
        case (_, (_, n)) =>
          APrime(n)
      }
    )

    val add: Syntax[ANumber] = syntax(number)(number.verbalization(
      SPVerbalizationContext(Some(DefiniteArticle))
    ) ~ "+" ~ number.verbalization(SPVerbalizationContext(Some(DefiniteArticle))) map {
      case (_, (left, _, right)) =>
        AAdd(left, right)
    })
  }

  object MyDsl extends BootVoc {

    val a: Axiom[ANumber] = axiom(number)

  }

  test("number literal") {
    AstHelpers.assertAst(MyDsl)("123") { tree =>
      assert(tree.markers.isEmpty)
      assert(tree.root.value == ANumberValue(123))
    }
  }

  test("length of prime") {
    AstHelpers.assertAst(MyDsl)("length of prime 123") { tree =>
      assert(tree.markers.isEmpty)
      assert(tree.root.value == ALength(APrime(ANumberValue(123))))
    }
  }

  test("length of prime with verbalizer") {
    val enVerbalizer = new EnglishVerbalizer(Vocabulary.empty)
    AstHelpers.assertAst(MyDsl, Some(enVerbalizer))("length of the primes 123") { tree =>
      assert(tree.markers.isEmpty)
      assert(tree.root.value == ALength(APrime(ANumberValue(123))))
    }
  }

  test("add") {
    val enVerbalizer = new EnglishVerbalizer(Vocabulary.empty)
    AstHelpers.assertAst(MyDsl, Some(enVerbalizer))("3 + the length of the primes 123") { tree =>
      assert(tree.markers.isEmpty)
      assert(tree.root.value == AAdd(ANumberValue(3), ALength(APrime(ANumberValue(123)))))
    }
  }
}
