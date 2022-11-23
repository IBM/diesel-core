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

package diesel.voc

import diesel.voc.i18n.EnglishVerbalizer
import munit.FunSuite

class EnglishVerbalizerTest extends FunSuite {

  private val vc = VerbalizationContext()

  private def verbalize(context: VerbalizationContext, verbalizable: Verbalizable): String =
    new EnglishVerbalizer(CustomerVoc.voc).verbalize(context, verbalizable)

  test("concept no article") {
    assert(
      verbalize(vc, ConceptVerbalizable(CustomerVoc.customer)) ==
        "customer"
    )
  }

  private val custV = ConceptVerbalizable(CustomerVoc.customer)

  test("concept definite") {
    assert(
      verbalize(vc.copy(article = DefiniteArticle), custV) == "the customer"
    )
  }

  test("concept definite partitive") {
    assert(
      verbalize(vc.copy(article = DefiniteArticle, partitive = true), custV) == "of the customer"
    )
  }

  test("concept definite plural") {
    assert(
      verbalize(vc.copy(article = DefiniteArticle, plural = true), custV) == "the customers"
    )
  }

  test("concept indefinite") {
    assert(
      verbalize(vc.copy(article = IndefiniteArticle), custV) == "a customer"
    )
  }

  test("concept possessive") {
    assert(
      verbalize(vc.copy(article = PossessiveArticle), custV) == "its customer"
    )
  }

  test("concept demonstrative") {
    assert(
      verbalize(vc.copy(article = DemonstrativeArticle), custV) == "this customer"
    )
  }

  test("concept demonstrative plural") {
    assert(
      verbalize(vc.copy(article = DemonstrativeArticle, plural = true), custV) == "these customers"
    )
  }

  test("concept quantitative") {
    assert(
      verbalize(vc.copy(article = QuantitativeArticle), custV) == "all customer"
    )
  }

  test("concept quantitative plural") {
    assert(
      verbalize(vc.copy(article = QuantitativeArticle, plural = true), custV) == "all customers"
    )
  }

  test("concept each") {
    assert(
      verbalize(vc.copy(article = EachArticle), custV) == "each customer"
    )
  }

  private val role1 = RoleVerbalizable(CustomerVoc.role1)

  test("role no article") {
    assert(
      verbalize(vc, role1) == "age"
    )
  }

  test("role definite") {
    assert(
      verbalize(vc.copy(article = DefiniteArticle), role1) == "the age"
    )
  }

  test("role definite plural") {
    assert(
      verbalize(vc.copy(article = DefiniteArticle, plural = true), role1) == "the ages"
    )
  }

  test("role possessive") {
    assert(
      verbalize(vc.copy(article = PossessiveArticle), role1) == "its age"
    )
  }

  test("role possessive plural") {
    assert(
      verbalize(vc.copy(article = PossessiveArticle, plural = true), role1) == "its ages"
    )
  }

  private val johnDoe = ConceptInstanceVerbalizable(CustomerVoc.johnDoe)

  test("instance no article") {
    assert(
      verbalize(vc, johnDoe) == "john"
    )
  }

  test("instance definite") {
    assert(
      verbalize(vc.copy(article = DefiniteArticle), johnDoe) == "the john"
    )
  }

  test("instance definite plural") {
    assert(
      verbalize(vc.copy(article = DefiniteArticle, plural = true), johnDoe) == "the johns"
    )
  }

  test("role with glossary term") {
    assert(verbalize(vc, RoleVerbalizable(CustomerVoc.roleYalla1)) == "yalla")
    assert(verbalize(vc.copy(plural = true), RoleVerbalizable(CustomerVoc.roleYalla1)) == "yallaS")
    val yalla = CustomerVoc.glossary.getTerm("yalla").get
    assert(yalla.pluralLabel.contains("yallaS"))
    assert(yalla.getTermPropertyValue("chnek").isEmpty)
  }

  test("verbalizer builders test") {
    val ab = new EnglishVerbalizer(CustomerVoc.voc).articleBuilder
    assert(ab.getArticle(vc, custV).isEmpty)
    assert(ab.getArticle(vc.copy(article = DefiniteArticle), custV).get == "the")
  }

}
