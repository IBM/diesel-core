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

object CustomerVoc {
  import SentenceCategory._

  val number: Concept   = Concept("number", "MyNumber", Seq())
  val customer: Concept = Concept("customer", "MyCustomer", Seq())

  val johnDoe: ConceptInstance = ConceptInstance("john", "john", customer.identifier)

  // Customer.age

  val role0: Role = Role(holder = true, Some("customer"), customer.identifier, 0)
  val role1: Role = Role(holder = false, Some("age"), number.identifier, 1)

  val getAge: Sentence = Sentence(
    Navigation,
    "{1} of {0}",
    Seq(
      SyntacticRole(Object, Single, role0.index),
      SyntacticRole(Subject, Single, role1.index)
    )
  )
  val setAge: Sentence = Sentence(
    Action,
    "set the age of {0} to {1}",
    Seq(
      SyntacticRole(Object, Single, role0.index),
      SyntacticRole(Subject, Single, role1.index)
    )
  )

  val customerAge: FactType = FactType(
    "age",
    Seq(role0, role1),
    Seq(setAge, getAge)
  )

  // Customer.yalla => in glossary

  val roleYalla0: Role = Role(holder = true, Some("customer"), customer.identifier, 0)
  val roleYalla1: Role = Role(holder = false, Some("yalla"), number.identifier, 1)

  val getYalla: Sentence = Sentence(
    Navigation,
    "{1} of {0}",
    Seq(
      SyntacticRole(Object, Single, roleYalla0.index),
      SyntacticRole(Subject, Single, roleYalla1.index)
    )
  )
  val setYalla: Sentence = Sentence(
    Action,
    "set the yalla of {0} to {1}",
    Seq(
      SyntacticRole(Object, Single, roleYalla0.index),
      SyntacticRole(Subject, Single, roleYalla1.index)
    )
  )

  val customerYalla: FactType = FactType(
    "yalla",
    Seq(roleYalla0, roleYalla1),
    Seq(setYalla, getYalla)
  )

  val glossary: Glossary = Glossary(
    Seq(
      Term(
        "yalla",
        Map(Glossary.TERM_PLURAL_LITERAL -> "yallaS", Glossary.TERM_GENDER_LITERAL -> Gender.MALE)
      )
    )
  )

  val voc: Vocabulary =
    Vocabulary(glossary, Seq(number, customer), Seq(johnDoe), Seq(customerAge, customerYalla))

}
