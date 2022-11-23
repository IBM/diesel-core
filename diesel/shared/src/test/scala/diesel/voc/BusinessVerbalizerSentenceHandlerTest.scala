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

import diesel.voc.SentenceProcessor.BusinessVerbalizerSentenceHandler
import diesel.voc.i18n.EnglishVerbalizer
import munit.FunSuite

import java.io.StringWriter

class BusinessVerbalizerSentenceHandlerTest extends FunSuite {

  private def verbalizeSentence(
    context: VerbalizationContext,
    sentence: Sentence,
    roles: Seq[Role]
  ): String = {
    val verbalizer = new EnglishVerbalizer(CustomerVoc.voc)
    val writer     = new StringWriter()
    try {
      val handler = new BusinessVerbalizerSentenceHandler(roles, writer, verbalizer)
      SentenceProcessor.processSentence(verbalizer, sentence, context, handler)
    } finally {
      writer.close()
    }
    writer.toString
  }

  private val vc = VerbalizationContext()

  test("getter default") {
    assert(
      verbalizeSentence(
        vc,
        CustomerVoc.getAge,
        CustomerVoc.customerAge.roles
      ) == "the age of <a customer>"
    )
  }

  test("getter plural") {
    assert(
      verbalizeSentence(
        vc.copy(plural = true),
        CustomerVoc.getAge,
        CustomerVoc.customerAge.roles
      ) == "the ages of <a customer>"
    )
  }

  // TODO is this normal ???
  test("getter plural quantitative plural") {
    assert(
      verbalizeSentence(
        vc.copy(article = QuantitativeArticle, plural = true),
        CustomerVoc.getAge,
        CustomerVoc.customerAge.roles
      ) ==
        "the ages of <a customer>"
    )
  }

  test("setter default") {
    assert(
      verbalizeSentence(
        vc,
        CustomerVoc.setAge,
        CustomerVoc.customerAge.roles
      ) == "set the age of <a customer> to age"
    )
  }

  // TODO no effect ?
  test("setter definite") {
    assert(
      verbalizeSentence(
        vc.copy(article = DefiniteArticle),
        CustomerVoc.setAge,
        CustomerVoc.customerAge.roles
      ) == "set the age of <a customer> to age"
    )
  }

  test("setter plural") {
    assert(
      verbalizeSentence(
        vc.copy(plural = true),
        CustomerVoc.setAge,
        CustomerVoc.customerAge.roles
      ) == "set the age of <a customer> to ages"
    )
  }

}
