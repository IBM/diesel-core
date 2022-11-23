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

package diesel.samples.feel

import diesel.AstHelpers._
import diesel.CompletionConfiguration
import munit.FunSuite

class FeelPredictionTest extends FunSuite {

  private def assertPredictions(
    text: String,
    offset: Int,
    expectedPredictions: Seq[String]
  ): Unit = {
    val config      = new CompletionConfiguration()
    val feel        = new Feel
    val predictions = predict(feel, text, offset, Some(config))
    assert(predictions.map(_.text) == expectedPredictions)
  }

  test("empty text") {
    assertPredictions(
      "",
      0,
      List(
        "-",
        "not (",
        "(",
        "[",
        "function (",
        "{",
        "for",
        "if",
        "-",
        "[a-z]+",
        "some",
        "every",
        "null",
        ">=",
        "<=",
        ">",
        "<",
        "[",
        "true",
        "false",
        "0",
        "\"\"",
        "@"
      )
    )
  }

}
