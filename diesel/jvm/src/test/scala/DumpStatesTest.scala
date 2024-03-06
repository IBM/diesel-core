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

import munit.FunSuite
import diesel.samples.calc.MyDsl
import diesel.AstHelpers
import diesel.DumpStates

class DumpStatesTest extends FunSuite {

  test("dump and open".ignore) {
    val text   = "1 + 2"
    val result = AstHelpers.parse(MyDsl, text)
    DumpStates.dumpAndOpen(text, result)
  }

}
