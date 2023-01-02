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

import munit.FunSuite

class VocabularyTest extends FunSuite {

  test("is assignable from") {
    val cObject = Concept("object", "java.lang.Object", Seq())
    val cShape  = Concept("shape", "com.foo.Shape", Seq(cObject.identifier))
    val cCircle = Concept("circle", "com.foo.Circle", Seq(cShape.identifier))
    val cRect   = Concept("rect", "com.foo.Rect", Seq(cShape.identifier))
    val cFoo    = Concept("foo", "com.foo.Foo", Seq(cObject.identifier))

    val voc: Vocabulary = Vocabulary(
      Glossary.empty,
      concepts = Seq(cObject, cShape, cCircle, cRect, cFoo),
      conceptInstances = Seq.empty,
      factTypes = Seq.empty
    )

    assert(voc.isAssignableFrom(cObject, cObject))
    assert(voc.isAssignableFrom(cObject, cShape))
    assert(voc.isAssignableFrom(cObject, cRect))
    assert(voc.isAssignableFrom(cObject, cCircle))
    assert(voc.isAssignableFrom(cShape, cShape))
    assert(voc.isAssignableFrom(cShape, cRect))
    assert(voc.isAssignableFrom(cShape, cCircle))
    assert(voc.isAssignableFrom(cObject, cFoo))
    assert(voc.isAssignableFrom(cFoo, cFoo))
    assert(!voc.isAssignableFrom(cFoo, cObject))
    assert(!voc.isAssignableFrom(cShape, cFoo))
    assert(!voc.isAssignableFrom(cRect, cCircle))
  }

}
