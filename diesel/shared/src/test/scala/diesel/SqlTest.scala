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

import diesel.samples.Sql.Ast._
import diesel.samples.Sql.Sql

class SqlTest extends DslTestFunSuite {

  type Ast = Clause
  override def dsl = Sql

  test("select all") {
    assertAst("select * from users") {
      Select(Star, Name("users"))
    }
  }

  test("select") {
    assertAst("select foo from users") {
      Select(Fields(Seq(Name("foo"))), Name("users"))
    }
  }

  test("select listed") {
    assertAst("select foo, bar from users") {
      Select(Fields(Seq(Name("foo"), Name("bar"))), Name("users"))
    }
  }

  test("delete") {
    assertAst("delete from users") {
      Delete(Name("users"))
    }
  }

  test("missing where") {
    withTree("select * from users x") { tree =>
      assert(tree.markers.size == 1)
      val m = tree.markers.head
      assert(m.message == InsertedTokenMsg("x"))
    }
  }

  test("select where") {
    assertAst("select * from users where id is null") {
      Select(Star, Name("users"), Some(Where(FieldTest(Name("id"), IsNull))))
    }
  }

  test("select where not") {
    assertAst("select * from users where id is not null") {
      Select(Star, Name("users"), Some(Where(FieldTest(Name("id"), IsNotNull))))
    }
  }

  // TODO: Bnf must handle inheritance of concepts
  test("where expression") {
    assertAst("select * from users where id = 123") {
      Select(Star, Name("users"), Some(Where(FieldTest(Name("id"), Equals(IntValue(123))))))
    }
  }

}
