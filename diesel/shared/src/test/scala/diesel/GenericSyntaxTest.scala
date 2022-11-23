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

import diesel.Dsl.{Axiom, Concept, Syntax, SyntaxGenericMulti}
import diesel.Marker.{Descriptor, Kind, Severity}

class GenericSyntaxTest extends DslTestFunSuite {
  case class MyList[T](items: Seq[T])

  type Result = Either[Int, String]

  object MyDsl extends Dsl {

    val word: Concept[Result]   =
      concept("'\\S+'".r, Right("").withLeft[Int]) map ((_, t) => Right(t.text).withLeft[Int])
    val number: Concept[Result] = concept("\\d+".r, Left(0).withRight[String]) map ((_, t) =>
      Left(t.text.toIntOption.getOrElse(0)).withRight[String]
    )

    val sGenericList: SyntaxGenericMulti[Result, MyList[Result]] =
      genericSyntaxMultiple[Result, MyList[Result]]((typeParam: Concept[Result]) => {
        ("[" ~ typeParam.rep(true) ~ "]") map {
          case (_, (_, items, _)) => MyList(items)
        }
      })

    val sSum: Syntax[Result] = syntax(number)(
      "sum" ~ number.multiple[MyList[Result]] map {
        case (_, (_, list)) => Left(list.items.collect { case Left(value) => value }.sum)
      }
    )

    val sConcat: Syntax[Result] = syntax(word)(
      "concat" ~ word.multiple[MyList[Result]] map {
        case (_, (_, list)) =>
          Right(list.items.collect { case Right(value) => value }.mkString("|"))
      }
    )

    val sResult: Syntax[Result] = syntax(
      (number | word) map {
        case (_, r) => r match {
            case Left(value)  => value
            case Right(value) => value
          }
      }
    )

    val a: Axiom[Result] = axiom(sResult)
  }

  type Ast = Result

  override def dsl: MyDsl.type = MyDsl

  test("parse list of integers") {
    assertAst("sum [ 1 2 3 ]") {
      Left(6)
    }
  }

  test("parse list of words") {
    assertAst("concat [ 'hello' 'world' ]") {
      Right("'hello'|'world'")
    }
  }

  test("do not parse sum of string list") {
    assertMarkers("sum [ 'foo' 'bar' ]") {
      Seq(
        Marker(
          descriptor = Descriptor(
            kind = Kind.Syntactic,
            severity = Severity.Error
          ),
          offset = 6,
          length = 5,
          message = InsertedTokenMsg(token =
            "'foo'")
        ),
        Marker(
          descriptor = Descriptor(
            kind = Kind.Syntactic,
            severity = Severity.Error
          ),
          offset = 12,
          length = 5,
          message = InsertedTokenMsg(token =
            "'bar'")
        )
      )
    }
  }

  test("do not parse concat of int list") {
    assertMarkers("concat [ 1 2 ]") {
      Seq(
        Marker(
          descriptor = Descriptor(
            kind = Kind.Syntactic,
            severity = Severity.Error
          ),
          offset = 9,
          length = 1,
          message = InsertedTokenMsg(token =
            "1")
        ),
        Marker(
          descriptor = Descriptor(
            kind = Kind.Syntactic,
            severity = Severity.Error
          ),
          offset = 11,
          length = 1,
          message = InsertedTokenMsg(token =
            "2")
        )
      )
    }
  }

  test("do not parse mixed list") {
    assertMarkers("concat [ 1 'bar' ]") {
      Seq(Marker(
        descriptor = Descriptor(
          kind = Kind.Syntactic,
          severity = Severity.Error
        ),
        offset = 9,
        length = 1,
        message = InsertedTokenMsg(token =
          "1")
      ))
    }
    assertMarkers("sum [ 1 'bar' ]") {
      Seq(Marker(
        descriptor = Descriptor(
          kind = Kind.Syntactic,
          severity = Severity.Error
        ),
        offset = 8,
        length = 5,
        message = InsertedTokenMsg(token =
          "'bar'")
      ))
    }
  }
}
