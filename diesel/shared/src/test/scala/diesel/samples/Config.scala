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

package diesel.samples

import diesel.{Context, Dsl, Lexer}
import diesel.Dsl.{Axiom, Concept, DynamicLexer, Syntax, Whitespaces}
import diesel.Lexer.RegexScanner
import diesel.samples.Config.Ast._

object Config {

  object Ast {
    sealed trait Node

    case class Name(label: String) extends Node

    case class Value(content: String) extends Node

    case class Item(name: Name, value: Option[Value]) extends Node

    case class Section(name: Name, items: Seq[Item]) extends Node

    case class Config(sections: Seq[Section]) extends Node
  }

  object Config extends Dsl with DynamicLexer with Whitespaces {

    val whitespacesScanner: Lexer.Scanner = RegexScanner("[ ]+".r)

    val name: Concept[Name] = concept[Name]("[a-z]+".r, Name(""))
      .valueToString((v: Name) => v.label) map { (_: Context, s) =>
      Name(s.text)
    }

    val value: Concept[Value] = concept[Value]("[^\n]+".r, Value(""))
      .valueToString((v: Value) => v.content) map { (_: Context, s) =>
      Value(s.text)
    }

    val item: Syntax[Item] = syntax(
      name ~ "=" ~ value.? ~ "\n" map {
        case (_, (name, _, value: Option[Value], _)) =>
          Item(name, value)
      }
    )

    val section: Syntax[Section] = syntax(
      "[" ~ name ~ "]" ~ "\n" ~ item.rep(false) map {
        case (_, (_, name, _, _, items: Seq[Item])) =>
          Section(name, items)
      }
    )

    val config: Syntax[Ast.Config] = syntax(
      "\n".? ~ section.rep(false) map {
        case (_, (_, sections: Seq[Section])) =>
          Ast.Config(sections)
      }
    )

    val start: Axiom[Config] = axiom(config)
  }
}
