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

trait Style {
  val name: String
}

case class StyledRange(offset: Int, length: Int, style: Style)

class Styles(val tree: GenericTree) {

  val styledRanges: Seq[StyledRange] = {
    tree.toIterator.toSeq
      .map(_.context)
      .flatMap(c => {
        c.getStyle
          .map(StyledRange(c.offset, c.length, _))
          .toSeq ++
          c.getTokenStyles.map(tokenStyle =>
            StyledRange(tokenStyle._1.offset, tokenStyle._1.text.length, tokenStyle._2)
          )
      })
  }
}

object BuiltinStyles {
  sealed trait BuiltinStyle                        extends Style
  case object NumberStyle                          extends BuiltinStyle { val name = "number"   }
  case object StringStyle                          extends BuiltinStyle { val name = "string"   }
  case object VariableStyle                        extends BuiltinStyle { val name = "variable" }
  case object CommentStyle                         extends BuiltinStyle { val name = "comment"  }
  case object ConstantStyle                        extends BuiltinStyle { val name = "constant" }
  case object KeywordStyle                         extends BuiltinStyle { val name = "keyword"  }
  abstract class WrappedStyle(style: BuiltinStyle) extends Style        {
    val name = style.name
  }
}
