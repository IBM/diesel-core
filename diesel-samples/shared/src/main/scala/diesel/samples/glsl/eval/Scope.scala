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

package diesel.samples.glsl.eval

import scala.collection.mutable

class Scope(
  private val parent: Option[Scope] = None
) {

  private val variables: mutable.Map[String, EValue] = mutable.Map[String, EValue]()
  private val functions: mutable.Map[String, Func]   = mutable.Map[String, Func]()

  def push(): Scope = new Scope(Some(this))

  def pop(): Option[Scope] = parent

  def find(name: String): Option[EValue] = variables.get(name)
    .orElse(
      parent.flatMap(p => p.find(name))
    )

  def findFunc(name: String): Option[Func] = functions.get(name)
    .orElse(
      parent.flatMap(p => p.findFunc(name))
    )

  def declare(name: String, v: EValue): Unit = variables.put(name, v)

  def declare(name: String, f: Func): Unit = functions.put(name, f)

  def getVariables: Map[String, EValue] = variables.toMap

  def getFunctions: Map[String, Func] = functions.toMap

  def setValue(name: String, value: EValue): Unit = {
    if variables.contains(name) then {
      variables(name) = value
    } else {
      if parent.isDefined then {
        parent.get.setValue(name, value)
      } else {
        throw new IllegalStateException(s"trying to set value of undeclared ${name}")
      }
    }
  }

  def toValue(e: Expr): EValue = {
    e match {
      case ERef(name)    =>
        find(name).get
      case value: EValue =>
        value
    }
  }

}

object Scope {

  def initial(): Scope = {
    val s = new Scope()
    for f <- BuiltInFunction.builtInFunctions do {
      s.declare(f._1, f._2)
    }
    s
  }

  private def toValue(scope: Scope, e: Expr): EValue = {
    e match {
      case ERef(name)    =>
        scope.find(name).get
      case value: EValue =>
        value
    }
  }

}
