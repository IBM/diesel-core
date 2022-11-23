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

sealed trait Expr {}

case class ERef(name: String) extends Expr

sealed trait EValue extends Expr {
  protected def unsupported(opName: String): EValue = {
    throw new UnsupportedOperationException(s"operation ${opName} unsupported on ${this}")
  }

  def dash(): EValue = unsupported("dash")

  def add(e: EValue): EValue = unsupported("add")

  def sub(e: EValue): EValue = unsupported("sub")

  def div(e: EValue): EValue = unsupported("div")

  def mul(e: EValue): EValue = unsupported("mul")

  def mod(e: EValue): EValue = unsupported("mod")

  def dotSelect(s: String): EValue = unsupported("dotSelect")
}

case class ENum(v: Double) extends EValue {
  override def add(e: EValue): EValue = {
    e match {
      case ENum(v2) =>
        ENum(v + v2)
      case EBool(v) =>
        ???
      case EVoid    =>
        ???
      case vec: Vec =>
        vec.add(this)
    }
  }

  override def sub(e: EValue): EValue = {
    e match {
      case ENum(v2) =>
        ENum(v - v2)
      case EBool(v) =>
        ???
      case EVoid    =>
        ???
      case vec: Vec =>
        vec.sub(this)
    }
  }

  override def mul(e: EValue): EValue = {
    e match {
      case ENum(v2) =>
        ENum(v * v2)
      case EBool(v) =>
        ???
      case EVoid    =>
        ???
      case vec: Vec =>
        vec.mul(this)
    }
  }

  override def div(e: EValue): EValue = {
    e match {
      case ENum(v2) =>
        ENum(v / v2)
      case EBool(v) =>
        ???
      case EVoid    =>
        ???
      case vec: Vec =>
        vec.div(this)
    }
  }

  override def dash(): EValue = {
    ENum(if (v == 0) 0 else -v)
  }
}

case class EBool(v: Boolean) extends EValue {}

case object EVoid extends EValue {}

object Vec {
  val MAX_VEC_SIZE = 4

  def mkValues(nbComponents: Int, values: Array[Double]): Array[Double] = {
    val buf: mutable.ArrayBuffer[Double] = mutable.ArrayBuffer()
    for (i <- 0 until nbComponents) {
      buf.append(
        values(
          if (values.length < nbComponents) {
            0
          } else {
            i
          }
        )
      )
    }
    buf.toArray
  }

  def fromValues(nbComponents: Int, values: Array[Double]): EValue = {
    val vals = mkValues(nbComponents, values)
    nbComponents match {
      case 1 =>
        ENum(vals(0))
      case 2 =>
        EVec2(vals(0), vals(1))
      case 3 =>
        EVec3(vals(0), vals(1), vals(2))
      case 4 =>
        EVec4(vals(0), vals(1), vals(2), vals(3))
    }
  }

  def fromExpressions(scope: Scope, nbComponents: Int, exprs: Array[Expr]): EValue = {
    val values: Array[Double] = exprs
      .map {
        case ERef(name)    =>
          scope.find(name).get
        case value: EValue =>
          value
      }
      .flatMap {
        case ENum(v)  =>
          Array(v)
        case EBool(v) =>
          ???
        case EVoid    =>
          ???
        case vec: Vec =>
          vec.values
      }
    fromValues(nbComponents, values)
  }
}

trait Vec extends EValue {

  val nbComponents: Int
  def values: Array[Double]
  def a: Double = values(0)
  def b: Double = values(1)
  def c: Double = values(2)
  def d: Double = values(3)

  def getComponents(s: String): List[Double] = {
    s.map {
      case 'x' =>
        a
      case 'y' =>
        b
      case 'z' =>
        c
      case 'w' =>
        d
      case _   =>
        ???
    }
  }.toList

  override def dotSelect(s: String): EValue = {
    getComponents(s) match {
      case a :: Nil                =>
        ENum(a)
      case a :: b :: Nil           =>
        EVec2(a, b)
      case a :: b :: c :: Nil      =>
        EVec3(a, b, c)
      case a :: b :: c :: d :: Nil =>
        EVec4(a, b, c, d)
      case _                       =>
        throw new UnsupportedOperationException()
    }
  }

  override def div(e: EValue): EValue = {
    binOp(e, (x, y) => x / y)
  }

  override def add(e: EValue): EValue = {
    binOp(e, (x, y) => x + y)
  }

  override def mul(e: EValue): EValue = {
    binOp(e, (x, y) => x * y)
  }

  private def binOp(e: EValue, f: (Double, Double) => Double): EValue = {
    e match {
      case ENum(v)  =>
        Vec.fromValues(nbComponents, values.map(thisV => f(thisV, v)))
      case EBool(_) =>
        ???
      case EVoid    =>
        throw new IllegalArgumentException(s"can't binOp void to ${this}!")
      case vec: Vec =>
        val thisValues  = values
        val otherValues = vec.values
        val newVals     = mutable.ArrayBuffer[Double]()
        for (i <- thisValues.indices) {
          if (i < otherValues.length) {
            newVals.append(f(values(i), otherValues(i)))
          } else {
            newVals.append(values(i))
          }
        }
        Vec.fromValues(nbComponents, newVals.toArray)
    }
  }

  def mapComponents(f: (Double => Double)): EValue =
    Vec.fromValues(
      nbComponents,
      values.map(f)
    )

}

case class EVec2(v0: Double, v1: Double) extends Vec {
  override val nbComponents: Int = 2

  override def values: Array[Double] = Array(v0, v1)
}

case class EVec3(v0: Double, v1: Double, v2: Double) extends Vec {
  override val nbComponents: Int = 3

  override def values: Array[Double] = Array(v0, v1, v2)
}

case class EVec4(v0: Double, v1: Double, v2: Double, v3: Double) extends Vec {
  override val nbComponents: Int = 4

  override def values: Array[Double] = Array(v0, v1, v2, v3)
}

object Expr {
  def num(v: Double): EValue = ENum(v)

  def bool(v: Boolean): EValue = EBool(v)

  val voidExpr: EValue = EVoid

  def ref(name: String): ERef = ERef(name)

  def vec2(a: Double, b: Double): EValue = EVec2(a, b)

  def vec3(a: Double, b: Double, c: Double): EValue = EVec3(a, b, c)

  def vec4(a: Double, b: Double, c: Double, d: Double): EValue = EVec4(a, b, c, d)

}
