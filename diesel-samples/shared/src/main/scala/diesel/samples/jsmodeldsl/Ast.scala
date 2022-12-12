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

package diesel.samples.jsmodeldsl

sealed trait TypeRef
case object StringType                                extends TypeRef
case object BoolType                                  extends TypeRef
case object NumType                                   extends TypeRef
case class CustomType(offsets: Offsets, name: String) extends TypeRef
case class ArrayRefType(compoundType: TypeRef)        extends TypeRef

case class Offsets(offset: Int, length: Int)

case class Root(rootType: TypeRef)
case class AttrDecl(
  offsets: Offsets,
  name: String,
  declaredType: TypeRef,
  optional: Boolean = false
)

sealed trait TypeDefinition {
  val name: String
}

sealed trait Discriminator
case class DFieldName(offsets: Offsets, name: String) extends Discriminator
case class DFieldValue(offsets: Offsets, v: String)   extends Discriminator

case class ClassDeclaration(
  offsets: Offsets,
  name: String,
  attributes: Seq[AttrDecl] = Seq.empty,
  superClass: Option[CustomType] = None,
  discriminator: Option[Discriminator] = None
) extends TypeDefinition
case class DomainDeclaration(offsets: Offsets, name: String, values: Seq[String] = Seq.empty)
    extends TypeDefinition

case class JsModelDecl(root: Root, types: Seq[TypeDefinition] = Seq()) {

  val classes: Seq[ClassDeclaration] = types.foldLeft(Seq.empty[ClassDeclaration])((acc, typeDef) =>
    typeDef match {
      case c @ ClassDeclaration(_, _, _, _, _) =>
        acc :+ c
      case _                                   =>
        acc
    }
  )

  val domains: Seq[DomainDeclaration] = types.foldLeft(Seq.empty[DomainDeclaration])(
    (acc, typeDef) =>
      typeDef match {
        case d @ DomainDeclaration(_, _, _) =>
          acc :+ d
        case _                              =>
          acc
      }
  )

  def findClass(name: String): Option[ClassDeclaration] = classes.find(_.name == name)

  def findDomain(name: String): Option[DomainDeclaration] = domains.find(_.name == name)

  def format: String =
    s"""root : ${formatTypeRef(root.rootType)}""" +
      (if types.isEmpty then "" else "\n\n") +
      types.zipWithIndex.map(x =>
        formatTypeDefinition(x._1) + (if x._2 < types.length - 1 then "\n\n" else "")
      ).mkString

  private def formatDiscriminator(discriminator: Discriminator): String =
    " discriminator " +
      (discriminator match {
        case DFieldName(_, name) =>
          s"field $name"
        case DFieldValue(_, v)   =>
          s"""value "$v""""
      })

  private def formatTypeDefinition(typeDef: TypeDefinition): String = typeDef match {
    case ClassDeclaration(_, name, attributes, superClass, discriminator) =>
      s"class ${name}" + superClass.map(ct => s" extends ${ct.name}").getOrElse(
        ""
      ) + discriminator.map(
        formatDiscriminator
      ).getOrElse("") + " {\n" +
        attributes.map(a =>
          s"  ${a.name}${if a.optional then "?" else ""} : ${formatTypeRef(a.declaredType)}"
        ).mkString(
          "\n"
        ) +
        "\n}"
    case DomainDeclaration(_, name, values)                               =>
      s"domain ${name} [\n" +
        values.zipWithIndex.map {
          case (v, i) =>
            s"""  "${v}"${if i < values.size - 1 then ",\n" else ""}"""
        }.mkString +
        "\n]"
  }

  private def formatTypeRef(typeRef: TypeRef): String = typeRef match {
    case StringType                 =>
      "string"
    case BoolType                   =>
      "boolean"
    case NumType                    =>
      "number"
    case CustomType(offsets, name)  =>
      name
    case ArrayRefType(compoundType) =>
      formatTypeRef(compoundType) + "[]"
  }
}
