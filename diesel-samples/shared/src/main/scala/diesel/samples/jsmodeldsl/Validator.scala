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

import diesel.{Marker}
import diesel.Marker.{Descriptor, Kind, Severity}

import scala.annotation.tailrec

trait Validator {
  def validate(model: JsModelDecl): Seq[Marker]
}

object Validator {

  import diesel.MarkerMessage.Implicits.strToMsg

  def validate(model: JsModelDecl): Seq[Marker] = defaultValidators.flatMap(v => v.validate(model))

  private val defaultValidators: Seq[Validator] = Seq(
    ClassDeclaredMoreThanOnce,
    UndeclaredType,
    AttributeDeclaredMoreThanOnce,
    CyclicInheritance,
    DiscriminatorUsedInAttributes,
    DiscriminatorValueWithoutExtends
  )

  private val semanticError = Descriptor(Kind.Semantic, Severity.Error)

  object ClassDeclaredMoreThanOnce extends Validator {
    override def validate(model: JsModelDecl): Seq[Marker] =
      model.classes.groupBy(_.name).filter(_._2.length > 1).values.flatten.map(cd =>
        Marker(
          semanticError,
          cd.offsets.offset,
          cd.offsets.length,
          s"Class ${cd.name} declared more than once"
        )
      ).toSeq
  }

  object UndeclaredType extends Validator {
    override def validate(model: JsModelDecl): Seq[Marker] = {

      def checkDeclared(offsets: Offsets, name: String): Option[Marker] =
        if (model.findClass(name).isDefined || model.findDomain(name).isDefined)
          None
        else {
          Some(Marker(
            semanticError,
            offsets.offset,
            offsets.length,
            s"Undeclared type $name"
          ))
        }

      @tailrec
      def checkTypeRef(typeRef: TypeRef): Option[Marker] = typeRef match {
        case CustomType(offsets, name)  =>
          checkDeclared(offsets, name)
        case ArrayRefType(compoundType) =>
          checkTypeRef(compoundType)
        case _                          =>
          None
      }

      def checkDeclaredClass(declaration: ClassDeclaration): Seq[Marker] =
        declaration.attributes.flatMap(ad => checkTypeRef(ad.declaredType))

      Seq(
        checkTypeRef(model.root.rootType),
        model.classes.flatMap(checkDeclaredClass)
      ).flatten
    }
  }

  object AttributeDeclaredMoreThanOnce extends Validator {
    override def validate(model: JsModelDecl): Seq[Marker] = {
      model.classes.flatMap(cd =>
        cd.attributes.groupBy(_.name).filter(_._2.length > 1).values.flatten.map(ad =>
          Marker(
            semanticError,
            ad.offsets.offset,
            ad.offsets.length,
            s"Attribute ${ad.name} declared more than once"
          )
        )
      )
    }
  }

  object CyclicInheritance extends Validator {
    override def validate(model: JsModelDecl): Seq[Marker] =
      model.classes.flatMap { cd => checkClass(model, cd, Set.empty) }

    private def checkClass(
      model: JsModelDecl,
      classDeclaration: ClassDeclaration,
      checked: Set[String]
    ): Option[Marker] = {
      classDeclaration.superClass
        .flatMap { superClass =>
          if (superClass.name == classDeclaration.name) {
            Some(Marker(
              semanticError,
              superClass.offsets.offset,
              superClass.offsets.length,
              "A class cannot extend itself"
            ))
          } else {
            if (checked.contains(superClass.name)) {
              Some(Marker(
                semanticError,
                superClass.offsets.offset,
                superClass.offsets.length,
                "Cyclic inheritance"
              ))
            } else {
              model.findClass(superClass.name) match {
                case Some(superClassDecl) =>
                  checkClass(model, superClassDecl, checked + classDeclaration.name)
                case None                 =>
                  Some(Marker(
                    semanticError,
                    superClass.offsets.offset,
                    superClass.offsets.length,
                    s"Undeclared type ${superClass.name}"
                  ))
              }
            }
          }
        }
    }
  }

  object DiscriminatorUsedInAttributes extends Validator {
    override def validate(model: JsModelDecl): Seq[Marker] =
      model.classes.flatMap(validateClass)

    private def validateClass(classDeclaration: ClassDeclaration): Seq[Marker] = {
      classDeclaration.discriminator
        .map {
          case DFieldName(offsets, name) =>
            // discriminator name defined, look for it in attributes
            val attrMarkers = classDeclaration.attributes
              .filter(_.name == name)
              .map { attr =>
                Marker(
                  semanticError,
                  attr.offsets.offset,
                  attr.offsets.length,
                  "Attribute is used as discriminator"
                )
              }
            val dMarker     =
              if (attrMarkers.isEmpty)
                Seq.empty
              else {
                Seq(
                  Marker(
                    semanticError,
                    offsets.offset,
                    offsets.length,
                    "Discriminator cannot be used in attributes"
                  )
                )
              }
            attrMarkers ++ dMarker
          case _: DFieldValue            =>
            Seq.empty
        }
        .getOrElse(Seq.empty)
    }
  }

  object DiscriminatorValueWithoutExtends extends Validator {
    override def validate(model: JsModelDecl): Seq[Marker] = {
      model.classes.flatMap { classDeclaration =>
        classDeclaration.discriminator match {
          case Some(DFieldValue(offsets, _)) =>
            if (classDeclaration.superClass.isEmpty) {
              Seq(
                Marker(
                  Descriptor(Kind.Semantic, Severity.Warning),
                  offsets.offset,
                  offsets.length,
                  """Discriminator value used without 'extends'. Please specify the super class."""
                )
              )
            } else {
              Seq.empty
            }
          case _                             =>
            Seq.empty
        }
      }
    }
  }
}
