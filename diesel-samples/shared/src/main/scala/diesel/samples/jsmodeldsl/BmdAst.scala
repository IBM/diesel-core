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

import diesel.Bnf.DslSyntax
import diesel.Dsl.Syntax
import diesel.samples.jsmodeldsl.BmdDsl.{sConceptDeclaration, sSymbolRef}
import diesel.{GenericNode, GenericNonTerminal, GenericTree, Lexer}

import scala.reflect.ClassTag

object BmdAst {
  import diesel.samples.jsmodeldsl.GenericTreeUtil._

  sealed trait Decl {
    val conceptName: String
    val offsets: Offsets
  }
  case class FieldDecl(classDeclaration: ClassDeclaration)    extends Decl {
    val conceptName: String = classDeclaration.name
    val offsets: Offsets    = classDeclaration.offsets
  }
  case class ConceptDecl(classDeclaration: ClassDeclaration)  extends Decl {
    val conceptName: String = classDeclaration.name
    val offsets: Offsets    = classDeclaration.offsets
  }
  case class DomainDecl(domainDeclaration: DomainDeclaration) extends Decl {
    val conceptName: String = domainDeclaration.name
    val offsets: Offsets    = domainDeclaration.offsets
  }

  sealed trait Arity
  case object Single   extends Arity
  case object Multiple extends Arity

  case class Symbol(ids: Seq[Lexer.Token]) {
    val text: String       = ids.map(_.text).mkString(" ")
    val offset: Int        = ids.head.offset
    val length: Int        = ids.last.offset + ids.last.length - ids.head.offset
    val token: Lexer.Token = Lexer.Token(offset, text, ids.head.id)
  }

  case class BmdModelDecl(startWith: Root, decls: Seq[Decl])

  def toJsModelDecl(model: BmdModelDecl): JsModelDecl = {
    val types = model.decls
      .flatMap {
        case concept: ConceptDecl =>
          val attributes = toAttributes(model)(concept)
          Some(
            ClassDeclaration(
              concept.offsets,
              concept.conceptName,
              attributes,
              concept.classDeclaration.superClass
            )
          )
        case DomainDecl(d)        => Some(d)
        case _                    => None
      }
    JsModelDecl(model.startWith, types)
  }

  private def toAttributes(model: BmdModelDecl)(concept: ConceptDecl): Seq[AttrDecl] = {
    model.decls
      .filter(_.isInstanceOf[FieldDecl])
      .map(_.asInstanceOf[FieldDecl])
      .filter(_.conceptName == concept.conceptName)
      .flatMap(_.classDeclaration.attributes)
  }

  def findConceptNameDeclaration(node: GenericNode): Option[BmdAst.Symbol] = {
    node.parent.flatMap(p =>
      GenericTree.asIterable(p, descendants = true)
        .find(isSyntax(BmdDsl.sConceptNameDeclaration))
    )
      .map(_.valueAs[BmdAst.Symbol])
  }

  def isConceptDeclaration(node: GenericNode): Boolean = {
    parents(node)
      .exists(isSyntax(sConceptDeclaration))
  }

  def findSelfConceptNameDeclaration(tree: GenericTree)(offset: Int): Option[BmdAst.Symbol] = {
    tree.root
      .findNodesAtOffset(offset)
      .toSeq
      .sortBy(_.length)
      .headOption
      .filter(isConceptDeclaration)
      .flatMap(findConceptNameDeclaration)
  }

  def findSymbolRefNode(tree: GenericTree)(offset: Int): Option[GenericNode] = {
    tree.root
      .findNodesAtOffset(offset)
      .toSeq
      .sortBy(_.length)
      .headOption
      .filter(isSyntax(sSymbolRef))
  }

}

// TODO move to a shared place
object GenericTreeUtil {

  def optionalAs[T: ClassTag](v: Any): Option[T] = v match {
    case t: T => Option(t)
    case _    => Option.empty
  }

  def isLeftOf(offset: Int): GenericNode => Boolean = _.offset < offset

  // TODO useful?
//  def isRule[T](syntax: Syntax[T]): GenericNode => Boolean = {
//    node =>
//      optionalAs[GenericNonTerminal](node)
//        .flatMap(_.production.rule)
//        .filter(_.isRule)
//        .flatMap(optionalAs[Rule](_))
//        .map(_.productions.map(_.element))
//        .flatMap(optionalAs[Syntax[T]](_))
//        .contains(syntax)
//  }

  def isSyntax[T](syntax: Syntax[T]): GenericNode => Boolean = {
    node =>
      optionalAs[GenericNonTerminal](node)
        .flatMap(_.production.element)
        .flatMap(optionalAs[DslSyntax[T]](_))
        .map(_.syntax)
        .contains(syntax)
  }

  def debugTraceValue[T](title: String): T => T = { v =>
    println(title, v)
    v
  }

  def parents(node: GenericNode): Iterator[GenericNode] = {
    var current = node
    new Iterator[GenericNode] {

      override def hasNext: Boolean = current.parent.isDefined

      override def next(): GenericNode = {
        current = current.parent.get
        current
      }
    }
  }

}
