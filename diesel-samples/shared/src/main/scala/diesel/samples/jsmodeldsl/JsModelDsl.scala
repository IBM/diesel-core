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
import diesel.Dsl._
import diesel.Lexer.{Scanner, Token}
import diesel._

case object JsModelDsl extends Dsl with Identifiers with Comments with DynamicLexer {

  def identScanner: Scanner = "[a-zA-Z][a-zA-Z0-9]*".r

  def commentScanners: Seq[Scanner] = Seq(
    "/\\*([^*]|\\*(\\*)*[^*/])*\\*(\\*)*/".r,
    "//[^\n]*(\n|$)".r
  )

  val cTypeRef: Concept[TypeRef]    = concept
  val iStringRef: Instance[TypeRef] = instance(cTypeRef)("string") map { c =>
    c.setStyle(JSMBuiltInType)
    StringType
  }
  val iBoolRef: Instance[TypeRef]   = instance(cTypeRef)("boolean") map { c =>
    c.setStyle(JSMBuiltInType)
    BoolType
  }
  val iNumRef: Instance[TypeRef]    = instance(cTypeRef)("number") map { c =>
    c.setStyle(JSMBuiltInType)
    NumType
  }
  val sCustomRef: Syntax[TypeRef]   = syntax(cTypeRef)(
    id map { (_, t) => CustomType(Offsets(t.offset, t.length), t.text) }
  )
  val sArrayRef: Syntax[TypeRef]    = syntax(cTypeRef)(
    cTypeRef ~ "[]" map {
      case (_, (typeRef, _)) =>
        ArrayRefType(typeRef)
    }
  )

  val sAttrName: Syntax[Token] = syntax(id)

  val sClassAttribute: Syntax[AttrDecl] = syntax(
    sAttrName ~ "?".? ~ ":" ~ cTypeRef map {
      case (_, (id, opt, _, declaredType)) =>
        AttrDecl(Offsets(id.offset, id.length), id.text, declaredType, opt.isDefined)
    }
  )

  val sClassName: Syntax[Token] = syntax(id)

  val sSuperClassName: Syntax[Token] = syntax(id)

  val sExtends: Syntax[Token] = syntax(
    "extends" ~ sSuperClassName map {
      case (_, (_, className)) =>
        className
    }
  )

  private val dblQuotedString = "\"([^\"\\\\]|\\\\.)*\"".r

  val sDiscriminator: Syntax[Discriminator] = syntax(
    "discriminator" ~ (("field" ~ id) | ("value" ~ dblQuotedString)) map {
      case (_, (_, Left((_, t))))  =>
        DFieldName(Offsets(t.offset, t.length), t.text)
      case (_, (_, Right((_, t)))) =>
        DFieldValue(Offsets(t.offset, t.length), t.text.drop(1).dropRight(1))
    }
  )

  val sClassDeclaration: Syntax[ClassDeclaration] = syntax(
    "class" ~ sClassName ~ sExtends.? ~ sDiscriminator.? ~ "{" ~ sClassAttribute.rep(
      true
    ) ~ "}" map {
      case (ctx, (clazz, id, superClass, discriminator, _, attrs, _)) =>
        ctx.setTokenStyle(clazz, JSMKeyword)
        ClassDeclaration(
          Offsets(id.offset, id.length),
          id.text,
          attrs,
          superClass.map(t => CustomType(Offsets(t.offset, t.length), t.text)),
          discriminator
        )
    }
  )

  val sDomainValue: Syntax[String] = syntax(
    dblQuotedString map {
      case (c, t) =>
        c.setStyle(JSMDomainValue)
        t.text.drop(1).dropRight(1)
    }
  )

  val sDomainValues: Syntax[Seq[String]] = syntax(
    sDomainValue ~ ("," ~ sDomainValue).rep(true) map {
      case (_, (s, rest)) =>
        Seq(s) ++ rest.map(_._2)
    }
  )

  val sDomainName: Syntax[Token] = syntax(id)

  val sDomainDeclaration: Syntax[DomainDeclaration] = syntax(
    "domain" ~ sDomainName ~ "[" ~ sDomainValues ~ "]" map {
      case (ctx, (domain, id, _, values, _)) =>
        ctx.setTokenStyle(domain, JSMKeyword)
        DomainDeclaration(Offsets(id.offset, id.length), id.text, values)
    }
  )

  val sTypeDefinition: Syntax[Either[ClassDeclaration, TypeDefinition]] = syntax(
    sClassDeclaration | sDomainDeclaration
  )

  val sRootDeclaration: Syntax[Root] = syntax(
    "root" ~ ":" ~ cTypeRef map {
      case (ctx, (root, _, typeRef)) =>
        ctx.setTokenStyle(root, JSMKeyword)
        Root(typeRef)
    }
  )

  val sCompileUnit: Syntax[JsModelDecl] = syntax(
    sRootDeclaration ~ sTypeDefinition.rep(true) map {
      case (_, (root, types)) =>
        JsModelDecl(root, types.map(e => e.fold(identity, identity)))
    }
  )

  val a: Axiom[JsModelDecl] = axiom(sCompileUnit) map {
    case (ctx, jsModelDecl) =>
      Validator.validate(jsModelDecl).foreach(ctx.addMarkers(_))
      jsModelDecl
  }

  def completionConfiguration: CompletionConfiguration = {
    val config = new CompletionConfiguration()
    config.setFilter(MyFilter)
    config
  }

  object MyFilter extends CompletionFilter {
    override def filterProposals(
      tree: GenericTree,
      offset: Int,
      node: Option[GenericNode],
      proposals: Seq[CompletionProposal]
    ): Seq[CompletionProposal] = {
      val model = tree.root.value.asInstanceOf[JsModelDecl]
      proposals.flatMap { p =>
        p.element match {
          case Some(DslSyntax(syntax)) =>
            if (syntax.name == sCustomRef.name) {
              // pediction for a custom ref : list what we have !
              model.types.map(td => CompletionProposal(p.element, td.name))
            } else if (syntax.name == sSuperClassName.name) {
              val classDeclNode =
                node.flatMap(_.findFirstParent(_.value.isInstanceOf[ClassDeclaration]))
              classDeclNode
                .map { cdn =>
                  val classDecl = cdn.value.asInstanceOf[ClassDeclaration]
                  model.classes
                    .filter(_.name != classDecl.name)
                    .map(_.name)
                    .sorted
                    .map(CompletionProposal(p.element, _))
                }
                .getOrElse(Seq.empty)
            } else if (
              syntax.name == sAttrName.name || syntax.name == sClassName.name || syntax.name == sDomainName.name
            ) {
              Seq()
            } else {
              Seq(p)
            }
          case _                       =>
            Seq(p)
        }
      }
    }
  }
}
