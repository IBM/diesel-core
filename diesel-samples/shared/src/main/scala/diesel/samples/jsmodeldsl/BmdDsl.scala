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
import diesel.Lexer.Scanner
import diesel._
import diesel.samples.jsmodeldsl.BmdAst.{findSelfConceptNameDeclaration, findSymbolRefNode}

case object BmdDsl extends Dsl with Identifiers with Comments { //with DynamicLexer {

  // TODO avoid identSpec
  private val identSpec     = "[a-zA-Z][a-zA-Z0-9]*"
  def identScanner: Scanner = "[a-zA-Z][a-zA-Z0-9]*".r

  def commentScanners: Seq[Scanner] = Seq(
    "--[^\n]*(\n|$)".r
  )

  val sSymbol: Syntax[BmdAst.Symbol] = syntax(
    idOrKeyword.rep(false) map {
      case (_, ids) =>
        BmdAst.Symbol(ids)
    }
  )

  val cTypeRef: Concept[TypeRef]     = concept
  val iTextRef: Instance[TypeRef]    = instance(cTypeRef)("text") map { c =>
    c.setStyle(JSMBuiltInType)
    StringType
  }
  val iNumericRef: Instance[TypeRef] = instance(cTypeRef)("numeric") map { c =>
    c.setStyle(JSMBuiltInType)
    NumType
  }

  val sSymbolRef: Syntax[BmdAst.Symbol] = syntax(
    sSymbol
  )

  val sConceptRef: Syntax[TypeRef] = syntax(cTypeRef)(
    "a" ~ sSymbolRef map {
      case (_, (_, symbol)) => CustomType(Offsets(symbol.offset, symbol.length), symbol.text)
    }
  )

  val sStartWithDeclaration: Syntax[Root] = syntax(
    "start" ~ "with" ~ cTypeRef ~ "." map {
      case (ctx, (start, _, typeRef, _)) =>
        ctx.setTokenStyle(start, JSMKeyword)
        Root(typeRef)
    }
  )

  val cArity: Concept[BmdAst.Arity]          = concept
  val iSingleArity: Instance[BmdAst.Arity]   = instance(cArity)("a") map { _ =>
    BmdAst.Single
  }
  val IMultipleArity: Instance[BmdAst.Arity] = instance(cArity)("some") map { _ =>
    BmdAst.Multiple
  }

  val sFieldDeclaration: Syntax[AttrDecl] = syntax(
    cArity ~ sSymbol ~ "(" ~ cTypeRef ~ ")" ~ ("[" ~ "optional" ~ "]").? map {
      case (ctx, (arity, symbol, _, declaredType, _, optional)) =>
        val fieldType = arity match {
          case BmdAst.Single   => declaredType
          case BmdAst.Multiple => ArrayRefType(declaredType)
        }
        AttrDecl(Offsets(symbol.offset, symbol.length), symbol.text, fieldType, optional.isDefined)
    }
  )

  val cDecl: Concept[BmdAst.Decl] = concept

  val sHasDeclaration: Syntax[BmdAst.Decl] = syntax(cDecl)(
    "a" ~ sSymbol ~ "has" ~ sFieldDeclaration ~ "." map {
      case (ctx, (_, symbol, has, field, _)) =>
        ctx.setTokenStyle(has, JSMKeyword)
        BmdAst.FieldDecl(ClassDeclaration(
          Offsets(symbol.offset, symbol.length),
          symbol.text,
          Seq(field)
        ))
    }
  )

  val sCanBeDeclaration: Syntax[BmdAst.Decl] = syntax(cDecl)(
    "a" ~ sSymbol ~ "can" ~ "be" ~ sSymbol ~ "." map {
      case (ctx, (_, symbol, can, _, pred, _)) =>
        ctx.setTokenStyle(can, JSMKeyword)
        val predField = AttrDecl(Offsets(pred.offset, pred.length), pred.text, BoolType)
        BmdAst.FieldDecl(ClassDeclaration(
          Offsets(symbol.offset, symbol.length),
          symbol.text,
          Seq(predField)
        ))
    }
  )

  val sConceptNameDeclaration: Syntax[BmdAst.Symbol] = syntax(
    "a" ~ sSymbol ~ "is" ~ "a" map {
      case (_, (_, symbol, _, _)) => symbol
    }
  )

  val sExtendsDeclaration: Syntax[CustomType] = syntax(
    sSymbolRef map {
      case (ctx, isA) =>
        if (isA.text == "concept") {
          ctx.abort()
        }
        CustomType(Offsets(isA.offset, isA.length), isA.text)
    }
  )

  val sConceptDeclaration: Syntax[BmdAst.Decl] = syntax(cDecl)(
    sConceptNameDeclaration ~ (sExtendsDeclaration | "concept") ~ "." map {
      case (ctx, (symbol, isA, _)) =>
        isA.fold(
          isASymbol => {
            BmdAst.ConceptDecl(
              ClassDeclaration(
                Offsets(symbol.offset, symbol.length),
                symbol.text,
                Nil,
                Option(isASymbol)
              )
            )
          },
          isAConcept => {
            ctx.setTokenStyle(isAConcept, JSMKeyword)
            BmdAst.ConceptDecl(ClassDeclaration(
              Offsets(symbol.offset, symbol.length),
              symbol.text,
              Nil,
              Option.empty
            ))
          }
        )
    }
  )

  val sDomainValues: Syntax[Seq[BmdAst.Symbol]] = syntax(
    sSymbol ~ ("," ~ sSymbol).rep(true) map {
      case (_, (s, rest)) =>
        Seq(s) ++ rest.map(_._2)
    }
  )

  val sCanBeOneOf: Syntax[Boolean] = syntax(
    "can" ~ "be" ~ "one" ~ "of" map { case (_, _) => true }
  )

  val sDomainDeclaration: Syntax[BmdAst.Decl] = syntax(cDecl)(
    "a" ~ sSymbol ~ sCanBeOneOf ~ ":" ~ sDomainValues ~ "." map {
      case (ctx, (_, symbol, _, _, values, _)) =>
        BmdAst.DomainDecl(DomainDeclaration(
          Offsets(symbol.offset, symbol.length),
          symbol.text,
          values.map(_.text)
        ))
    }
  )

  val sCompileUnit: Syntax[BmdAst.BmdModelDecl] = syntax(
    sStartWithDeclaration ~ cDecl.rep(true) map {
      case (_, (startWith, concepts)) =>
        BmdAst.BmdModelDecl(startWith, concepts)
    }
  )

  val aDecl: Axiom[BmdAst.Decl] = axiom(cDecl) map {
    case (_, d) =>
      d
  }

  val aCompileUnit: Axiom[JsModelDecl] = axiom(sCompileUnit) map {
    case (ctx, bmdModelDecl) =>
      val merged = BmdAst.toJsModelDecl(bmdModelDecl)
      Validator
        .validate(merged)
        .foreach(ctx.addMarkers(_))
      merged
  }

  def completionConfiguration: CompletionConfiguration = {
    val config = new CompletionConfiguration()
    config.setFilter(new BmdModelDslCompletionFilter())
    config.setProvider(DslSyntax(sSymbolRef), new BmdModelDslConceptRefProvider())
    config
  }

  private class BmdModelDslCompletionFilter extends CompletionFilter {
    override def filterProposals(
      tree: GenericTree,
      offset: Int,
      node: Option[GenericNode],
      proposals: Seq[CompletionProposal]
    ): Seq[CompletionProposal] = {
      proposals.filter { p =>
        p.element match {
          case Some(Bnf.DslSyntax(syntax)) =>
            syntax.name != BmdDsl.sSymbolRef.name
          case None                        =>
            // TODO howto avoid?
            p.text != identSpec
          case _                           =>
            true
        }
      }
    }
  }

  class BmdModelDslConceptRefProvider extends CompletionProvider {
    override def getProposals(
      element: Option[Bnf.DslElement],
      tree: GenericTree,
      offset: Int,
      node: Option[GenericNode]
    ): Seq[CompletionProposal] = tree.value match {
      case model: JsModelDecl =>
        val selfConceptName = findSelfConceptNameDeclaration(tree)(offset).map(_.text)
        val isSelf          = (name: String) => selfConceptName.contains(name)
        val thisRef         = findSymbolRefNode(tree)(offset)
        val replaceThis     = thisRef.map(node => (node.offset, node.length))
        model
          .classes
          .map(_.name)
          .filterNot(isSelf)
          .map(CompletionProposal(None, _, replaceThis))
      case _                  =>
        Seq.empty
    }
  }

}
