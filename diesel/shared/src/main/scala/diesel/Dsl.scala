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

import diesel.Lexer.{RegexScanner, Scanner, Token}
import diesel.voc.Article

import diesel.i18n.DeclaringSourceName

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.matching.Regex

/** Type definitions of the Dsl API
  */
object Dsl {

  /*
    Concepts
   */

  sealed trait LexicalRepresentation

  case class ScannerRepr(scanner: Scanner) extends LexicalRepresentation

  sealed case class ConceptData[T](
    lexicalRepresentation: LexicalRepresentation,
    defaultValue: T,
    stringOf: (T) => String,
    valueOf: (Context, Token) => T,
    style: Option[Style]
  ) {
    def defaultValueAsString: String = stringOf(defaultValue)
  }

  trait ConceptBase {
    def name: String
  }

  case class Concept[T: ClassTag](name: String, data: Option[ConceptData[T]])(implicit
    tag: ClassTag[T]
  ) extends ConceptBase {
    def typeOf: ClassTag[T] = tag
  }

  case class Inheritance(parent: ConceptBase, child: ConceptBase)

  /*
    Instances
   */

  trait InstanceBase {
    def name: String
    def concept: ConceptBase
  }

  case class Instance[T](
    name: String,
    concept: Concept[T],
    production: InstanceProduction,
    value: Context => T,
    style: Option[Style]
  ) extends InstanceBase

  sealed trait InstanceProduction

  case class IPStr(s: String) extends InstanceProduction {
    def ~(o: InstanceProduction): IPAnd2 = IPAnd2(this, o)
  }

  case class IPAnd2(i1: InstanceProduction, i2: InstanceProduction) extends InstanceProduction {
    def ~(o: InstanceProduction): IPAnd3 = IPAnd3(i1, i2, o)
  }

  case class IPAnd3(i1: InstanceProduction, i2: InstanceProduction, i3: InstanceProduction)
      extends InstanceProduction {
//    def ~(o: InstanceProduction): IPAnd3 = IPAnd3(i1, i2, o)
  }

  object Associativity extends Enumeration {
    val Left, None, Right = Value
  }

  /*
    Phrase
   */

  trait PhraseBase {
    val name: String
    def concept: ConceptBase
    def multiple: Boolean
  }

  trait Applicable[+T] {
    def applyDynamic(context: Context, args: Any): T
  }

  /*
    Syntax
   */

  trait SyntaxBase {
    def name: String
  }

  trait SyntaxTypedBase extends SyntaxBase {
    def concept: ConceptBase
    def multiple: Boolean
  }

  sealed trait Syntax[T] extends SyntaxBase {
    val production: SyntaxProduction[T]
  }

  case class SyntaxUntyped[T](name: String, production: SyntaxProduction[_ <: T]) extends Syntax[T]

  case class SyntaxTyped[T](
    name: String,
    concept: Concept[T],
    expression: Boolean,
    production: SyntaxProduction[_ <: T]
  ) extends Syntax[T]
      with SyntaxTypedBase {
    override def multiple: Boolean = false
  }

  case class SyntaxMulti[T, T2](
    name: String,
    concept: Concept[T],
    production: SyntaxProduction[_ <: T2]
  ) extends Syntax[T2]
      with SyntaxTypedBase {
    override def multiple: Boolean = true
  }

  trait SyntaxGenericBase[T] {
    val name: String
  }

  case class SyntaxGeneric[T: ClassTag](
    override val name: String,
    accept: (Concept[T], Expressions.Types, Dsl) => Boolean,
    syntaxOf: Concept[T] => SyntaxTyped[T]
  )(implicit tag: ClassTag[T])
      extends SyntaxGenericBase[T] {
    def apply(
      concept: Concept[_],
      exprTypes: Expressions.Types,
      dsl: Dsl,
      consumer: SyntaxTyped[T] => Unit
    ): Unit = {
      if (concept.typeOf == tag) {
        val c = concept.asInstanceOf[Concept[T]]
        if (accept(c, exprTypes, dsl))
          consumer(syntaxOf(c))
      }
    }
  }

  case class SyntaxGenericMulti[T: ClassTag, T2](
    override val name: String,
    accept: (Concept[T], Expressions.Types, Dsl) => Boolean,
    syntaxOf: Concept[T] => SyntaxMulti[T, T2]
  )(implicit tag: ClassTag[T])
      extends SyntaxGenericBase[T] {
    def apply(
      concept: Concept[_],
      exprTypes: Expressions.Types,
      dsl: Dsl,
      consumer: (SyntaxMulti[T, T2]) => Unit
    ): Unit =
      if (concept.typeOf == tag) {
        val c = concept.asInstanceOf[Concept[T]]
        if (accept(c, exprTypes, dsl))
          consumer(syntaxOf(c))
      }
  }

  sealed trait SyntaxProduction[+T] extends Applicable[T] {
    def map[T2](f: (Context, T) => T2): SyntaxProduction[T2] = SPMapped[T, T2](this, f)
  }

  trait SPAppendable[T] { this: SyntaxProduction[T] =>
    def ~[T2](o: SyntaxProduction[T2]): SPAnd2[T, T2] = SPAnd2(this, o)
    def ~[T2](s: String): SPAnd2[T, Token]            = SPAnd2(this, SPStr(s))
  }

  trait SPAssociable[T] { this: SyntaxProduction[T] =>
    def leftAssoc(order: Int = 0): SPAssoc[T]  = SPAssoc(this, Associativity.Left, order)
    def noneAssoc(order: Int = 0): SPAssoc[T]  = SPAssoc(this, Associativity.None, order)
    def rightAssoc(order: Int = 0): SPAssoc[T] = SPAssoc(this, Associativity.Right, order)
  }

  trait SPOptionalizable[T] { this: SyntaxProduction[T] =>
    def `?` : SPOpt[T] = SPOpt(this)
  }

  trait SPRepeatable[T] { this: SyntaxProduction[T] =>
    def rep(zeroIncluded: Boolean): SPRep[T] = SPRep(this, zeroIncluded)
  }

  trait SPOrable[T] { this: SyntaxProduction[T] =>
    def |[T2](o: SyntaxProduction[T2]) = SPOr(this, o)
  }

  //  trait SPHasCardinality[T] { this: SyntaxProduction[T] =>
//    def multiple[T2](): SPMultiple[T2] = SPMultiple(this)
//  }

  case class SPOpt[T](e: SyntaxProduction[T])
      extends SyntaxProduction[Option[T]]
      with SPAppendable[Option[T]]
      with SPAssociable[Option[T]]
      with SPRepeatable[Option[T]] {

    override def applyDynamic(context: Context, args: Any): Option[T] =
      args.asInstanceOf[Option[T]]
  }

  case class SPOr[T1, T2](e1: SyntaxProduction[T1], e2: SyntaxProduction[T2])
      extends SyntaxProduction[Either[T1, T2]]
      with SPAppendable[Either[T1, T2]]
      with SPAssociable[Either[T1, T2]]
      with SPOptionalizable[Either[T1, T2]]
      with SPRepeatable[Either[T1, T2]]
      with SPOrable[Either[T1, T2]] {

    override def applyDynamic(context: Context, args: Any): Either[T1, T2] = {
      args match {
        case Left(v)  =>
          Left(e1.applyDynamic(context, v))
        case Right(v) =>
          Right(e2.applyDynamic(context, v))
      }
    }

  }

  case class SPRep[T](e: SyntaxProduction[T], zeroIncluded: Boolean)
      extends SyntaxProduction[Seq[T]]
      with SPAppendable[Seq[T]]
      with SPAssociable[Seq[T]]
      with SPOptionalizable[Seq[T]] {

    override def applyDynamic(context: Context, args: Any): Seq[T] = {
      val s = args.asInstanceOf[Seq[T]]
      s.map { a => e.applyDynamic(context, a) }
    }
  }

  case class SPAssoc[T](e: SyntaxProduction[T], associativity: Associativity.Value, order: Int = 0)
      extends SyntaxProduction[T]
      with SPAppendable[T]
      with SPOptionalizable[T]
      with SPRepeatable[T] {
    override def applyDynamic(context: Context, args: Any): T = e.applyDynamic(context, args)
  }

  case class SPMapped[T, T2](e: SyntaxProduction[T], f: (Context, T) => T2)
      extends SyntaxProduction[T2]
      with SPAppendable[T2]
      with SPAssociable[T2]
      with SPOptionalizable[T2]
      with SPRepeatable[T2] {

    override def applyDynamic(context: Context, args: Any): T2 = {
      // do not go deep, to fit parsers' polnish evaluation pattern
      val argsAsT = args.asInstanceOf[T]
      f(context, argsAsT)
    }
  }

  case class SPSubject(str: SPStr)
      extends SyntaxProduction[Seq[Token]]
      with SPAppendable[Seq[Token]]
      with SPAssociable[Seq[Token]] {
    override def applyDynamic(context: Context, args: Any): Seq[Token] =
      args match {
        case t: Token   =>
          Seq(t)
        case p: Product =>
          p.productIterator
            .map(_.asInstanceOf[Token])
            .toSeq
      }
  }

  case class SPStr(text: String, style: Option[Style] = None)
      extends SyntaxProduction[Token]
      with SPAppendable[Token]
      with SPAssociable[Token]
      with SPOptionalizable[Token]
      with SPRepeatable[Token]
      with SPOrable[Token] {
    override def applyDynamic(context: Context, args: Any): Token = { args.asInstanceOf[Token] }

    def tokenStyle(style: Style): SPStr = SPStr(text, Some(style))

    def subject: SPSubject = SPSubject(this)

    override def toString: String = s"syntaxStr($text)"
  }

  case class SPRegex(r: Regex, style: Option[Style] = None)
      extends SyntaxProduction[Token]
      with SPAppendable[Token]
      with SPAssociable[Token]
      with SPOptionalizable[Token]
      with SPRepeatable[Token]
      with SPOrable[Token] {

    override def applyDynamic(context: Context, args: Any): Token = {
      args.asInstanceOf[Token]
    }

    def tokenStyle(style: Style): SPRegex = SPRegex(r, Some(style))

    override def toString: String = s"syntaxRegex($r)"
  }

  case class SPIdentifier(s: String, withKeywords: Boolean = false)
      extends SyntaxProduction[Token]
      with SPAppendable[Token]
      with SPAssociable[Token]
      with SPOptionalizable[Token]
      with SPRepeatable[Token]
      with SPOrable[Token] {
    override def applyDynamic(context: Context, args: Any): Token = { args.asInstanceOf[Token] }

    override def toString: String = "sI"
  }

  object Expressions {
    sealed trait Type {
      def &(other: Type): Types = TypeSet(Set(this) ++ Set(other))
    }

    case object Values    extends Type
    case object Instances extends Type
    case object Phrases   extends Type
    case object Syntaxes  extends Type

    case class Custom(name: String) extends Type

    trait Types {
      def types: Set[Type]
      def has(exprType: Type): Boolean = types.contains(exprType)

      override def toString: String = types.mkString("-")
    }

    case class TypeSet(types: Set[Type]) extends Types {
      def &(other: Type): Types = TypeSet(types ++ Seq(other))
    }
  }

  case class SPExprRef[T](c: Concept[T], exprTypes: Expressions.Types)
      extends SyntaxProduction[T]
      with SPAppendable[T]
      with SPOptionalizable[T]
      with SPOrable[T]
      with SPRepeatable[T] {

    override def applyDynamic(context: Context, args: Any): T = args.asInstanceOf[T]

    override def toString: String = s"syntaxExprRef(${c.name})"

    def multiple[T2]: SPMultiple[T, T2] = SPMultiple(this)

    def article(article: Article): SPRefWithContext[T] =
      SPRefWithContext(this, SPVerbalizationContext(Some(article), None, None))

    def exprTypes(exprType: Expressions.Type): SPExprRef[T] =
      SPExprRef[T](c, Expressions.TypeSet(Set(exprType)))

    def exprTypes(exprTypes: Expressions.Types): SPExprRef[T] = SPExprRef[T](c, exprTypes)

    def verbalization(vc: SPVerbalizationContext): SPRefWithContext[T] = SPRefWithContext(this, vc)
  }

  case class SPVerbalizationContext(
    article: Option[Article] = None,
    plural: Option[Boolean] = None,
    partitive: Option[Boolean] = None
  ) {
    def article(article: Article): SPVerbalizationContext =
      SPVerbalizationContext(Some(article), plural, partitive)

    def plural(plural: Boolean): SPVerbalizationContext =
      SPVerbalizationContext(article, Some(plural), partitive)

    def partitive(partitive: Boolean): SPVerbalizationContext =
      SPVerbalizationContext(article, plural, Some(partitive))
  }

  case class SPMultiple[T, T2](
    ref: SPExprRef[T],
    verbalizationContext: Option[SPVerbalizationContext] = None
  ) extends SyntaxProduction[T2] {
    override def applyDynamic(context: Context, args: Any): T2 = args.asInstanceOf[T2]

    def verbalization(vc: SPVerbalizationContext): SPMultiple[T, T2] =
      this.copy(verbalizationContext =
        Some(vc)
      )
  }

  case class SPRefWithContext[T](ref: SPExprRef[T], verbalizationContext: SPVerbalizationContext)
      extends SyntaxProduction[T]
      with SPAppendable[T]
      with SPOptionalizable[T]
      with SPRepeatable[T]
      with SPOrable[T] {
    override def applyDynamic(context: Context, args: Any): T = ref.applyDynamic(context, args)
  }

  case class SPConceptRef[T, T2](
    concept: Concept[T],
    map: (Context, Concept[T]) => T2,
    multiple: Boolean = false,
    verbalizationContext: SPVerbalizationContext = SPVerbalizationContext(None, None, None)
  ) extends SyntaxProduction[T2]
      with SPAppendable[T2]
      with SPOptionalizable[T2]
      with SPOrable[T2]
      with SPRepeatable[T2] {

    override def applyDynamic(context: Context, args: Any): T2 =
      map(context, args.asInstanceOf[Concept[T]])

    override def toString: String = s"syntaxConceptRef(${concept.name})"

    def nary: SPConceptRef[T, T2] =
      SPConceptRef(concept, map, multiple = true, verbalizationContext)

    def article(article: Article): SPConceptRef[T, T2] =
      SPConceptRef(concept, map, multiple, verbalizationContext.article(article))

    def plural(plural: Boolean): SPConceptRef[T, T2] =
      SPConceptRef(concept, map, multiple, verbalizationContext.plural(plural))

    def verbalization(vc: SPVerbalizationContext): SPConceptRef[T, T2] =
      SPConceptRef(concept, map, multiple, vc)
  }

  case class SPRuleRef[T](r: Syntax[T])
      extends SyntaxProduction[T]
      with SPAppendable[T]
      with SPOptionalizable[T]
      with SPRepeatable[T]
      with SPOrable[T] {

    override def applyDynamic(context: Context, args: Any): T = args.asInstanceOf[T]

    override def toString: String = s"syntaxRuleRef(${r.name})"
  }

  case class SPLazyRuleRef[T](name: String, r: () => Syntax[T])
      extends SyntaxProduction[T]
      with SPAppendable[T]
      with SPOptionalizable[T]
      with SPOrable[T] {

    override def applyDynamic(context: Context, args: Any): T = args.asInstanceOf[T]

    override def toString: String = s"lazyRuleRef($name)"
  }

  case class SPAnd2[T1, T2](e1: SyntaxProduction[T1], e2: SyntaxProduction[T2])
      extends SyntaxProduction[(T1, T2)]
      with SPAssociable[(T1, T2)]
      with SPOptionalizable[(T1, T2)]
      with SPRepeatable[(T1, T2)]
      with SPOrable[(T1, T2)] {

    def ~[T3](o: SyntaxProduction[T3]): SPAnd3[T1, T2, T3] = SPAnd3(e1, e2, o)

    override def applyDynamic(context: Context, args: Any): (T1, T2) = {
      val (v1, v2) = args.asInstanceOf[(T1, T2)]
      (e1.applyDynamic(context, v1), e2.applyDynamic(context, v2))
    }
  }

  case class SPAnd3[T1, T2, T3](
    e1: SyntaxProduction[T1],
    e2: SyntaxProduction[T2],
    e3: SyntaxProduction[T3]
  ) extends SyntaxProduction[(T1, T2, T3)]
      with SPAssociable[(T1, T2, T3)]
      with SPOptionalizable[(T1, T2, T3)]
      with SPRepeatable[(T1, T2, T3)] {

    def ~[T4](o: SyntaxProduction[T4]): SPAnd4[T1, T2, T3, T4] = SPAnd4(e1, e2, e3, o)

    override def applyDynamic(context: Context, args: Any): (T1, T2, T3) = {
      val (v1, v2, v3) = args.asInstanceOf[(T1, T2, T3)]
      (e1.applyDynamic(context, v1), e2.applyDynamic(context, v2), e3.applyDynamic(context, v3))
    }
  }

  case class SPAnd4[T1, T2, T3, T4](
    e1: SyntaxProduction[T1],
    e2: SyntaxProduction[T2],
    e3: SyntaxProduction[T3],
    e4: SyntaxProduction[T4]
  ) extends SyntaxProduction[(T1, T2, T3, T4)]
      with SPAssociable[(T1, T2, T3, T4)]
      with SPOptionalizable[(T1, T2, T3, T4)]
      with SPRepeatable[(T1, T2, T3, T4)] {

    def ~[T5](o: SyntaxProduction[T5]): SPAnd5[T1, T2, T3, T4, T5] = SPAnd5(e1, e2, e3, e4, o)

    override def applyDynamic(context: Context, args: Any): (T1, T2, T3, T4) = {
      val (v1, v2, v3, v4) = args.asInstanceOf[(T1, T2, T3, T4)]
      (
        e1.applyDynamic(context, v1),
        e2.applyDynamic(context, v2),
        e3.applyDynamic(context, v3),
        e4.applyDynamic(context, v4)
      )
    }
  }

  case class SPAnd5[T1, T2, T3, T4, T5](
    e1: SyntaxProduction[T1],
    e2: SyntaxProduction[T2],
    e3: SyntaxProduction[T3],
    e4: SyntaxProduction[T4],
    e5: SyntaxProduction[T5]
  ) extends SyntaxProduction[(T1, T2, T3, T4, T5)]
      with SPAssociable[(T1, T2, T3, T4, T5)]
      with SPOptionalizable[(T1, T2, T3, T4, T5)]
      with SPRepeatable[(T1, T2, T3, T4, T5)] {

    def ~[T6](o: SyntaxProduction[T6]): SPAnd6[T1, T2, T3, T4, T5, T6] =
      SPAnd6(e1, e2, e3, e4, e5, o)

    override def applyDynamic(context: Context, args: Any): (T1, T2, T3, T4, T5) = {
      val (v1, v2, v3, v4, v5) = args.asInstanceOf[(T1, T2, T3, T4, T5)]
      (
        e1.applyDynamic(context, v1),
        e2.applyDynamic(context, v2),
        e3.applyDynamic(context, v3),
        e4.applyDynamic(context, v4),
        e5.applyDynamic(context, v5)
      )
    }
  }

  case class SPAnd6[T1, T2, T3, T4, T5, T6](
    e1: SyntaxProduction[T1],
    e2: SyntaxProduction[T2],
    e3: SyntaxProduction[T3],
    e4: SyntaxProduction[T4],
    e5: SyntaxProduction[T5],
    e6: SyntaxProduction[T6]
  ) extends SyntaxProduction[(T1, T2, T3, T4, T5, T6)]
      with SPAssociable[(T1, T2, T3, T4, T5, T6)]
      with SPOptionalizable[(T1, T2, T3, T4, T5, T6)]
      with SPRepeatable[(T1, T2, T3, T4, T5, T6)] {

    def ~[T7](o: SyntaxProduction[T7]): SPAnd7[T1, T2, T3, T4, T5, T6, T7] =
      SPAnd7(e1, e2, e3, e4, e5, e6, o)

    override def applyDynamic(context: Context, args: Any): (T1, T2, T3, T4, T5, T6) = {
      val (v1, v2, v3, v4, v5, v6) = args.asInstanceOf[(T1, T2, T3, T4, T5, T6)]
      (
        e1.applyDynamic(context, v1),
        e2.applyDynamic(context, v2),
        e3.applyDynamic(context, v3),
        e4.applyDynamic(context, v4),
        e5.applyDynamic(context, v5),
        e6.applyDynamic(context, v6)
      )
    }
  }

  case class SPAnd7[T1, T2, T3, T4, T5, T6, T7](
    e1: SyntaxProduction[T1],
    e2: SyntaxProduction[T2],
    e3: SyntaxProduction[T3],
    e4: SyntaxProduction[T4],
    e5: SyntaxProduction[T5],
    e6: SyntaxProduction[T6],
    e7: SyntaxProduction[T7]
  ) extends SyntaxProduction[(T1, T2, T3, T4, T5, T6, T7)]
      with SPAssociable[(T1, T2, T3, T4, T5, T6, T7)]
      with SPOptionalizable[(T1, T2, T3, T4, T5, T6, T7)]
      with SPRepeatable[(T1, T2, T3, T4, T5, T6, T7)] {

    def ~[T8](o: SyntaxProduction[T8]): SPAnd8[T1, T2, T3, T4, T5, T6, T7, T8] =
      SPAnd8(e1, e2, e3, e4, e5, e6, e7, o)

    override def applyDynamic(context: Context, args: Any): (T1, T2, T3, T4, T5, T6, T7) = {
      val (v1, v2, v3, v4, v5, v6, v7) = args.asInstanceOf[(T1, T2, T3, T4, T5, T6, T7)]
      (
        e1.applyDynamic(context, v1),
        e2.applyDynamic(context, v2),
        e3.applyDynamic(context, v3),
        e4.applyDynamic(context, v4),
        e5.applyDynamic(context, v5),
        e6.applyDynamic(context, v6),
        e7.applyDynamic(context, v7)
      )
    }
  }

  case class SPAnd8[T1, T2, T3, T4, T5, T6, T7, T8](
    e1: SyntaxProduction[T1],
    e2: SyntaxProduction[T2],
    e3: SyntaxProduction[T3],
    e4: SyntaxProduction[T4],
    e5: SyntaxProduction[T5],
    e6: SyntaxProduction[T6],
    e7: SyntaxProduction[T7],
    e8: SyntaxProduction[T8]
  ) extends SyntaxProduction[(T1, T2, T3, T4, T5, T6, T7, T8)]
      with SPAssociable[(T1, T2, T3, T4, T5, T6, T7, T8)]
      with SPOptionalizable[(T1, T2, T3, T4, T5, T6, T7, T8)]
      with SPRepeatable[(T1, T2, T3, T4, T5, T6, T7, T8)] {

    override def applyDynamic(context: Context, args: Any): (T1, T2, T3, T4, T5, T6, T7, T8) = {
      val (v1, v2, v3, v4, v5, v6, v7, v8) = args.asInstanceOf[(T1, T2, T3, T4, T5, T6, T7, T8)]
      (
        e1.applyDynamic(context, v1),
        e2.applyDynamic(context, v2),
        e3.applyDynamic(context, v3),
        e4.applyDynamic(context, v4),
        e5.applyDynamic(context, v5),
        e6.applyDynamic(context, v6),
        e7.applyDynamic(context, v7),
        e8.applyDynamic(context, v8)
      )
    }
  }

  case class SPAndN[T](es: Seq[SyntaxProduction[T]])
      extends SyntaxProduction[Seq[T]]
      with SPAssociable[Seq[T]]
      with SPOptionalizable[Seq[T]]
      with SPRepeatable[Seq[T]]
      with SPOrable[Seq[T]] {

    override def applyDynamic(context: Context, args: Any): Seq[T] = {
      // the sequence comes in as a looong tuple
      val vs = args.asInstanceOf[Product].productIterator.toSeq
      es.zip(vs).map { case (e, v) => e.applyDynamic(context, v) }
    }
  }

  /*
    Axiom
   */

  trait AxiomBase {
    def name: String
  }

  case class Axiom[+T](name: String, production: SyntaxProduction[_ <: T]) extends AxiomBase

  /*
    Identifiers
   */

  trait Identifiers {

    def identScanner: Scanner

    def keywordScanner: Scanner = identScanner

    val id: SPIdentifier = SPIdentifier(identScanner.name)

    val idOrKeyword: SPIdentifier = SPIdentifier(identScanner.name, withKeywords = true)
  }

  /*
    Comments
   */

  trait Comments {

    def commentScanners: Seq[Scanner]
  }

  /*
    Comments
   */

  trait Whitespaces {

    def whitespacesScanner: Scanner
  }

  trait DynamicLexer {}
}

/** Dsl trait : to be mixed in by language definitions. Holds all declared concepts, phrases,
  * instances etc., and provides user-friendly APIs for declaring grammars and mapping to custom
  * ASTs.
  */
trait Dsl {

  import Dsl._

  def defaultExprs: Set[Expressions.Type] = Set(
    Expressions.Values,
    Expressions.Instances,
    Expressions.Phrases,
    Expressions.Syntaxes
  )

  private[diesel] def internalDefaultExprs: Expressions.Types = new Expressions.Types {
    override def types: Set[Expressions.Type] = defaultExprs

    override def toString: String = "default"
  }

  private var concepts: Seq[ConceptBase]                 = Seq()
  private var instances: Seq[InstanceBase]               = Seq()
  private var syntaxes: Seq[Syntax[_]]                   = Seq()
  private var genericSyntaxes: Seq[SyntaxGenericBase[_]] = Seq()
  private var axioms: Seq[AxiomBase]                     = Seq()
  private var inheritances: Seq[Inheritance]             = Seq()

  def getConcepts: Seq[ConceptBase]                        = concepts
  def getInstances: Seq[InstanceBase]                      = instances
  def getSyntaxes: Seq[Syntax[_]]                          = syntaxes
  def getGenericSyntaxes: Seq[SyntaxGenericBase[_]]        = genericSyntaxes
  def getAxioms: Seq[AxiomBase]                            = axioms
  def getConcept(name: String): Option[ConceptBase]        =
    concepts.find(c => name == c.name)
  def getParent(c: ConceptBase): Option[ConceptBase]       =
    inheritances.find(i => i.child == c).map(_.parent)
  def subConceptsOf(c: ConceptBase): Seq[ConceptBase]      = inheritances collect {
    case i if i.parent == c => i.child
  }
  def isSubtypeOf(c: ConceptBase, p: ConceptBase): Boolean = c == p || hasAncestor(c, p)
  def hasAncestor(c: ConceptBase, p: ConceptBase): Boolean =
    getParent(c).contains(p) || subConceptsOf(p).exists(d => hasAncestor(c, d))

  def concept[T: ClassTag](implicit name: DeclaringSourceName): Concept[T] = {
    val result = Concept[T](name = name.name, None)
    concepts :+= result
    result
  }

  def concept[T: ClassTag, P >: T](parent: Concept[P])(implicit
    name: DeclaringSourceName
  ): Concept[T] = {
    val result = Concept[T](name = name.name, None)
    concepts :+= result
    inheritances :+= Inheritance(parent, result)
    result
  }

  def concept[T: ClassTag](scanner: Scanner, defaultValue: T)(implicit
    name: DeclaringSourceName
  ): ConceptBuilder[T, T] =
    new ConceptBuilder(name.name, scanner, defaultValue, (value: T) => value.toString, None)

  def concept[T: ClassTag, P >: T](
    scanner: Scanner,
    defaultValue: T,
    parent: Option[Concept[P]]
  )(implicit name: DeclaringSourceName): ConceptBuilder[T, P] =
    new ConceptBuilder[T, P](
      name.name,
      scanner,
      defaultValue,
      (value: T) => value.toString,
      parent
    )

  class ConceptBuilder[T: ClassTag, P >: T](
    val name: String,
    val scanner: Scanner,
    val defaultValue: T,
    var stringOf: T => String,
    val parent: Option[Concept[P]],
    var style: Option[Style] = None
  ) {

    def tokenStyle(s: Style): ConceptBuilder[T, P] = {
      style = Some(s)
      this
    }

    def valueToString(stringOf: T => String): ConceptBuilder[T, P] = {
      this.stringOf = stringOf
      this
    }

    def map(valueOf: (Context, Token) => T): Concept[T] = {
      val c = Concept[T](
        name = name,
        Some(
          ConceptData(
            lexicalRepresentation = ScannerRepr(scanner),
            defaultValue = defaultValue,
            stringOf = stringOf,
            valueOf = valueOf,
            style = style
          )
        )
      )
      concepts ++= Seq(c)

      // add to inheritance relations if needed
      parent.foreach(p => inheritances ++= Seq(Inheritance(p, c)))

      c
    }
  }

  def instance[T](concept: Concept[T])(s: String)(implicit
    name: DeclaringSourceName
  ): InstanceBuilder[T] =
    new InstanceBuilder(name.name, concept, IPStr(s))

  class InstanceBuilder[T](
    val name: String,
    val concept: Concept[T],
    val production: InstanceProduction,
    var style: Option[Style] = None
  ) {

    def tokenStyle(s: Style): InstanceBuilder[T] = {
      style = Some(s)
      this
    }

    def map(value: Context => T): Instance[T] = {
      val i = Instance(name, concept, production, value, style)
      instances ++= Seq(i)
      i
    }
  }

  private def addSyntax[T](syntax: Syntax[T]): Syntax[T] = {
    syntaxes ++= Seq(syntax)
    syntax
  }

  def syntax[T](production: SyntaxProduction[T])(implicit
    name: DeclaringSourceName
  ): SyntaxUntyped[T] =
    addSyntax(SyntaxUntyped(name.name, production)).asInstanceOf[SyntaxUntyped[T]]

  def syntax[T](
    concept: Concept[T],
    expression: Boolean = true
  )(production: SyntaxProduction[T])(implicit name: DeclaringSourceName): SyntaxTyped[T] =
    addSyntax(SyntaxTyped(name.name, concept, expression, production)).asInstanceOf[SyntaxTyped[T]]

  def syntaxMultiple[T, T2](concept: Concept[T])(production: SyntaxProduction[T2])(implicit
    name: DeclaringSourceName
  ): SyntaxMulti[T, T2] = {
    addSyntax(SyntaxMulti(name.name, concept, production)).asInstanceOf[SyntaxMulti[T, T2]]
  }

  private def addGenericSyntax[T](syntax: SyntaxGenericBase[T]): SyntaxGenericBase[T] = {
    genericSyntaxes ++= Seq(syntax)
    syntax
  }

  def genericSyntax[T: ClassTag](base: Concept[T], toProduction: Concept[T] => SyntaxProduction[T])(
    implicit name: DeclaringSourceName
  ): SyntaxGeneric[T] = {
    genericSyntax[T](
      toProduction,
      (concept: Concept[T], _: Expressions.Types, dsl: Dsl) => dsl.isSubtypeOf(concept, base)
    )
  }

  def genericSyntax[T: ClassTag](
    toProduction: Concept[T] => SyntaxProduction[T],
    accept: (Concept[T], Expressions.Types, Dsl) => Boolean =
      (_: Concept[T], _: Expressions.Types, _: Dsl) => true
  )(
    implicit name: DeclaringSourceName
  ): SyntaxGeneric[T] = {
    var cache: Map[Concept[T], SyntaxTyped[T]] = Map()
    addGenericSyntax(SyntaxGeneric[T](
      name.name,
      accept,
      concept => {
        cache.getOrElse(
          concept, {
            val rule = SyntaxTyped(name.name, concept, expression = true, toProduction(concept))
            cache += concept -> rule
            rule
          }
        )
      }
    )).asInstanceOf[SyntaxGeneric[T]]
  }

  def genericSyntaxMultiple[T: ClassTag, T2](
    base: Concept[T],
    toProduction: Concept[T] => SyntaxProduction[T2]
  )(
    implicit name: DeclaringSourceName
  ): SyntaxGenericMulti[T, T2] = {
    genericSyntaxMultiple[T, T2](
      toProduction,
      (concept: Concept[T], _: Expressions.Types, dsl: Dsl) => dsl.isSubtypeOf(concept, base)
    )
  }

  def genericSyntaxMultiple[T: ClassTag, T2](
    toProduction: Concept[T] => SyntaxProduction[T2],
    accept: (Concept[T], Expressions.Types, Dsl) => Boolean =
      (_: Concept[T], _: Expressions.Types, _: Dsl) => true
  )(
    implicit name: DeclaringSourceName
  ): SyntaxGenericMulti[T, T2] = {
    var cache: Map[Concept[T], SyntaxMulti[T, T2]] = Map()
    addGenericSyntax(SyntaxGenericMulti[T, T2](
      name.name,
      accept,
      concept => {
        cache.getOrElse(
          concept, {
            val rule = SyntaxMulti(name.name, concept, toProduction(concept))
            cache += concept -> rule
            rule
          }
        )
      }
    )).asInstanceOf[SyntaxGenericMulti[T, T2]]
  }

  class AxiomBuilder[T](val name: String, val production: SyntaxProduction[T]) {
    def build: Axiom[T] = {
      val a = Axiom(name, production)
      axioms ++= Seq(a)
      a
    }

    def map[T2](f: (Context, T) => T2): AxiomBuilder[T2] =
      new AxiomBuilder(name, SPMapped(production, f))
  }

  def axiom[T](concept: Concept[T])(implicit name: DeclaringSourceName): AxiomBuilder[T] =
    new AxiomBuilder(name.name, SPExprRef(concept, internalDefaultExprs))

  def axiom[T](syntax: Syntax[T])(implicit name: DeclaringSourceName): AxiomBuilder[T] =
    new AxiomBuilder(name.name, SPRuleRef(syntax))

  implicit def builderToAxiom[T](builder: AxiomBuilder[T]): Axiom[T] = builder.build

  def iS(s: String): IPStr = IPStr(s)

  // enable implicit conversions for Syntaxes only

  implicit def syntaxStr(s: String): SPStr = SPStr(s)

  implicit def syntaxExprRef[T](c: Concept[T]): SPExprRef[T] = SPExprRef(c, internalDefaultExprs)

  implicit def syntaxRuleRef[T](s: Syntax[T]): SPRuleRef[T] = SPRuleRef(s)

  implicit def syntaxRegex(r: Regex): SPRegex = SPRegex(r)

  implicit def regexToScanner(r: Regex): Scanner = RegexScanner(r)

  def syntaxRef[T](name: String): SPLazyRuleRef[T] = SPLazyRuleRef(
    name,
    () =>
      syntaxes.find(_.name == name)
        .filter(_.isInstanceOf[Syntax[_]])
        .map(_.asInstanceOf[Syntax[T]])
        .get
  )

  def dynamicLexer: Boolean = this match {
    case _: DynamicLexer => true
    case _               => false
  }
}
