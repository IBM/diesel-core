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

package diesel.facade

import diesel._

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation._

trait MarkerPostProcessor {
  def postProcessMarkers(tree: GenericTree): Seq[Marker]
}

case class ParseContext(
  markerPostProcessor: Option[MarkerPostProcessor] = None,
  navigatorFactory: Result => Navigator = Navigator(_)
)

case class PredictContext(
  markerPostProcessor: Option[MarkerPostProcessor] = None,
  navigatorFactory: Result => Navigator = Navigator(_),
  config: Option[CompletionConfiguration] = None,
  userDataProvider: Option[UserDataProvider] = None
)

class DieselParserFacade(
  val dsl: Dsl,
  val parseContextFactory: Option[ParseRequest => ParseContext] = None,
  val predictContextFactory: Option[PredictRequest => PredictContext] = None
) {

  val bnf: Bnf       = Bnf(dsl)
  val parser: Earley = Earley(bnf, dsl.dynamicLexer)

  private def doParse(request: ParseRequest): Result = {
    val a = getBnfAxiomOrThrow(request.axiom.toOption)
    parser.parse(new Lexer.Input(request.text), a)
  }

  @JSExport
  def parse(request: ParseRequest): DieselParseResult = {
    val parseContext = parseContextFactory.map(_(request)).getOrElse(ParseContext())
    DieselParseResult(
      doParse(request),
      parseContext.markerPostProcessor,
      parseContext.navigatorFactory
    )
  }

  @JSExport
  def predict(request: PredictRequest): DieselPredictResult = {
    val predictContext = predictContextFactory.map(_(request)).getOrElse(PredictContext())
    DieselPredictResult(
      doParse(request),
      request.text,
      request.offset,
      predictContext.config,
      predictContext.navigatorFactory,
      predictContext.userDataProvider
    )
  }

  // TODO borrowed from AstHelper
  private def getBnfAxiomOrThrow(axiom: Option[String]): Bnf.Axiom = {
    axiom match {
      case Some(x) =>
        bnf.axioms
          .find(ba => ba.name == s"${x}[_,_,_,_,_].axiom")
          .getOrElse(throw new IllegalArgumentException(s"missing axiom '$x'"))
      case None    =>
        bnf.axioms
          .headOption
          .getOrElse(throw new IllegalArgumentException("no axiom"))
    }
  }
}

@js.native
trait ParseRequest extends js.Object {
  var text: String                                        = js.native
  var axiom: js.UndefOr[String]                           = js.native
  var customParameters: js.UndefOr[js.Dictionary[String]] = js.native
}

@js.native
trait PredictRequest extends ParseRequest {
  val offset: Int = js.native
}

class DieselMarker(private val marker: Marker) {

  @JSExport
  val offset: Int = marker.offset

  @JSExport
  val length: Int = marker.length

  @JSExport
  def getMessage(locale: js.UndefOr[String]): String =
    marker.message.format(locale.getOrElse("en"))

  @JSExport
  val severity: String = marker.descriptor.severity match {
    case diesel.Marker.Severity.Info    => "info"
    case diesel.Marker.Severity.Warning => "warning"
    case diesel.Marker.Severity.Error   => "error"
  }

}

case class DieselStyle(private val styledRange: StyledRange) {

  @JSExport
  val offset: Int = styledRange.offset

  @JSExport
  val length: Int = styledRange.length

  @JSExport
  val name: String = styledRange.style.name
}

class DieselParseResult(
  private val res: Either[String, GenericTree],
  private val markerPostProcessor: Option[MarkerPostProcessor]
) {

  @JSExport
  val success: Boolean = res.isRight

  @JSExport
  val error: js.UndefOr[String] = res.left.toOption.orUndefined

  @JSExport
  val markers: js.Array[DieselMarker] = res.toOption
    .map(tree => markerPostProcessor.map(mp => mp.postProcessMarkers(tree)).getOrElse(tree.markers))
    .getOrElse(Seq.empty)
    .map(m => new DieselMarker(m))
    .toJSArray

  @JSExport
  val styles: js.Array[DieselStyle] = res.toOption
    .map(new Styles(_).styledRanges)
    .getOrElse(Seq.empty)
    .map(DieselStyle)
    .toJSArray
}

object DieselParseResult {

  private def errorResult(reason: String): DieselParseResult =
    new DieselParseResult(Left(reason), None)

  def apply(
    result: Result,
    markerPostProcessor: Option[MarkerPostProcessor],
    navigatorFactory: Result => Navigator = Navigator(_)
  ): DieselParseResult = {
    if (result.success) {
      val r = navigatorFactory(result).expectOneTree()
        .swap
        .map(x => x._1)
        .swap
      new DieselParseResult(r, markerPostProcessor)
    } else {
      errorResult("parsing failure :/ ")
    }
  }
}

class Replace(private val r: (Int, Int)) {

  @JSExport
  val offset: Int = r._1

  @JSExport
  val length: Int = r._2

}

class DieselCompletionProposal(private val proposal: CompletionProposal) {

  @JSExport
  val text: String = proposal.text

  @JSExport
  val replace: js.UndefOr[Replace] = proposal.replace
    .map(new Replace(_))
    .orUndefined

  @JSExport
  val documentation: js.UndefOr[String] = proposal.documentation.orUndefined

  override def toString = s"DieselCompletionProposal($text, $replace)"
}

case class DieselPredictResult(private val res: Either[String, Seq[CompletionProposal]]) {

  @JSExport
  val success: Boolean = res.isRight

  @JSExport
  val error: js.UndefOr[String] = res.left.toOption.orUndefined

  @JSExport
  val proposals: js.Array[DieselCompletionProposal] = res
    .toOption
    .getOrElse(Seq.empty)
    .map(new DieselCompletionProposal(_))
    .toJSArray

}

object DieselPredictResult {

  private def errorResult(reason: String): DieselPredictResult = DieselPredictResult(Left(reason))

  def apply(
    result: Result,
    text: String,
    offset: Int,
    config: Option[CompletionConfiguration],
    navigatorFactory: Result => Navigator,
    userDataProvider: Option[UserDataProvider]
  ): DieselPredictResult = {
    if (result.success) {
      val navigator = navigatorFactory(result)
      if (navigator.hasNext) {
        val proposals = new CompletionProcessor(
          result,
          text,
          config,
          userDataProvider
        ).computeCompletionProposal(offset).distinctBy(
          _.text
        ) // not sure why but we have to dedup this
        new DieselPredictResult(Right(proposals))
      } else {
        errorResult("No AST found ??")
      }
    } else {
      errorResult("parsing failure :/")
    }
  }
}
