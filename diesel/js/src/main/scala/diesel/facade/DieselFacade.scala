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

@JSExportTopLevel("diesel")
object DieselParsers {

  @JSExport
  def createParseRequest(text: String) = ParseRequest(text)

  @JSExport
  def createPredictRequest(text: String, offset: Int) =
    PredictionRequest(ParseRequest(text), offset)

}

trait MarkerPostProcessor {
  def postProcessMarkers(tree: GenericTree): Seq[Marker]
}

class DieselParserFacade(
  val dsl: Dsl,
  val config: Option[CompletionConfiguration] = None,
  val userDataProvider: Option[UserDataProvider] = None,
  val markerPostProcessor: Option[MarkerPostProcessor] = None
) {

  val bnf: Bnf       = Bnf(dsl, None)
  val parser: Earley = Earley(bnf, dsl.dynamicLexer)

  private def doParse(request: ParseRequest): Result = {
    val a = getBnfAxiomOrThrow(request.axiom.toOption)
    parser.parse(new Lexer.Input(request.text), a)
  }

  @JSExport
  def parse(request: ParseRequest): DieselParseResult =
    DieselParseResult(doParse(request), markerPostProcessor)

  @JSExport
  def predict(request: PredictionRequest): DieselPredictResult = {
    DieselPredictResult(doParse(request.parseRequest), request.offset, config, userDataProvider)
  }

  // TODO borrowed from AstHelper
  private def getBnfAxiomOrThrow(axiom: Option[String]): Bnf.Axiom = {
    axiom match {
      case Some(x) =>
        bnf.axioms
          .find(ba => ba.name == s"${x}[_,_,_,_,_].axiom")
          .getOrElse(throw new IllegalArgumentException(s"missing axiom '${x}'"))
      case None    =>
        bnf.axioms
          .headOption
          .getOrElse(throw new IllegalArgumentException("no axiom"))
    }
  }

}

@JSExportAll
case class ParseRequest(text: String, axiom: js.UndefOr[String] = js.undefined) {

  def setAxiom(axiom: js.UndefOr[String]): ParseRequest = {
    this.copy(axiom = axiom)
  }

}

@JSExportAll
case class PredictionRequest(parseRequest: ParseRequest, offset: Int)

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

  def apply(result: Result, markerPostProcessor: Option[MarkerPostProcessor]): DieselParseResult = {
    if (result.success) {
      val navigator = Navigator(result)
      if (navigator.hasNext) {
        val ast = navigator.next()
        if (navigator.hasNext) {
          // TODO how to debug ?
          errorResult("too many ASTs")
        } else {
          new DieselParseResult(Right(ast), markerPostProcessor)
        }
      } else {
        errorResult("No AST found ??")
      }
    } else {
      errorResult("parsing failure :/")
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
    offset: Int,
    config: Option[CompletionConfiguration],
    userDataProvider: Option[UserDataProvider]
  ): DieselPredictResult = {
    if (result.success) {
      val navigator = Navigator(result)
      if (navigator.hasNext) {
        val proposals = new CompletionProcessor(
          result,
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
