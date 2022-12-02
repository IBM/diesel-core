package diesel.samples.mybundle

import diesel.facade.DieselParserFacade
import diesel.samples.jsmodeldsl.BmdDsl

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("MyFacade")
object MyFacade {

  @JSExport
  def createMyParser(): DieselParserFacade = new DieselParserFacade(BmdDsl)

}
