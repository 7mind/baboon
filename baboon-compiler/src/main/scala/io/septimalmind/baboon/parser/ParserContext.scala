package io.septimalmind.baboon.parser

import io.septimalmind.baboon.parser.defns.{DefAdt, DefContract, DefDto, DefEnum, DefForeign, DefMeta, DefModel, DefService}
import io.septimalmind.baboon.parser.model.*

case class ParserContext(file: FSPath, content: String) {
  // Side-channel set by `DefDocs.prefixDocs` when it detects two prefix
  // doc blocks back-to-back with no intervening declaration. Set just
  // before the doc rule fails the parse; consumed by the parser driver
  // (`BaboonParser.BaboonParserImpl.parse`) which translates a parse
  // failure with this set into a `ParserIssue.StackedDocComments(pos)`.
  //
  // Per Q3 lock (`docs/spec/docstrings.md` §4) stacked prefix docs are a
  // parse error citing the second block's position. We carry this through
  // a side-channel because FastParse failures are message-typed
  // (Parsed.Failure) and cannot directly carry a typed `ParserIssue`.
  @volatile var stackedDocAt: Option[InputPointer] = None

  val defMeta     = new DefMeta(this)
  val defDocs     = new io.septimalmind.baboon.parser.defns.base.DefDocs(this)
  val defEnum     = new DefEnum(this, defMeta)
  val defDto      = new DefDto(this, defMeta, defDocs)
  val defContract = new DefContract(this, defMeta, defDto)
  val defAdt      = new DefAdt(this, defMeta, defDto, defContract)
  val defForeign  = new DefForeign(this, defMeta, defDto)

  val defService = new DefService(this, defMeta, defDto, defAdt, defEnum)

  val defModel =
    new DefModel(this, defMeta, defEnum, defDto, defAdt, defForeign, defContract, defService)

}
