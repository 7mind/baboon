package io.septimalmind.baboon.parser

import io.septimalmind.baboon.parser.defns.{DefAdt, DefContract, DefDto, DefEnum, DefForeign, DefMeta, DefModel}
import io.septimalmind.baboon.parser.model.*

case class ParserContext(file: FSPath, content: String) {
  val defMeta     = new DefMeta(this)
  val defEnum     = new DefEnum(this, defMeta)
  val defDto      = new DefDto(this, defMeta)
  val defContract = new DefContract(this, defMeta, defDto)
  val defAdt      = new DefAdt(this, defMeta, defDto, defContract)
  val defForeign  = new DefForeign(this, defMeta)
  val defModel =
    new DefModel(this, defMeta, defEnum, defDto, defAdt, defForeign, defContract)

}
