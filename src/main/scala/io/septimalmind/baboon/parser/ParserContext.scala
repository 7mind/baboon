package io.septimalmind.baboon.parser

import io.septimalmind.baboon.parser.defns.{
  DefAdt,
  DefDto,
  DefEnum,
  DefMeta,
  DefModel
}
import io.septimalmind.baboon.parser.model.*

case class ParserContext(file: FSPath, content: String) {
  val defMeta = new DefMeta(this)
  val defEnum = new DefEnum(this, defMeta)
  val defDto = new DefDto(this, defMeta)
  val defAdt = new DefAdt(this, defMeta, defDto)
  val defModel = new DefModel(this, defMeta, defEnum, defDto, defAdt)

}
