package io.septimalmind.baboon.parser.model

import io.septimalmind.baboon.parser.defns.RawDto

sealed trait RawTLDef

object RawTLDef {
  case class Enum(enum: RawEnum) extends RawTLDef
  case class DTO(dto: RawDto) extends RawTLDef
  case class ADT() extends RawTLDef

}
