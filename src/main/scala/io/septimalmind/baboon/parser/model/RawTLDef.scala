package io.septimalmind.baboon.parser.model

import io.septimalmind.baboon.parser.defns.RawAdt

sealed trait RawTLDef

object RawTLDef {
  case class Enum(value: RawEnum) extends RawTLDef
  case class DTO(value: RawDto) extends RawTLDef
  case class ADT(value: RawAdt) extends RawTLDef

}
