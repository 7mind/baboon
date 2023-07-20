package io.septimalmind.baboon.parser.model

sealed trait RawTLDef {
  def value: RawDefn
  def root: Boolean
  def setRoot(root: Boolean): RawTLDef
}

object RawTLDef {
  case class Enum(root: Boolean, value: RawEnum) extends RawTLDef {
    override def setRoot(root: Boolean): Enum = this.copy(root = root)
  }
  case class DTO(root: Boolean, value: RawDto) extends RawTLDef {
    override def setRoot(root: Boolean): DTO = this.copy(root = root)
  }
  case class ADT(root: Boolean, value: RawAdt) extends RawTLDef {
    override def setRoot(root: Boolean): ADT = this.copy(root = root)
  }

}
