package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.translator.FQNSymbol
import izumi.fundamentals.collections.nonempty.NEList

sealed trait TsValue

object TsValue {
  case class TsModuleId(parts: NEList[String])

  object TsModuleId {
    def apply(pkg: String): TsModuleId =
      TsModuleId(NEList.unsafeFrom(pkg.split("/").toList))
  }

  case class TsType(
    module: TsModuleId,
    name: String,
    fq: Boolean = false,
    predef: Boolean = false,
  ) extends TsValue {
    def fullyQualified: TsType = this.copy(fq = true)
    def asName: TsTypeName     = TsTypeName(name)

    override def toString: String = s"${module.parts.mkString("/")}/${name}"
  }

  case class TsTypeName(name: String) extends TsValue

  implicit object FQNTsValue extends FQNSymbol[TsValue] {
    override def fullyQualified(value: TsValue): TsValue = value match {
      case t: TsType     => t.fullyQualified
      case n: TsTypeName => n
    }
  }
}
