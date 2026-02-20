package io.septimalmind.baboon.translator.swift

import io.septimalmind.baboon.translator.FQNSymbol
import izumi.fundamentals.collections.nonempty.NEList

sealed trait SwValue

object SwValue {
  case class SwPackageId(parts: NEList[String])

  object SwPackageId {
    def apply(pkg: String): SwPackageId =
      SwPackageId(NEList.unsafeFrom(pkg.split('.').toList))
  }

  case class SwType(
    pkg: SwPackageId,
    name: String,
    fq: Boolean = false,
    predef: Boolean = false,
    importAs: Option[String] = None,
    localName: Option[String] = None,
  ) extends SwValue {
    def fullyQualified: SwType = this.copy(fq = true)
    def asName: SwTypeName     = SwTypeName(name)
    def asDeclName: SwTypeName = SwTypeName(localName.getOrElse(name))

    override def toString: String = s"${pkg.parts.mkString(".")}#$name"
  }

  case class SwTypeName(name: String) extends SwValue

  implicit object FQNSwValue extends FQNSymbol[SwValue] {
    override def fullyQualified(value: SwValue): SwValue = value match {
      case t: SwType     => t.fullyQualified
      case n: SwTypeName => n
    }
  }
}
