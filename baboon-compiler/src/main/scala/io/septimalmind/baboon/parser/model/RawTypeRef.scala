package io.septimalmind.baboon.parser.model

import izumi.fundamentals.collections.nonempty.NEList

sealed trait RawTypeRef {
  def render: String = this match {
    case RawTypeRef.Simple(name, prefix) =>
      (prefix.map(_.name) :+ name.name).mkString(".")
    case RawTypeRef.Constructor(name, params, prefix) =>
      val base = (prefix.map(_.name) :+ name.name).mkString(".")
      s"$base[${params.toList.map(_.render).mkString(", ")}]"
  }
}

object RawTypeRef {
  case class Simple(name: RawTypeName, prefix: List[RawTypeName]) extends RawTypeRef
  case class Constructor(name: RawTypeName, params: NEList[RawTypeRef], prefix: List[RawTypeName]) extends RawTypeRef
}
