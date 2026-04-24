package io.septimalmind.baboon.parser.model

import izumi.fundamentals.collections.nonempty.NEList

sealed trait RawTypeRef {
  def render: String = this match {
    case RawTypeRef.Simple(name, prefix) =>
      (prefix.map(_.name) :+ name.name).mkString(".")
    case RawTypeRef.Constructor(name, params, prefix) =>
      val base = (prefix.map(_.name) :+ name.name).mkString(".")
      s"$base[${params.toList.map(_.render).mkString(", ")}]"
    case RawTypeRef.AnyRef(qualifier, underlying) =>
      // `AnyRef` guarantees at least one of qualifier/underlying is defined (see require below),
      // so the rendered form always has brackets.
      val parts = qualifier.map(RawTypeRef.AnyRef.qualToString).toList ++ underlying.map(_.render).toList
      s"any[${parts.mkString(", ")}]"
  }
}

object RawTypeRef {
  case class Simple(name: RawTypeName, prefix: List[RawTypeName]) extends RawTypeRef
  case class Constructor(name: RawTypeName, params: NEList[RawTypeRef], prefix: List[RawTypeName]) extends RawTypeRef

  // `any` is a global builtin — never prefixed, optionally qualified and/or underlying-typed.
  // At least one of `qualifier` / `underlying` must be defined: bare `any` is represented as
  // `Simple(RawTypeName("any"), Nil)` at the parse level, NOT `AnyRef(None, None)`.
  case class AnyRef(qualifier: Option[AnyRef.Qual], underlying: Option[RawTypeRef]) extends RawTypeRef {
    require(
      qualifier.isDefined || underlying.isDefined,
      "AnyRef(None, None) is unreachable at parse level; bare `any` is represented as Simple(RawTypeName(\"any\"), Nil)",
    )
  }
  object AnyRef {
    sealed trait Qual
    case object DomainThis extends Qual
    case object DomainCurrent extends Qual

    def qualToString(q: Qual): String = q match {
      case DomainThis    => "domain:this"
      case DomainCurrent => "domain:current"
    }
  }
}
