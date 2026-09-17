package io.septimalmind.baboon.translator

import io.septimalmind.baboon.typer.model.{Domain, TypeRef}

final case class AnyFieldPlan(
  kind: Byte,
  staticDomain: Option[String],
  staticVersion: Option[String],
  staticTypeId: Option[String],
)

object AnyFieldPlan {
  def forField(field: TypeRef.Any, domain: Domain): AnyFieldPlan = {
    val (staticDomain, staticVersion) = field.variant match {
      case TypeRef.AnyVariant.Global  => (None, None)
      case TypeRef.AnyVariant.ThisDom => (Some(domain.id.toString), None)
      case TypeRef.AnyVariant.Current => (Some(domain.id.toString), Some(domain.version.v.toString))
    }
    AnyFieldPlan(
      TypeRef.AnyVariant.metaKindByte(field.variant, field.underlying.isDefined),
      staticDomain,
      staticVersion,
      field.underlying.map(_.id.toString),
    )
  }
}
