package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.translator.AnyFieldPlan
import io.septimalmind.baboon.typer.model.{Domain, TypeRef}
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

final case class TsAnyFieldPlan(kindHex: String, staticDomain: TextTree[TsValue], staticVersion: TextTree[TsValue], staticTypeId: TextTree[TsValue])

object TsAnyFieldPlan {
  def forField(field: TypeRef.Any, domain: Domain): TsAnyFieldPlan = {
    val plan = AnyFieldPlan.forField(field, domain)
    def render(value: Option[String]): TextTree[TsValue] = value match {
      case Some(v) => q"\"$v\""
      case None    => q"undefined"
    }
    TsAnyFieldPlan("0x%02x".format(plan.kind & 0xFF), render(plan.staticDomain), render(plan.staticVersion), render(plan.staticTypeId))
  }
}
