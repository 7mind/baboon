package io.septimalmind.baboon.translator.dart

import io.septimalmind.baboon.translator.AnyFieldPlan
import io.septimalmind.baboon.typer.model.{Domain, TypeRef}
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

private[dart] object DtAnyFieldPlan {
  def fallbacks(field: TypeRef.Any, domain: Domain): (TextTree[DtValue], TextTree[DtValue], TextTree[DtValue]) = {
    val plan = AnyFieldPlan.forField(field, domain)
    def render(value: Option[String]): TextTree[DtValue] = value match {
      case Some(v) => q"\"$v\""
      case None    => q"null"
    }
    (render(plan.staticDomain), render(plan.staticVersion), render(plan.staticTypeId))
  }
}
