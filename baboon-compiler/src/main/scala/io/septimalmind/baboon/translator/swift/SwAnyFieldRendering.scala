package io.septimalmind.baboon.translator.swift

import io.septimalmind.baboon.translator.AnyFieldPlan
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

private[swift] object SwAnyFieldRendering {
  def kind(plan: AnyFieldPlan): String = "0x%02x".format(plan.kind & 0xFF)

  def arguments(plan: AnyFieldPlan): TextTree[SwValue] = {
    def optional(value: Option[String]): TextTree[SwValue] = value match {
      case Some(s) => q""""$s""""
      case None    => q"nil"
    }
    q"${kind(plan)}, ${optional(plan.staticDomain)}, ${optional(plan.staticVersion)}, ${optional(plan.staticTypeId)}"
  }
}
