package io.septimalmind.baboon.translator.rust

import io.septimalmind.baboon.typer.model.{DomainMember, TypeId}
import izumi.fundamentals.platform.strings.TextTree

trait RsCodecTranslator {
  def translate(defn: DomainMember.User, rsRef: RsValue.RsType, srcRef: RsValue.RsType): Option[TextTree[RsValue]]
  def codecName(name: RsValue.RsType): RsValue.RsType
  def isActive(id: TypeId): Boolean
  def id: String
}
