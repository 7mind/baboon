package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.typer.model.{DomainMember, TypeId}
import izumi.fundamentals.platform.strings.TextTree

trait TsCodecTranslator {
  def translate(defn: DomainMember.User, tsRef: TsValue.TsType, srcRef: TsValue.TsType): Option[TextTree[TsValue]]
  def codecName(name: TsValue.TsType): TsValue.TsType
  def codecMeta(definition: DomainMember.User, name: TsValue.TsType): Option[TextTree[TsValue]]
  def isActive(id: TypeId): Boolean
  def id: String
}
