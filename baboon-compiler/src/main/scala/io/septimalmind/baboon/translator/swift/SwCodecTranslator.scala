package io.septimalmind.baboon.translator.swift

import io.septimalmind.baboon.translator.swift.SwCodecTranslator.CodecMeta
import io.septimalmind.baboon.typer.model.{DomainMember, TypeId}
import izumi.fundamentals.platform.strings.TextTree

trait SwCodecTranslator {
  def translate(defn: DomainMember.User, swRef: SwValue.SwType, srcRef: SwValue.SwType): Option[TextTree[SwValue]]
  def codecName(name: SwValue.SwType): SwValue.SwType
  def codecMeta(defn: DomainMember.User, name: SwValue.SwType): Option[CodecMeta]
  def isActive(id: TypeId): Boolean
  def id: String
}

object SwCodecTranslator {
  case class CodecMeta(member: TextTree[SwValue])
}
