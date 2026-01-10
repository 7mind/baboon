package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.translator.scl.ScCodecTranslator.CodecMeta
import io.septimalmind.baboon.typer.model.{DomainMember, TypeId}
import izumi.fundamentals.platform.strings.TextTree

trait ScCodecTranslator {
  def translate(defn: DomainMember.User, csRef: ScValue.ScType, srcRef: ScValue.ScType): Option[TextTree[ScValue]]
  def codecName(name: ScValue.ScType): ScValue.ScType
  def codecMeta(defn: DomainMember.User, name: ScValue.ScType): Option[CodecMeta]
  def isActive(id: TypeId): Boolean
  def id: String
}

object ScCodecTranslator {
  case class CodecMeta(member: TextTree[ScValue])
}
