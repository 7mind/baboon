package io.septimalmind.baboon.translator.java

import io.septimalmind.baboon.translator.java.JvCodecTranslator.CodecMeta
import io.septimalmind.baboon.typer.model.{DomainMember, Owner, TypeId}
import izumi.fundamentals.platform.strings.TextTree

trait JvCodecTranslator {
  def translate(defn: DomainMember.User, jvRef: JvValue.JvType, srcRef: JvValue.JvType): Option[TextTree[JvValue]]
  def codecName(name: JvValue.JvType, owner: Owner): JvValue.JvType
  def codecMeta(defn: DomainMember.User, name: JvValue.JvType): Option[CodecMeta]
  def isActive(id: TypeId): Boolean
  def id: String
}

object JvCodecTranslator {
  case class CodecMeta(member: TextTree[JvValue])
}
