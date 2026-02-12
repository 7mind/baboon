package io.septimalmind.baboon.translator.kotlin

import io.septimalmind.baboon.translator.kotlin.KtCodecTranslator.CodecMeta
import io.septimalmind.baboon.typer.model.{DomainMember, TypeId}
import izumi.fundamentals.platform.strings.TextTree

trait KtCodecTranslator {
  def translate(defn: DomainMember.User, ktRef: KtValue.KtType, srcRef: KtValue.KtType): Option[TextTree[KtValue]]
  def codecName(name: KtValue.KtType): KtValue.KtType
  def codecMeta(defn: DomainMember.User, name: KtValue.KtType): Option[CodecMeta]
  def isActive(id: TypeId): Boolean
  def id: String
}

object KtCodecTranslator {
  case class CodecMeta(member: TextTree[KtValue])
}
