package io.septimalmind.baboon.translator.dart

import io.septimalmind.baboon.translator.dart.DtCodecTranslator.CodecMeta
import io.septimalmind.baboon.typer.model.{DomainMember, TypeId}
import izumi.fundamentals.platform.strings.TextTree

trait DtCodecTranslator {
  def translate(defn: DomainMember.User, dtRef: DtValue.DtType, srcRef: DtValue.DtType): Option[TextTree[DtValue]]
  def codecName(name: DtValue.DtType): DtValue.DtType
  def codecMeta(defn: DomainMember.User, name: DtValue.DtType): Option[CodecMeta]
  def isActive(id: TypeId): Boolean
  def id: String
}

object DtCodecTranslator {
  case class CodecMeta(member: TextTree[DtValue])
}
