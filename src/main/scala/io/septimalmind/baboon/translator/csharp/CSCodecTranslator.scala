package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.translator.csharp.CSCodecTranslator.CodecMeta
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree

trait CSCodecTranslator {
  def translate(defn: DomainMember.User, csRef: CSValue.CSType, srcRef: CSValue.CSType): Option[TextTree[CSValue]]

  def isActive(id: TypeId): Boolean
  def id: String

  def codecName(name: CSValue.CSType): CSValue.CSType
  def codecMeta(defn: DomainMember.User, name: CSValue.CSType): Option[CodecMeta]
}

object CSCodecTranslator {
  case class CodecMeta(member: TextTree[CSValue])
}
