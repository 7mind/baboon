package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.translator.csharp.CSCodecTranslator.CodecMeta
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree

trait CSCodecTranslator {
  def translate(defn: DomainMember.User,
                csRef: CSValue.CSType,
                srcRef: CSValue.CSType,
                domain: Domain): TextTree[CSValue]

  def codecName(name: CSValue.CSType): CSValue.CSType

  def codecMeta(defn: DomainMember.User, name: CSValue.CSType): CodecMeta

  def metaField(): TextTree[CSValue]

}

object CSCodecTranslator {
  case class CodecMeta(member: TextTree[CSValue])
}
