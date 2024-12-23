package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.translator.csharp.CSCodecTranslator.CodecMeta
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree

trait CSCodecTranslator {
  def translate(defn: DomainMember.User,
                csRef: CSValue.CSType,
                srcRef: CSValue.CSType,
               ): Option[TextTree[CSValue]]

  def codecType(): CSValue.CSType

  def codecName(name: CSValue.CSType): CSValue.CSType

  def codecMeta(defn: DomainMember.User, name: CSValue.CSType): CodecMeta

  def codecInterfaceProperty(): TextTree[CSValue]

  def codecImplProperty(): TextTree[CSValue]

  def codecGenericImplField(): TextTree[CSValue]

  def codecImplField(): TextTree[CSValue]

}

object CSCodecTranslator {
  case class CodecMeta(member: TextTree[CSValue])
}
