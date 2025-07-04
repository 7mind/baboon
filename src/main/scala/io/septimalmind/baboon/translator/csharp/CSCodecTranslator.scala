package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.translator.csharp.CSCodecTranslator.CodecMeta
import io.septimalmind.baboon.translator.csharp.CSValue.CSTypeOrigin
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

trait CSCodecTranslator {
  def translate(defn: DomainMember.User, csRef: CSValue.CSType, srcRef: CSValue.CSType): Option[TextTree[CSValue]]

  def isActive(id: TypeId): Boolean
  def id: String

  def codecName(name: CSValue.CSType, origin: CSTypeOrigin.TypeInDomain): CSValue.CSType
  def codecMeta(defn: DomainMember.User, name: CSValue.CSType): Option[CodecMeta]
}

object CSCodecTranslator {
  case class CodecMeta(member: TextTree[CSValue])

  final class CodecArguments private (val level: Int) {
    def arg(v: String): TextTree[CSValue] = q"$v${level.toString}"
    def next: CodecArguments              = new CodecArguments(level + 1)
  }
  object CodecArguments {
    def empty: CodecArguments = new CodecArguments(0)
  }
}
