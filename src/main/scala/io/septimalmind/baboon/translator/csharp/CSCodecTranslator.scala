package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree

trait CSCodecTranslator {
  def translate(defn: DomainMember.User,
                name: CSValue.CSType,
                version: Version): TextTree[CSValue]
}
