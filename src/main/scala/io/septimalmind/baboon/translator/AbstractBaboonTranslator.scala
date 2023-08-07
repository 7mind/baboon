package io.septimalmind.baboon.translator

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.fundamentals.collections.nonempty.NonEmptyList

case class Sources(files: Map[String, String])

trait AbstractBaboonTranslator {
  def translate(
    family: BaboonFamily
  ): Either[NonEmptyList[BaboonIssue.TranslationIssue], Sources]
}
