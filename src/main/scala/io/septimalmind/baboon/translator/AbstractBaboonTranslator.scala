package io.septimalmind.baboon.translator

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.Domain
import izumi.fundamentals.collections.nonempty.NonEmptyList

case class Sources(files: Map[String, String])

trait AbstractBaboonTranslator {
  def translate(
    models: NonEmptyList[Domain]
  ): Either[NonEmptyList[BaboonIssue.TranslationIssue], Sources]
}
