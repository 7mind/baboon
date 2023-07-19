package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.{AbstractBaboonTranslator, Sources}
import io.septimalmind.baboon.typer.model.Domain
import izumi.fundamentals.collections.nonempty.NonEmptyList

class CSBaboonTranslator() extends AbstractBaboonTranslator {
  override def translate(
    models: NonEmptyList[Domain]
  ): Either[NonEmptyList[BaboonIssue.TranslationIssue], Sources] = ???
}
