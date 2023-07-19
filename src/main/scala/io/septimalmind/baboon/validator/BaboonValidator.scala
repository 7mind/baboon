package io.septimalmind.baboon.validator

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.Domain
import izumi.fundamentals.collections.nonempty.NonEmptyList

trait BaboonValidator {
  def validate(
    models: NonEmptyList[Domain]
  ): Either[NonEmptyList[BaboonIssue.VerificationIssue], Unit]
}

class BaboonValidatorImpl() extends BaboonValidator {
  override def validate(
    models: NonEmptyList[Domain]
  ): Either[NonEmptyList[BaboonIssue.VerificationIssue], Unit] = ???
}
