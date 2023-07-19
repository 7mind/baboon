package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.RawDomain
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.Domain
import izumi.fundamentals.collections.nonempty.NonEmptyList



trait BaboonTyper {
  def process(
    model: RawDomain
  ): Either[NonEmptyList[BaboonIssue.TyperIssue], Domain]
}

class BaboonTyperImpl() extends BaboonTyper {
  override def process(
    model: RawDomain
  ): Either[NonEmptyList[BaboonIssue.TyperIssue], Domain] = ???
}
