package io.septimalmind.baboon

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.fundamentals.collections.nonempty.NEList

trait LockfileManager[F[+_, +_]] {
  def validateLock(model: BaboonFamily): F[NEList[BaboonIssue], Unit]
}
