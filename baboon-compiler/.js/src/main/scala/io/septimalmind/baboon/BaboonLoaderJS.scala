package io.septimalmind.baboon

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.BaboonFamilyManager
import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.functional.bio.{Error2, ParallelErrorAccumulatingOps2}
import izumi.fundamentals.collections.nonempty.NEList

trait BaboonLoaderJS[F[+_, +_]] {
  def load(inputs: Seq[BaboonParser.Input]): F[NEList[BaboonIssue], BaboonFamily]
}

object BaboonLoaderJS {
  class BaboonLoaderJSImpl[F[+_, +_]: Error2](
    manager: BaboonFamilyManager[F]
  ) extends BaboonLoaderJS[F] {

    override def load(
      inputs: Seq[BaboonParser.Input]
    ): F[NEList[BaboonIssue], BaboonFamily] = {
      for {
        out <- manager.load(inputs.toList)
      } yield {
        out
      }
    }
  }
}
