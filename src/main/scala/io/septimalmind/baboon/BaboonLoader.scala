package io.septimalmind.baboon

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.BaboonFamilyManager
import io.septimalmind.baboon.typer.model.BaboonFamily
import io.septimalmind.baboon.validator.BaboonValidator
import izumi.functional.bio.{Error2, F, ParallelErrorAccumulatingOps2}
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.fundamentals.platform.files.IzFiles

import java.nio.file.Path

trait BaboonLoader[F[+_, +_]] {
  def load(paths: List[Path]): F[NEList[BaboonIssue], BaboonFamily]
}

object BaboonLoader {
  class BaboonLoaderImpl[F[+_, +_]: Error2: ParallelErrorAccumulatingOps2](
    manager: BaboonFamilyManager[F],
    validator: BaboonValidator[F],
  ) extends BaboonLoader[F] {

    override def load(
      paths: List[Path]
    ): F[NEList[BaboonIssue], BaboonFamily] = {
      for {
        inputs <- F.parTraverseAccumErrors(paths) {
          path =>
            for {
              content <- F.fromAttempt {
                IzFiles.readString(path.toFile)
              }.leftMap(e => NEList(BaboonIssue.CantReadInput(path.toString, e)))
            } yield {
              BaboonParser.Input(
                FSPath.parse(NEString.unsafeFrom(path.toString)),
                content,
              )
            }
        }
        out <- manager.load(inputs)
        _   <- validator.validate(out)
      } yield {
        out
      }
    }
  }
}
