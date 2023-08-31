package io.septimalmind.baboon

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.BaboonFamilyManager
import io.septimalmind.baboon.typer.model.BaboonFamily
import io.septimalmind.baboon.validator.BaboonValidator
import izumi.functional.IzEitherAggregations.*
import izumi.fundamentals.collections.nonempty.{NonEmptyList, NonEmptyString}
import izumi.fundamentals.platform.files.IzFiles

import java.nio.file.Path
import scala.util.Try

trait BaboonLoader {
  def load(paths: List[Path]): Either[NonEmptyList[BaboonIssue], BaboonFamily]
}

object BaboonLoader {
  class BaboonLoaderImpl(manager: BaboonFamilyManager,
                         validator: BaboonValidator)
      extends BaboonLoader {
    override def load(
      paths: List[Path]
    ): Either[NonEmptyList[BaboonIssue], BaboonFamily] = {
      for {
        inputs <- paths.biMapAggregate { path =>
          for {
            content <- Try(IzFiles.readString(path.toFile)).toEither.left
              .map(e => NonEmptyList(BaboonIssue.CantReadInput(e)))
          } yield {
            BaboonParser.Input(
              FSPath.parse(NonEmptyString.unsafeFrom(path.toString)),
              content
            )
          }
        }
        out <- manager.load(inputs)
        _ <- validator.validate(out)
      } yield {
        out
      }
    }
  }
}
