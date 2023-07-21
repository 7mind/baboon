package io.septimalmind.baboon

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.BaboonFamilyManager.BaboonFamilyManagerImpl
import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.fundamentals.collections.nonempty.{NonEmptyList, NonEmptyString}

import java.nio.file.Path
import izumi.functional.IzEitherAggregations.*
import izumi.fundamentals.platform.files.IzFiles

import scala.util.Try
trait BaboonLoader {
  def load(paths: List[Path]): Either[NonEmptyList[BaboonIssue], BaboonFamily]
}

object BaboonLoader {
  class BaboonLoaderImpl() extends BaboonLoader {
    override def load(
      paths: List[Path]
    ): Either[NonEmptyList[BaboonIssue], BaboonFamily] = {
      val manager = new BaboonFamilyManagerImpl()

      for {
        inputs <- paths.biMapAggregate { path =>
          for {
            content <- Try(IzFiles.readString(path.toFile)).toEither.left
              .map(e => {
                e.printStackTrace(); NonEmptyList(BaboonIssue.TODOIssue())
              })
          } yield {
            BaboonParser.Input(
              FSPath.parse(NonEmptyString.unsafeFrom(path.toString)),
              content
            )
          }
        }
        out <- manager.load(inputs)
      } yield {
        out
      }
    }
  }
}
