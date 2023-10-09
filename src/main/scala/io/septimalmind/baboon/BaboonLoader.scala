package io.septimalmind.baboon

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.BaboonFamilyManager
import io.septimalmind.baboon.typer.model.BaboonFamily
import io.septimalmind.baboon.validator.BaboonValidator
import izumi.functional.IzEither.*
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.fundamentals.platform.files.IzFiles

import java.nio.file.Path
import scala.util.Try

trait BaboonLoader {
  def load(paths: List[Path]): Either[NEList[BaboonIssue], BaboonFamily]
}

object BaboonLoader {
  class BaboonLoaderImpl(manager: BaboonFamilyManager,
                         validator: BaboonValidator)
    extends BaboonLoader {
    override def load(
                       paths: List[Path]
                     ): Either[NEList[BaboonIssue], BaboonFamily] = {
      for {
        inputs <- paths.biTraverse { path =>
          for {
            content <- Try(IzFiles.readString(path.toFile)).toEither.left
              .map(e => NEList(BaboonIssue.CantReadInput(path.toString, e)))
          } yield {
            BaboonParser.Input(
              FSPath.parse(NEString.unsafeFrom(path.toString)),
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
