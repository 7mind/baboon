package io.septimalmind.baboon

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.BaboonFamilyManager
import io.septimalmind.baboon.typer.model.BaboonFamily
import io.septimalmind.baboon.validator.BaboonValidator
import izumi.functional.bio.{Error2, F}
import izumi.functional.quasi.QuasiAsync
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.fundamentals.platform.files.IzFiles

import java.nio.file.Path
import scala.util.Try

trait BaboonLoader[F[+_, +_]] {
  def load(paths: List[Path]): F[NEList[BaboonIssue], BaboonFamily]
}

object BaboonLoader {
  class BaboonLoaderImpl[F[+_, +_]: Error2](
    manager: BaboonFamilyManager[F],
    validator: BaboonValidator[F],
  ) extends BaboonLoader[F] {

    override def load(
      paths: List[Path]
    ): F[NEList[BaboonIssue], BaboonFamily] = {
      for {
        inputs <- F.sequenceAccumErrors {
          QuasiAsync.quasiAsyncIdentity
            .parTraverse(paths) {
              path =>
                for {
                  content <- F.fromTry {
                    Try(IzFiles.readString(path.toFile))
                  }.leftMap(e => NEList(BaboonIssue.CantReadInput(path.toString, e)))
                } yield {
                  BaboonParser.Input(
                    FSPath.parse(NEString.unsafeFrom(path.toString)),
                    content,
                  )
                }
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
