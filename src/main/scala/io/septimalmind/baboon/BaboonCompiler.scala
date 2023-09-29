package io.septimalmind.baboon

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator
import io.septimalmind.baboon.util.BLogger
import izumi.functional.IzEither.*
import izumi.fundamentals.collections.nonempty.NEList

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}
import scala.util.Try
import izumi.fundamentals.platform.strings.TextTree.*

trait BaboonCompiler {
  def run(inputs: Set[Path], output: Path): Either[NEList[BaboonIssue], Unit]
}

object BaboonCompiler {
  case class CompilerOptions(debug: Boolean,
                             obsoleteErrors: Boolean,
                             runtime: RuntimeGenOpt,
                             generateConversions: Boolean,
  )

  class BaboonCompilerImpl(loader: BaboonLoader,
                           translator: CSBaboonTranslator,
                           options: CompilerOptions,
                           logger: BLogger,
  ) extends BaboonCompiler {
    override def run(inputs: Set[Path],
                     output: Path): Either[NEList[BaboonIssue], Unit] = {
      for {
        loaded <- loader.load(inputs.toList)
        translated <- translator.translate(loaded)
        _ <- translated.files.map {
          case (p, content) =>
            Try {
              val tgt = output.resolve(p)
              tgt.getParent.toFile.mkdirs()

              if (options.debug) {
                logger.message("debug", q"$tgt\n$content")
              }

              Files.writeString(
                tgt,
                content,
                StandardCharsets.UTF_8,
                StandardOpenOption.CREATE,
                StandardOpenOption.TRUNCATE_EXISTING
              )
            }.toEither.left
              .map(t => NEList(BaboonIssue.CantWriteOutput(p, t)))
        }.biSequence_
      } yield {}
    }
  }

}
