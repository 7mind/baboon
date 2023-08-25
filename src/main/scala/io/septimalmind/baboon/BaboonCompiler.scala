package io.septimalmind.baboon

import io.septimalmind.baboon.BaboonCompiler.CompilerOptions
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator
import izumi.functional.IzEitherAggregations.*
import izumi.fundamentals.collections.nonempty.NonEmptyList

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}
import scala.util.Try

trait BaboonCompiler {
  def run(inputs: Set[Path],
          output: Path,
          options: CompilerOptions,
  ): Either[NonEmptyList[BaboonIssue], Unit]
}

object BaboonCompiler {
  case class CompilerOptions(debug: Boolean, obsoleteErrors: Boolean)
  class BaboonCompilerImpl() extends BaboonCompiler {
    override def run(inputs: Set[Path],
                     output: Path,
                     options: CompilerOptions,
    ): Either[NonEmptyList[BaboonIssue], Unit] = {
      for {
        loader <- Right(new BaboonLoader.BaboonLoaderImpl())
        loaded <- loader.load(inputs.toList)
        translator <- Right(new CSBaboonTranslator(options))
        translated <- translator.translate(loaded)
        _ <- translated.files.map {
          case (p, c) =>
            Try {
              val tgt = output.resolve(p)
              tgt.getParent.toFile.mkdirs()

              if (options.debug) {
                println(s">> $tgt")
                println(c)
              }

              Files.writeString(
                tgt,
                c,
                StandardCharsets.UTF_8,
                StandardOpenOption.CREATE,
                StandardOpenOption.TRUNCATE_EXISTING
              )
            }.toEither.left
              .map(t => NonEmptyList(BaboonIssue.CantWriteOutput(p, t)))
        }.biAggregateVoid
      } yield {}
    }
  }

}
