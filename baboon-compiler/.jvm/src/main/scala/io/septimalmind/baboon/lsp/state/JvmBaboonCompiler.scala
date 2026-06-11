package io.septimalmind.baboon.lsp.state

import io.septimalmind.baboon.lsp.LspLogging
import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.BaboonFamilyManager
import io.septimalmind.baboon.typer.model.BaboonFamily
import io.septimalmind.baboon.util.BLogger
import izumi.functional.bio.Error2
import izumi.functional.quasi.QuasiIORunner
import izumi.fundamentals.collections.nonempty.NEList

/** JVM implementation of LspCompiler that wraps BaboonFamilyManager */
class JvmBaboonCompiler[F[+_, +_]: Error2](
  manager: BaboonFamilyManager[F],
  runner: QuasiIORunner[F[Throwable, _]],
  logger: BLogger,
) extends LspCompiler {

  override def reload(
    inputs: Seq[BaboonParser.Input],
    previous: Option[BaboonFamily],
  ): Either[NEList[BaboonIssue], BaboonFamily] = {
    val F = implicitly[Error2[F]]
    runner.run(
      F.catchAll(
        F.flatMap(buildReloadInputs(inputs.toList)) {
          reloadInputs =>
            F.map(manager.reload(previous, reloadInputs))(family => Right(family): Either[NEList[BaboonIssue], BaboonFamily])
        }
      )(issues => F.pure(Left(issues): Either[NEList[BaboonIssue], BaboonFamily]))
    )
  }

  private def buildReloadInputs(
    inputs: List[BaboonParser.Input]
  ): F[NEList[BaboonIssue], List[BaboonParser.ReloadInput]] = {
    val F = implicitly[Error2[F]]
    // Defense-in-depth dedup: other assemblers may feed this method with duplicate paths.
    // Keep-first policy mirrors the assembly order (first entry is authoritative).
    // Log whenever a duplicate with differing content is dropped so the discard is never silent.
    val seen     = scala.collection.mutable.LinkedHashMap.empty[String, BaboonParser.Input]
    var hasDups  = false
    inputs.foreach {
      input =>
        val key = input.path.asString
        seen.get(key) match {
          case None =>
            seen.put(key, input)
          case Some(existing) =>
            hasDups = true
            if (existing.content != input.content) {
              logger.message(
                LspLogging.Context,
                s"buildReloadInputs: duplicate path with differing content dropped — keeping first occurrence: ${input.path.asString}",
              )
            }
        }
    }
    if (hasDups) {
      logger.message(
        LspLogging.Context,
        s"buildReloadInputs: deduplicated ${inputs.size - seen.size} duplicate path(s) from ${inputs.size} inputs",
      )
    }
    val reloadInputs = seen.values.toList.map {
      input =>
        BaboonParser.ReloadInput.Unparsed(input.path, input.content)
    }
    F.pure(reloadInputs)
  }
}
