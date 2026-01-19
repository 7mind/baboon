package io.septimalmind.baboon.lsp.state

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.BaboonFamilyManager
import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.functional.bio.Error2
import izumi.functional.quasi.QuasiIORunner
import izumi.fundamentals.collections.nonempty.NEList

/** JVM implementation of LspCompiler that wraps BaboonFamilyManager */
class JvmBaboonCompiler[F[+_, +_]: Error2](
  manager: BaboonFamilyManager[F],
  runner: QuasiIORunner[F[Throwable, _]]
) extends LspCompiler {

  override def reload(
    inputs: Seq[BaboonParser.Input],
    previous: Option[BaboonFamily],
  ): Either[NEList[BaboonIssue], BaboonFamily] = {
    val F = implicitly[Error2[F]]
    runner.run(
      F.catchAll(
        F.flatMap(buildReloadInputs(inputs.toList)) { reloadInputs =>
          F.map(manager.reload(previous, reloadInputs))(family => Right(family): Either[NEList[BaboonIssue], BaboonFamily])
        }
      )(issues => F.pure(Left(issues): Either[NEList[BaboonIssue], BaboonFamily]))
    )
  }

  private def buildReloadInputs(
    inputs: List[BaboonParser.Input]
  ): F[NEList[BaboonIssue], List[BaboonParser.ReloadInput]] = {
    val F = implicitly[Error2[F]]
    val paths = inputs.map(_.path)
    assert(paths.distinct.size == paths.size, "Duplicate file paths provided to LSP compiler")
    val reloadInputs = inputs.map { input =>
      BaboonParser.ReloadInput.Unparsed(input.path, input.content)
    }
    F.pure(reloadInputs)
  }
}
