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

  override def compile(inputs: Seq[BaboonParser.Input]): Either[NEList[BaboonIssue], BaboonFamily] = {
    val F = implicitly[Error2[F]]
    runner.run(
      F.catchAll(
        F.map(manager.load(inputs.toList))(family => Right(family): Either[NEList[BaboonIssue], BaboonFamily])
      )(issues => F.pure(Left(issues): Either[NEList[BaboonIssue], BaboonFamily]))
    )
  }
}
