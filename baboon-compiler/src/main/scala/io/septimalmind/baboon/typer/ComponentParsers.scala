package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TyperIssue}
import io.septimalmind.baboon.parser.model.{RawHeader, RawVersion}
import io.septimalmind.baboon.typer.model.{ParsedVersion, Pkg, Version}
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList

trait ComponentParsers[F[+_, +_]] {
  def parsePkg(
    header: RawHeader
  ): F[NEList[BaboonIssue], Pkg]

  def parseVersion(
    version: RawVersion
  ): F[NEList[BaboonIssue], Version]
}

object ComponentParsers {

  class ComponentParsersImpl[F[+_, +_]: Error2](
  ) extends ComponentParsers[F] {
    def parsePkg(
      header: RawHeader
    ): F[NEList[BaboonIssue], Pkg] = {
      for {
        nel <- F.fromOption(BaboonIssue.of(TyperIssue.EmptyPackageId(header))) {
          NEList.from(header.name)
        }
        // TODO: validate format
      } yield {
        Pkg(nel)
      }
    }

    def parseVersion(
      version: RawVersion
    ): F[NEList[BaboonIssue], Version] = {
      for {
        v <- F.pure(version.value)
        _ <- F.fromOption(BaboonIssue.of(TyperIssue.GenericTyperIssue(s"Bad version format in '$v'", version.meta)))(ParsedVersion.parse(v))
      } yield {
        Version(v)
      }
    }
  }

}
