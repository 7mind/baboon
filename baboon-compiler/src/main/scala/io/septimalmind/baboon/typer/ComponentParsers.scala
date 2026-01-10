package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TyperIssue}
import io.septimalmind.baboon.parser.model.{RawHeader, RawVersion}
import io.septimalmind.baboon.typer.model.{Pkg, Version}
import izumi.functional.bio.*
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
        ret <- izumi.fundamentals.platform.versions.Version.parse(version.value) match {
          case c: izumi.fundamentals.platform.versions.Version.Canonical =>
            F.pure(c)
          case s: izumi.fundamentals.platform.versions.Version.Semver =>
            F.pure(s)
          case u: izumi.fundamentals.platform.versions.Version.Unknown =>
            F.fail(BaboonIssue.of(TyperIssue.GenericTyperIssue(s"Bad version format in '${u.version}'", version.meta)))
        }
      } yield {
        Version(ret)
      }
    }
  }

}
