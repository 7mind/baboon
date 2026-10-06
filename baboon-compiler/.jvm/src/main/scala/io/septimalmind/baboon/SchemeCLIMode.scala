package io.septimalmind.baboon

import io.septimalmind.baboon.scheme.{SchemeSelection, SchemeSelector}
import io.septimalmind.baboon.typer.model.{Pkg, Version}
import izumi.fundamentals.collections.nonempty.NEList

/** The two `:scheme` modes. Their flags are disjoint; mixing them, or giving one flag of a pair
  * without the other, is a CLI error.
  */
sealed trait SchemeCLIMode
object SchemeCLIMode {
  /** `--domain --version [--target]`: one rendered schema, to a file or (pure DSL) to stdout. */
  final case class Single(pkg: Pkg, version: Version, target: Option[String]) extends SchemeCLIMode

  /** `--domains --zip-output`: every selected domain version, rendered into one ZIP archive. */
  final case class Archive(selectors: NEList[SchemeSelector], zipOutput: String) extends SchemeCLIMode

  def parse(options: SchemeCLIOptions): Either[NEList[String], SchemeCLIMode] = {
    val singleFlags  = List("--domain" -> options.domain, "--version" -> options.version, "--target" -> options.target).collect { case (flag, Some(_)) => flag }
    val archiveFlags = List("--domains" -> options.domains, "--zip-output" -> options.zipOutput).collect { case (flag, Some(_)) => flag }

    if (singleFlags.isEmpty && archiveFlags.isEmpty) {
      Left(NEList("scheme: specify either --domain and --version (single schema) or --domains and --zip-output (ZIP archive)"))
    } else if (singleFlags.nonEmpty && archiveFlags.nonEmpty) {
      Left(NEList(s"scheme: ${singleFlags.mkString(", ")} (single-schema mode) cannot be combined with ${archiveFlags.mkString(", ")} (archive mode)"))
    } else if (archiveFlags.isEmpty) {
      (options.domain, options.version) match {
        case (Some(domain), Some(version)) => Right(Single(Pkg(NEList.unsafeFrom(domain.split("\\.").toList)), Version.parse(version), options.target))
        case (None, _)                     => Left(NEList("scheme: --domain is required with --version/--target"))
        case (_, None)                     => Left(NEList("scheme: --version is required with --domain"))
      }
    } else {
      (options.domains, options.zipOutput) match {
        case (Some(domains), Some(zipOutput)) => SchemeSelection.parseSelectors(domains).map(Archive(_, zipOutput))
        case (None, _)                        => Left(NEList("scheme: --zip-output requires --domains"))
        case (_, None)                        => Left(NEList("scheme: --domains requires --zip-output"))
      }
    }
  }
}
