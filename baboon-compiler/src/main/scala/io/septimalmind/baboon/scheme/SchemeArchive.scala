package io.septimalmind.baboon.scheme

import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.fundamentals.collections.nonempty.NEList

final case class SchemeArchiveEntry(path: String, content: String)

/** The `.baboon` files of a `:scheme --zip-output` archive: one rendered schema per selected domain
  * version at `schemas/<domain>/<version>.baboon`, sorted by path.
  */
object SchemeArchive {
  val SchemasRoot: String = "schemas"

  def entryPath(entry: SchemeDomainVersion): String = s"$SchemasRoot/${entry.pkg}/${entry.version}.baboon"

  def render(renderer: BaboonSchemeRenderer, family: BaboonFamily, selection: NEList[SchemeDomainVersion]): Either[NEList[String], NEList[SchemeArchiveEntry]] = {
    val rendered = selection.toList.map {
      entry =>
        renderer
          .render(family, entry.pkg, entry.version)
          .left.map(error => s"cannot render ${entry.pkg}@${entry.version}: $error")
          .map(content => SchemeArchiveEntry(entryPath(entry), content))
    }
    NEList.from(rendered.collect { case Left(error) => error }) match {
      case Some(errors) => Left(errors)
      case None         => Right(NEList.unsafeFrom(rendered.collect { case Right(entry) => entry }.sortBy(_.path)))
    }
  }
}
