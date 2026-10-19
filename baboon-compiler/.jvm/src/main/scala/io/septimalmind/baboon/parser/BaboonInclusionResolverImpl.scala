package io.septimalmind.baboon.parser

import io.septimalmind.baboon.CompilerOptions
import io.septimalmind.baboon.parser.model.RawInclude
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.ParserIssue
import izumi.functional.bio.Error2
import izumi.fundamentals.collections.nonempty.NEString
import izumi.fundamentals.platform.files.IzFiles

import java.nio.file.{Path, Paths}

class BaboonInclusionResolverImpl[F[+_, +_]](
  options: CompilerOptions
)(implicit @annotation.unused evidence: Error2[F]
) extends BaboonInclusionResolver[F] {
  import io.septimalmind.baboon.PathTools.*

  def resolveInclude(inc: RawInclude, includedFrom: FSPath): Either[ParserIssue, (FSPath, String)] = {
    val directory = Option(includedFrom.toPath.toAbsolutePath.getParent).getOrElse(Paths.get("").toAbsolutePath)
    val roots     = options.directoryInputs.toList.map(_.resolve(inc.value).toAbsolutePath.normalize()).sortBy(_.toString)
    BaboonInclusionResolver.resolve(inc, Some(directory.resolve(inc.value).normalize()), roots)(read, _.toString)
  }

  private def read(path: Path): Option[(FSPath, String)] = {
    val file = path.toFile
    Option.when(file.isFile)(FSPath.parse(NEString.unsafeFrom(file.getAbsolutePath)) -> IzFiles.readString(file))
  }
}
