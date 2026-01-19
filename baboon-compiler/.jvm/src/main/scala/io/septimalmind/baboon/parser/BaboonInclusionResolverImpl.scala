package io.septimalmind.baboon.parser

import io.septimalmind.baboon.CompilerOptions
import io.septimalmind.baboon.parser.model.RawInclude
import io.septimalmind.baboon.parser.model.FSPath
import izumi.functional.bio.Error2
import izumi.fundamentals.collections.nonempty.NEString
import izumi.fundamentals.platform.files.IzFiles

class BaboonInclusionResolverImpl[F[+_, +_]](
  options: CompilerOptions
)(implicit @annotation.unused evidence: Error2[F]
) extends BaboonInclusionResolver[F] {
  import io.septimalmind.baboon.PathTools.*

  def resolveInclude(inc: RawInclude): Option[(FSPath, String)] = {
    options.directoryInputs
      .iterator
      .map(_.resolve(inc.value))
      .map(_.toAbsolutePath.normalize())
      .map(p => p.toFile)
      .find(f => f.exists() && f.isFile)
      .map { file =>
        val path = FSPath.parse(NEString.unsafeFrom(file.getAbsolutePath))
        path -> IzFiles.readString(file)
      }
  }
}
