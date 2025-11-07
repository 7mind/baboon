package io.septimalmind.baboon.parser

import io.septimalmind.baboon.CompilerOptions
import io.septimalmind.baboon.parser.model.RawInclude
import izumi.functional.bio.Error2
import izumi.fundamentals.platform.files.IzFiles

class BaboonInclusionResolverImpl[F[+_, +_]: Error2](
  options: CompilerOptions
) extends BaboonInclusionResolver[F] {
  def getIclusionContent(inc: RawInclude): Option[String] = {
    import io.septimalmind.baboon.PathTools.*
    options.directoryInputs
      .map(_.resolve(inc.value).toFile)
      .find(f => f.exists() && f.isFile)
      .map(IzFiles.readString)

  }
}
