package io.septimalmind.baboon.parser

import io.septimalmind.baboon.CompilerOptions
import io.septimalmind.baboon.parser.model.RawInclude
import izumi.functional.bio.Error2

class BaboonInclusionResolverImpl[F[+_, +_]: Error2](
  options: CompilerOptions,
  inputs: Seq[BaboonParser.Input],
) extends BaboonInclusionResolver[F] {
  def getIclusionContent(inc: RawInclude): Option[String] = {
    None
  }
}
