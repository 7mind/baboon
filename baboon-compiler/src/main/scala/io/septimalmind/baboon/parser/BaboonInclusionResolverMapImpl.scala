package io.septimalmind.baboon.parser

import io.septimalmind.baboon.CompilerOptions
import io.septimalmind.baboon.parser.model.{FSPath, RawInclude}
import io.septimalmind.baboon.parser.model.issues.ParserIssue
import izumi.fundamentals.collections.nonempty.NEString

class BaboonInclusionResolverMapImpl[F[+_, +_]](
  options: CompilerOptions,
  inputs: Seq[BaboonParser.Input],
) extends BaboonInclusionResolver[F] {

  private val inputsCache: Map[FSPath, String] = inputs.map(i => i.path -> i.content).toMap

  def resolveInclude(inc: RawInclude, includedFrom: FSPath): Either[ParserIssue, (FSPath, String)] = {
    val relative = BaboonInclusionResolver.against(BaboonInclusionResolver.directoryOf(includedFrom), inc.value)
    val roots    = options.directoryInputs.toList.flatMap(dir => BaboonInclusionResolver.against(dir.asString, inc.value)).sorted
    BaboonInclusionResolver.resolve(inc, relative, roots)(read, identity)
  }

  private def read(path: String): Option[(FSPath, String)] = {
    val key = FSPath.parse(NEString.unsafeFrom(path))
    inputsCache.get(key).map(content => key -> content)
  }
}
