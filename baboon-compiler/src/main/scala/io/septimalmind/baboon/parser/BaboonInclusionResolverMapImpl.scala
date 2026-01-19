package io.septimalmind.baboon.parser

import io.septimalmind.baboon.CompilerOptions
import io.septimalmind.baboon.parser.model.{FSPath, RawInclude}
import izumi.fundamentals.collections.nonempty.NEString

class BaboonInclusionResolverMapImpl[F[+_, +_]](
  options: CompilerOptions,
  inputs: Seq[BaboonParser.Input],
) extends BaboonInclusionResolver[F] {

  private val inputsCache: Map[FSPath, String] = inputs.map(i => i.path -> i.content).toMap

  private def resolvePath(baseDir: FSPath, relativePath: String): FSPath = {
    val relativeSegments   = relativePath.split("/").toSeq.filter(_.nonEmpty)
    val relativeNESegments = relativeSegments.map(NEString.unsafeFrom)

    baseDir match {
      case FSPath.Full(location, name) =>
        FSPath((location :+ name) ++ relativeNESegments)
      case FSPath.Relative(location, name) =>
        FSPath((location :+ name) ++ relativeNESegments)
      case FSPath.Name(name) =>
        FSPath(Seq(name) ++ relativeNESegments)
    }
  }

  def resolveInclude(inc: RawInclude): Option[(FSPath, String)] = {
    options.directoryInputs.iterator
      .map(dir => resolvePath(dir, inc.value))
      .flatMap(resolvedPath => inputsCache.get(resolvedPath).map(content => resolvedPath -> content))
      .nextOption()
  }
}
