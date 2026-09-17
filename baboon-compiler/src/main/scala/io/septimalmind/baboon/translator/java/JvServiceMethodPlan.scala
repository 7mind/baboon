package io.septimalmind.baboon.translator.java

import io.septimalmind.baboon.translator.ResolvedServiceResult
import io.septimalmind.baboon.typer.model.{TypeRef, Typedef}
import izumi.fundamentals.platform.strings.TextTree

final class JvServiceMethodPlan(
  val method: Typedef.MethodDef,
  resolveType: TypeRef => TextTree[JvValue],
  result: ResolvedServiceResult,
) {
  val hasError: Boolean                      = method.err.isDefined && !result.noErrors
  val hasOutput: Boolean                     = method.out.isDefined
  val declarationName: String                = JvTypeTranslator.escapeJvKeyword(method.name.name)
  lazy val input: TextTree[JvValue]          = resolveType(method.sig)
  lazy val output: Option[TextTree[JvValue]] = method.out.map(resolveType)
  lazy val error: Option[TextTree[JvValue]]  = method.err.map(resolveType)
}
