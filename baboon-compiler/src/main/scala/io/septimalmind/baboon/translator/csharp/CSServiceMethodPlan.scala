package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.translator.ResolvedServiceResult
import io.septimalmind.baboon.typer.model.{TypeRef, Typedef}
import izumi.fundamentals.platform.strings.TextTree

final class CSServiceMethodPlan(
  val method: Typedef.MethodDef,
  resolveType: TypeRef => TextTree[CSValue],
  result: ResolvedServiceResult,
) {
  val methodName: String                     = CSTypes.escapeCsKeyword(method.name.name.capitalize)
  val hasError: Boolean                      = method.err.isDefined && !result.noErrors
  lazy val input: TextTree[CSValue]          = resolveType(method.sig)
  lazy val output: Option[TextTree[CSValue]] = method.out.map(resolveType)
  lazy val error: Option[TextTree[CSValue]]  = method.err.map(resolveType)
}
