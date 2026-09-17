package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.translator.ResolvedServiceResult
import io.septimalmind.baboon.typer.model.{TypeRef, Typedef}
import izumi.fundamentals.platform.strings.TextTree

final class ScServiceMethodPlan(
  method: Typedef.MethodDef,
  resolveType: TypeRef => TextTree[ScValue],
  result: ResolvedServiceResult,
) {
  val methodName: String = method.name.name
  val hasError: Boolean  = method.err.isDefined && !result.noErrors

  // Keep type resolution demand-driven: a disabled transport or error mode
  // must not start resolving types that its renderer never used.
  lazy val input: TextTree[ScValue]          = resolveType(method.sig)
  lazy val output: Option[TextTree[ScValue]] = method.out.map(resolveType)
  lazy val error: Option[TextTree[ScValue]]  = method.err.map(resolveType)
}
