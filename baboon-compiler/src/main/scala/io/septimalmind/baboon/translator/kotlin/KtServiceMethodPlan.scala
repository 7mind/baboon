package io.septimalmind.baboon.translator.kotlin

import io.septimalmind.baboon.translator.ResolvedServiceResult
import io.septimalmind.baboon.typer.model.{TypeRef, Typedef}
import izumi.fundamentals.platform.strings.TextTree

final class KtServiceMethodPlan(
  method: Typedef.MethodDef,
  resolveType: TypeRef => TextTree[KtValue],
  result: ResolvedServiceResult,
) {
  val methodName: String = KtTypeTranslator.escapeKtKeyword(method.name.name)
  val hasError: Boolean  = method.err.isDefined && !result.noErrors

  // Keep type resolution demand-driven: a disabled transport or error mode
  // must not start resolving types that its renderer never used.
  lazy val input: TextTree[KtValue]          = resolveType(method.sig)
  lazy val output: Option[TextTree[KtValue]] = method.out.map(resolveType)
  lazy val error: Option[TextTree[KtValue]]  = method.err.map(resolveType)
}
