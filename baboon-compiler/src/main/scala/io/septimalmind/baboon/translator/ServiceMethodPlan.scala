package io.septimalmind.baboon.translator

import io.septimalmind.baboon.typer.model.{TypeRef, Typedef}

final class ServiceMethodPlan[A](
  val method: Typedef.MethodDef,
  resolveType: TypeRef => A,
  result: ResolvedServiceResult,
  val methodName: String,
) {
  val hasError: Boolean  = method.err.isDefined && !result.noErrors
  val hasOutput: Boolean = method.out.isDefined

  // Disabled transports and error modes must not resolve types they do not use.
  lazy val input: A          = resolveType(method.sig)
  lazy val output: Option[A] = method.out.map(resolveType)
  lazy val error: Option[A]  = method.err.map(resolveType)
}
