package io.septimalmind.baboon.typer.model

import io.septimalmind.baboon.typer.model.Scope.NestedScope

trait ScopeInContext {
  def scope: Scope[ExtendedRawDefn]

  def parentOf(s: NestedScope[ExtendedRawDefn]): ScopeInContext =
    CAnyScope(
      s.defn.context
        .index(s.defn.context.parents(s.id))
        .map(d => ExtendedRawDefn(d, s.defn.context))
    )
}

case class CNestedScope(scope: NestedScope[ExtendedRawDefn])
    extends ScopeInContext

case class CAnyScope(scope: Scope[ExtendedRawDefn]) extends ScopeInContext
