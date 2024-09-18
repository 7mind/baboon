package io.septimalmind.baboon.typer.model

import io.septimalmind.baboon.typer.model.Scope.NestedScope

trait ScopeInContext {
  def tree: ScopeTree
  def scope: Scope[FullRawDefn]

  def parentOf(s: NestedScope[FullRawDefn]): ScopeInContext =
    CAnyScope(tree.parents(s), tree)
}

case class CNestedScope(scope: NestedScope[FullRawDefn], tree: ScopeTree)
    extends ScopeInContext

case class CAnyScope(scope: Scope[FullRawDefn], tree: ScopeTree)
    extends ScopeInContext
