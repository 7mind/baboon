package io.septimalmind.baboon.typer.model

import io.septimalmind.baboon.typer.model.Scope.{NestedScope, RootScope}

case class ScopeTree(
  root: RootScope[FullRawDefn],
  // the underlying map is an identity hashmap!
  parents: scala.collection.Map[NestedScope[FullRawDefn], Scope[FullRawDefn]]
)
