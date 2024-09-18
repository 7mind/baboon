package io.septimalmind.baboon.typer.model

import io.septimalmind.baboon.typer.model.Scope.{RootScope, ScopeUID}

case class ScopeTree(root: RootScope[FullRawDefn],
                     parents: Map[ScopeUID, ScopeUID],
                     index: Map[ScopeUID, Scope[FullRawDefn]])
