package io.septimalmind.baboon.typer.model

import io.septimalmind.baboon.parser.model.RawNodeMeta

case class AliasInfo(
  name: TypeName,
  owner: Owner,
  targetRepr: String,
  resolvedTarget: TypeRef,
  meta: RawNodeMeta,
  root: Boolean,
)
