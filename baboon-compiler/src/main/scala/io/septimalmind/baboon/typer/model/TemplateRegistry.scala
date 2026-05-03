package io.septimalmind.baboon.typer.model

import io.septimalmind.baboon.parser.model.{RawAdt, RawContract, RawDto, RawService, RawTypeName}

/** A raw template body, keyed by the template's own name.
  *
  * `rawDefn` is one of `RawDto`, `RawAdt`, `RawContract`, or `RawService`. The type-parameter
  * substitution machinery in PR-29.5 (`TemplateInstantiator`) reads `typeParams` to drive the
  * substitution walk over `rawDefn`'s field/member/method trees.
  *
  * The `rawDefn` is stored post-parse, pre-typer — `RawTypeRef.Simple(RawTypeName("T"), Nil)` nodes
  * inside it are un-substituted placeholders. PR-29.5 replaces each such node with the resolved
  * argument `RawTypeRef` when producing the monomorphised concrete type.
  */
sealed trait RawTemplateDefn
object RawTemplateDefn {
  case class Dto(raw: RawDto) extends RawTemplateDefn
  case class Adt(raw: RawAdt) extends RawTemplateDefn
  case class Contract(raw: RawContract) extends RawTemplateDefn
  case class Service(raw: RawService) extends RawTemplateDefn
}

/** Carry the template's declared type-parameter list and the cached raw body.
  *
  * `typeParams` holds the parameter names in declaration order, e.g. `List(RawTypeName("T"),
  * RawTypeName("E"))` for `data Foo[T, E] { … }`.
  */
case class TemplateBody(typeParams: List[RawTypeName], rawDefn: RawTemplateDefn)

/** Per-domain registry that maps a `TypeId.User`-shaped key `(Pkg, Owner, TypeName)` to the
  * template's body.
  *
  * The key mirrors the `TypeId.User(pkg, owner, name)` that `BaboonTyper.runTyper` would have
  * assigned to the type had it been a non-template. Using the same three-tuple ensures that
  * template name resolution in PR-29.5 is consistent with how the typer resolves all other
  * user-defined type names.
  *
  * `templates` may be empty for domains that contain no templates.
  */
case class TemplateRegistry(templates: Map[(Pkg, Owner, TypeName), TemplateBody]) {
  def isEmpty: Boolean = templates.isEmpty
}

object TemplateRegistry {
  val empty: TemplateRegistry = TemplateRegistry(Map.empty)
}
