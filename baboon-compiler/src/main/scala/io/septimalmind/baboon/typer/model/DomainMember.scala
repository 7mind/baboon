package io.septimalmind.baboon.typer.model

import io.septimalmind.baboon.parser.model.{RawMemberMeta, RawNodeMeta}
import io.septimalmind.baboon.typer.DocFormat

sealed trait DomainMember {
  def id: TypeId
}

object DomainMember {
  case class Builtin(id: TypeId.Builtin) extends DomainMember

  case class User(root: Boolean, defn: Typedef.User, derivations: Set[RawMemberMeta], meta: RawNodeMeta) extends DomainMember {
    def id: TypeId.User = defn.id
    def ownedByAdt: Boolean = {
      defn.id.owner match {
        case _: Owner.Adt => true
        case _            => false
      }
    }
    def isAdt: Boolean = {
      defn match {
        case _: Typedef.Adt => true
        case _              => false
      }
    }
    /** Type-level docs lifted from `meta.docs` (parser-side `RawDocs`).
      *
      * Defined as a body-level `val` rather than a constructor parameter so
      * that the case-class signature (and therefore `unapply` / pattern
      * matches across ~60 translator and typer files) is unaffected by
      * PR-30.3. The field is computed once at construction; `DocFormat`
      * is pure / deterministic.
      *
      * Layered drop note: parser captures docs uniformly at every
      * `withMeta` site (PR-30.2). Non-position carriers per spec §3.2
      * (namespace openers, plain non-template aliases, foreign per-language
      * mapping entries, ADT inheritance arms) do not surface here as
      * `DomainMember.User`, so their captured docs are silently dropped by
      * construction; suffix docs on type declarations are also impossible
      * because parser only stitches `//!` into field-line metas.
      */
    val docs: Docs = DocFormat.liftDocs(meta.docs)
  }
}
