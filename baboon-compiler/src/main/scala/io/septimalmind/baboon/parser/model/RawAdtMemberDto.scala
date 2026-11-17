package io.septimalmind.baboon.parser.model

import izumi.fundamentals.collections.nonempty.NEList

sealed trait RawAdtMember {
  def meta: RawNodeMeta
  def defn: RawDefn
}

/** Where the body of an ADT branch was written, which is where the names in it are looked up.
  *
  * `+ X` re-emits the branches of `X` into another ADT (`AdtInheritanceExpander`). The copy is owned by
  * the receiving ADT, but a name in its body must keep denoting what it denoted in `X`: the receiving
  * ADT may sit in a scope where that name is not visible at all, or denotes a different type.
  */
sealed trait RawAdtBranchOrigin

object RawAdtBranchOrigin {

  /** The branch is declared in the body of the ADT that carries it. The parser only produces this. */
  case object Declared extends RawAdtBranchOrigin

  /** The branch was re-emitted from another ADT. `declaringAdt` is the path, from the domain root, of
    * the ADT in whose body the branch is declared; a chain of inclusions keeps the first one.
    */
  case class ReEmitted(declaringAdt: NEList[RawTypeName]) extends RawAdtBranchOrigin
}

case class RawAdtMemberDto(dto: RawDto, meta: RawNodeMeta, origin: RawAdtBranchOrigin) extends RawAdtMember {
  override def defn: RawDefn = dto
}

case class RawAdtMemberContract(contract: RawContract, meta: RawNodeMeta, origin: RawAdtBranchOrigin) extends RawAdtMember {
  override def defn: RawDefn = contract
}

object RawAdtMember {

  /** `+ ref` — include the referenced ADT (all branches) or a single branch into this ADT.
    *
    * The All-vs-Branch decision is deferred to PR-63's typer-early pass, which calls
    * `scopeSupport.resolveScopedRef(ref)` to decide whether `ref` names a whole ADT (→ all
    * branches are included) or whether `ref.init` names an ADT and `ref.last` is a branch name
    * inside it (→ single branch included). The decision lives in PR-63 because parser-time context
    * is insufficient for namespace-qualified ADTs: `+ pkg.sub.ErrorAtom` is ambiguous at parse
    * time — `ErrorAtom` could be the ADT itself (qualified by `pkg.sub`) or a branch of the
    * `pkg.sub` ADT.
    */
  case class Include(ref: ScopedRef, meta: RawNodeMeta) extends RawAdtMember {
    override def defn: RawDefn = throw new RuntimeException(
      s"BUG: RawAdtMember.Include.defn must not be called; PR-63 typer-early pass must rewrite this arm before convertAdt runs: $ref"
    )
  }

  /** `- ref` — exclude the referenced ADT (all branches) or a single branch from this ADT's
    * branch set.
    *
    * See [[Include]] for the All-vs-Branch disambiguation note.
    */
  case class Exclude(ref: ScopedRef, meta: RawNodeMeta) extends RawAdtMember {
    override def defn: RawDefn = throw new RuntimeException(
      s"BUG: RawAdtMember.Exclude.defn must not be called; PR-63 typer-early pass must rewrite this arm before convertAdt runs: $ref"
    )
  }

  /** `^ ref` — intersect this ADT's branch set with all branches from the referenced ADT.
    *
    * See [[Include]] for the All-vs-Branch disambiguation note.
    */
  case class Intersect(ref: ScopedRef, meta: RawNodeMeta) extends RawAdtMember {
    override def defn: RawDefn = throw new RuntimeException(
      s"BUG: RawAdtMember.Intersect.defn must not be called; PR-63 typer-early pass must rewrite this arm before convertAdt runs: $ref"
    )
  }

  /** `keep *` or `keep A, B` — retain only specified branches in an ADT delta body.
    *
    * `branches = None` encodes the wildcard form `keep *` (keep all existing branches).
    * `branches = Some(names)` encodes selective retention `keep A, B, ...`.
    *
    * Consumed by the materialization pre-pass (a later task) before `convertAdt` runs.
    * A safety-net guard in `convertAdt` will throw if this arm reaches it unexpectedly.
    */
  case class Keep(branches: Option[NEList[RawTypeName]], meta: RawNodeMeta) extends RawAdtMember {
    override def defn: RawDefn = throw new RuntimeException(
      s"BUG: RawAdtMember.Keep.defn must not be called; materialization pre-pass must consume this arm before convertAdt runs: $branches"
    )
  }

  /** `drop X` — remove a single named branch from an ADT delta body.
    *
    * Consumed by the materialization pre-pass (a later task) before `convertAdt` runs.
    * A safety-net guard in `convertAdt` will throw if this arm reaches it unexpectedly.
    */
  case class Drop(branch: RawTypeName, meta: RawNodeMeta) extends RawAdtMember {
    override def defn: RawDefn = throw new RuntimeException(
      s"BUG: RawAdtMember.Drop.defn must not be called; materialization pre-pass must consume this arm before convertAdt runs: $branch"
    )
  }

}
