package io.septimalmind.baboon.parser.model

sealed trait RawAdtMember {
  def meta: RawNodeMeta
  def defn: RawDefn
}

case class RawAdtMemberDto(dto: RawDto, meta: RawNodeMeta) extends RawAdtMember {
  override def defn: RawDefn = dto
}

case class RawAdtMemberContract(contract: RawContract, meta: RawNodeMeta) extends RawAdtMember {
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

}
