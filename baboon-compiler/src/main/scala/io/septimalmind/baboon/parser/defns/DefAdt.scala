package io.septimalmind.baboon.parser.defns

import fastparse.*
import io.septimalmind.baboon.parser.{ParserContext, model}
import io.septimalmind.baboon.parser.defns.base.{kw, struct}
import io.septimalmind.baboon.parser.model.RawDtoMember.ContractRef
import io.septimalmind.baboon.parser.model.{RawAdt, RawAdtMember, RawAdtMemberContract, RawAdtMemberDto, RawDtoMember, RawTypeName}
import izumi.fundamentals.platform.language.Quirks.Discarder

class DefAdt(context: ParserContext, meta: DefMeta, defDto: DefDto, defContract: DefContract) {
  context.discard()

  def adtMember[$: P]: P[RawAdtMemberDto] =
    defDto.dtoEnclosed.map { dto =>
      // `dtoEnclosed` calls `meta.member(kw.data, …)` internally, which calls `withMeta`
      // and captures any prefix `/** … */` doc directly into `dto.meta`.  Wrapping in
      // an outer `meta.withMeta` would consume the doc first (into a separate outer meta)
      // and leave `dto.meta.docs.prefix` empty — causing ADT-arm prefix-doc loss
      // (PR-30.4-D02).  Mirror `dto.meta` onto the wrapper so both sides see the doc.
      RawAdtMemberDto(dto, dto.meta)
    }

  def adtMemberContract[$: P]: P[RawAdtMemberContract] =
    defContract.contractEnclosed.map { c =>
      // Same reasoning as `adtMember` above: `contractEnclosed` calls `meta.member`
      // internally and captures the prefix doc into `c.meta`.  No outer `withMeta`.
      RawAdtMemberContract(c, c.meta)
    }

  /** `+ ref` — include the referenced ADT or branch; All-vs-Branch resolution deferred to PR-63. */
  def adtIncludeDef[$: P]: P[RawAdtMember.Include] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(meta.withMeta("+" ~ defDto.nonGenericTypeRef)).map {
      case (m, ref) => RawAdtMember.Include(ref, m)
    }
  }

  /** `- ref` — exclude the referenced ADT or branch; All-vs-Branch resolution deferred to PR-63. */
  def adtExcludeDef[$: P]: P[RawAdtMember.Exclude] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(meta.withMeta("-" ~ defDto.nonGenericTypeRef)).map {
      case (m, ref) => RawAdtMember.Exclude(ref, m)
    }
  }

  /** `^ ref` — intersect this ADT's branch set with the referenced ADT; deferred to PR-63. */
  def adtIntersectDef[$: P]: P[RawAdtMember.Intersect] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(meta.withMeta("^" ~ defDto.nonGenericTypeRef)).map {
      case (m, ref) => RawAdtMember.Intersect(ref, m)
    }
  }

  // Three-way tag for adt body members (T37):
  //   Right(m)          — a branch DTO/contract or structural +/-/^
  //   Left(Left(ref))   — `is <ContractRef>` clause
  //   Left(Right(extr)) — `has (mirror|contract) <Name>` clause
  private type AdtEntry = Either[Either[ContractRef, RawDtoMember.ExtractionDef], RawAdtMember]

  def adt[$: P]: P[Seq[AdtEntry]] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(
      (adtIncludeDef | adtExcludeDef | adtIntersectDef | adtMember | adtMemberContract)
        .map(m => Right(m): AdtEntry) |
      defDto.extendedContractRef.map(ref => Left(Left(ref)): AdtEntry) |
      defDto.extendedExtractionDef.map(extr => Left(Right(extr)): AdtEntry)
    ).rep()
  }

  def adtEnclosed[$: P]: P[RawAdt] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(meta.member(kw.adt, defDto.templateHead.? ~ meta.derived ~ struct.enclosed(adt))).map {
      case (meta, name, (tps, derived, members)) =>
        val typeMembers     = members.collect { case Right(m) => m }
        val contractMembers = members.collect { case Left(Left(m)) => m }
        val extractions     = members.collect { case Left(Right(e)) => e }

        model.RawAdt(RawTypeName(name), typeMembers, contractMembers, extractions, derived, meta, tps.getOrElse(Nil))
    }
  }

}
