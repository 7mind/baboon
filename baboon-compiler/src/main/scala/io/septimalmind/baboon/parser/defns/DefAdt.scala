package io.septimalmind.baboon.parser.defns

import fastparse.*
import io.septimalmind.baboon.parser.{ParserContext, model}
import io.septimalmind.baboon.parser.defns.base.{kw, struct}
import io.septimalmind.baboon.parser.model.RawDtoMember.ContractRef
import io.septimalmind.baboon.parser.model.{RawAdt, RawAdtMember, RawAdtMemberContract, RawAdtMemberDto, RawTypeName}
import izumi.fundamentals.platform.language.Quirks.Discarder

class DefAdt(context: ParserContext, meta: DefMeta, defDto: DefDto, defContract: DefContract) {
  context.discard()

  def adtMember[$: P]: P[RawAdtMemberDto] =
    P(meta.withMeta(defDto.dtoEnclosed)).map {
      case (meta, dto) =>
        RawAdtMemberDto(dto, meta)
    }

  def adtMemberContract[$: P]: P[RawAdtMemberContract] =
    P(meta.withMeta(defContract.contractEnclosed)).map {
      case (meta, c) =>
        RawAdtMemberContract(c, meta)
    }

  /** `+ ref` — include the referenced ADT or branch; All-vs-Branch resolution deferred to PR-63. */
  def adtIncludeDef[$: P]: P[RawAdtMember.Include] = {
    import fastparse.ScalaWhitespace.whitespace
    P(meta.withMeta("+" ~ defDto.nonGenericTypeRef)).map {
      case (m, ref) => RawAdtMember.Include(ref, m)
    }
  }

  /** `- ref` — exclude the referenced ADT or branch; All-vs-Branch resolution deferred to PR-63. */
  def adtExcludeDef[$: P]: P[RawAdtMember.Exclude] = {
    import fastparse.ScalaWhitespace.whitespace
    P(meta.withMeta("-" ~ defDto.nonGenericTypeRef)).map {
      case (m, ref) => RawAdtMember.Exclude(ref, m)
    }
  }

  /** `^ ref` — intersect this ADT's branch set with the referenced ADT; deferred to PR-63. */
  def adtIntersectDef[$: P]: P[RawAdtMember.Intersect] = {
    import fastparse.ScalaWhitespace.whitespace
    P(meta.withMeta("^" ~ defDto.nonGenericTypeRef)).map {
      case (m, ref) => RawAdtMember.Intersect(ref, m)
    }
  }

  def adt[$: P]: P[Seq[Either[ContractRef, RawAdtMember]]] = {
    import fastparse.ScalaWhitespace.whitespace
    P(
      (adtIncludeDef | adtExcludeDef | adtIntersectDef | adtMember | adtMemberContract)
        .map(m => Right(m)) | defDto.extendedContractRef.map(ref => Left(ref))
    ).rep()
  }

  def adtEnclosed[$: P]: P[RawAdt] = {
    import fastparse.ScalaWhitespace.whitespace
    P(meta.member(kw.adt, defDto.templateHead.? ~ meta.derived ~ struct.enclosed(adt))).map {
      case (meta, name, (tps, derived, members)) =>
        val typeMembers     = members.collect { case Right(m) => m }
        val contractMembers = members.collect { case Left(m) => m }

        model.RawAdt(RawTypeName(name), typeMembers, contractMembers, derived, meta, tps.getOrElse(Nil))
    }
  }

}
