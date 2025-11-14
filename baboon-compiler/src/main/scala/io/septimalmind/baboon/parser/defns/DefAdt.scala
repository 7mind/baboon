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

  def adt[$: P]: P[Seq[Either[ContractRef, RawAdtMember]]] = {
    import fastparse.ScalaWhitespace.whitespace
    P(
      (adtMember | adtMemberContract)
        .map(m => Right(m)) | defDto.extendedContractRef.map(ref => Left(ref))
    ).rep()
  }

  def adtEnclosed[$: P]: P[RawAdt] = {
    import fastparse.ScalaWhitespace.whitespace
    P(meta.member(kw.adt, meta.derived ~ struct.enclosed(adt))).map {
      case (meta, name, (derived, members)) =>
        val typeMembers     = members.collect { case Right(m) => m }
        val contractMembers = members.collect { case Left(m) => m }

        model.RawAdt(RawTypeName(name), typeMembers, contractMembers, derived, meta)
    }
  }

}
