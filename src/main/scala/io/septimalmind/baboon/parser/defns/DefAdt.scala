package io.septimalmind.baboon.parser.defns

import fastparse.*
import io.septimalmind.baboon.parser.{ParserContext, model}
import io.septimalmind.baboon.parser.defns.base.{kw, struct}
import io.septimalmind.baboon.parser.model.{
  RawAdt,
  RawAdtMember,
  RawAdtMemberContract,
  RawAdtMemberDto,
  RawTypeName
}
import izumi.fundamentals.platform.language.Quirks.Discarder

class DefAdt(context: ParserContext,
             meta: DefMeta,
             defDto: DefDto,
             defContract: DefContract) {
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

  def adt[$: P]: P[Seq[RawAdtMember]] = {
    import fastparse.ScalaWhitespace.whitespace
    P((adtMember | adtMemberContract).rep())
  }

  def adtEnclosed[$: P]: P[RawAdt] = {
    P(meta.member(kw.adt, struct.enclosed(adt))).map {
      case (meta, name, members) =>
        model.RawAdt(RawTypeName(name), members, meta)
    }
  }

}
