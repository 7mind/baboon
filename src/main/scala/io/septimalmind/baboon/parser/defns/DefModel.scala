package io.septimalmind.baboon.parser.defns

import fastparse.*
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.defns.base.{Literals, idt, kw}
import io.septimalmind.baboon.parser.model.*

class DefModel(context: ParserContext,
               meta: DefMeta,
               defEnum: DefEnum,
               defDto: DefDto,
               defAdt: DefAdt) {

  def header[$: P]: P[RawHeader] =
    meta.withMeta(P(kw(kw.model, idt.symbolSeq))).map(RawHeader.tupled)

  def version[$: P]: P[RawVersion] =
    meta
      .withMeta(P(kw(kw.version, Literals.Literals.SimpleStr)))
      .map(RawVersion.tupled)

  def choice[$: P]: P[RawTLDef.Enum] = defEnum.enumEnclosed.map(RawTLDef.Enum)
  def dto[$: P]: P[RawTLDef.DTO] = defDto.dtoEnclosed.map(RawTLDef.DTO)
  def adt[$: P]: P[RawTLDef.ADT] = defAdt.adtEnclosed.map(RawTLDef.ADT)

  def member[$: P]: P[RawTLDef] = {
    P(choice | dto | adt)
  }

  def model[$: P]: P[RawDomain] = {
    import fastparse.ScalaWhitespace.whitespace
    P(Start ~ header ~ version ~ member.rep() ~ End).map {
      case (decl, version, members) =>
        RawDomain(decl, version, members)
    }
  }
}
