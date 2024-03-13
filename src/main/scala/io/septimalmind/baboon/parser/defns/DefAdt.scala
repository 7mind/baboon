package io.septimalmind.baboon.parser.defns

import fastparse.*
import io.septimalmind.baboon.parser.{ParserContext, model}
import io.septimalmind.baboon.parser.defns.base.{kw, struct}
import io.septimalmind.baboon.parser.model.{RawAdt, RawAdtMember, RawTypeName}




class DefAdt(context: ParserContext, meta: DefMeta, defDto: DefDto) {
  def adtMember[$: P]: P[RawAdtMember] =
    P(meta.withMeta(defDto.dtoEnclosed)).map {
      case (meta, name) =>
        RawAdtMember(name, meta)
    }

  def adt[$: P]: P[Seq[RawAdtMember]] = {
    import fastparse.ScalaWhitespace.whitespace
    P(adtMember.rep())
  }

  def adtEnclosed[$: P]: P[RawAdt] = {
    P(meta.member(kw.adt, struct.enclosed(adt))).map {
      case (meta, name, members) => model.RawAdt(RawTypeName(name), members, meta)
    }
  }

}


