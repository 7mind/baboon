package io.septimalmind.baboon.parser.defns

import fastparse.P
import io.septimalmind.baboon.parser.{ParserContext, model}
import io.septimalmind.baboon.parser.defns.base.{Literals, idt, kw, struct}
import io.septimalmind.baboon.parser.model.{RawForeign, RawTypeName}

class DefForeign(context: ParserContext, meta: DefMeta) {

  def foreignMember[$: P]: P[(String, String)] = {
    import fastparse.ScalaWhitespace.whitespace
    idt.symbol ~ "=" ~ Literals.Literals.SimpleStr
  }

  def foreign[$: P]: P[Seq[(String, String)]] = {
    import fastparse.ScalaWhitespace.whitespace
    P(foreignMember.rep())
  }

  def foreignEnclosed[$: P]: P[RawForeign] = {
    P(meta.member(kw.foreign, struct.enclosed(foreign))).map {
      case (meta, name, members) =>
        model.RawForeign(RawTypeName(name), members.toMap, meta)
    }
  }
}
