package io.septimalmind.baboon.parser.defns

import fastparse.P
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.defns.base.{idt, kw, sep, struct}
import io.septimalmind.baboon.parser.model.{RawEnum, RawEnumMember, RawTypeName}

class DefEnum(context: ParserContext, meta: DefMeta) {
  def enumMemberName[$: P]: P[String] = idt.symbol

  def enumMember[$: P]: P[RawEnumMember] =
    P(meta.withMeta(enumMemberName)).map {
      case (meta, name) =>
        RawEnumMember(name, meta)
    }

  def defEnum[$: P]: P[Seq[RawEnumMember]] = {
    import fastparse.ScalaWhitespace.whitespace
    P(enumMember.rep())
  }

  //def sepEnumFreeForm[$: P]: P[Unit] = P("|" | ";" | ",")

  def sepEnum[$: P]: P[Unit] = {
    import fastparse.ScalaWhitespace.whitespace
    P((sep.ws.rep(1) | sep.NLC).rep(1))
  }

  def enumEnclosed[$: P]: P[RawEnum] = {
    P(meta.member(kw.choice, struct.enclosed(defEnum(sepEnum)))).map {
      case (meta, name, members) => RawEnum(RawTypeName(name), members, meta)
    }
  }
}
