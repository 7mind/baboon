package io.septimalmind.baboon.parser.defns

import fastparse.{ByNameOps, EagerOps, LiteralStr, P}
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.defns.base.{Literals, idt, kw, sep, struct}
import io.septimalmind.baboon.parser.model.{RawEnum, RawEnumConst, RawEnumMember, RawTypeName}

import scala.annotation.unused

class DefEnum(@unused context: ParserContext, meta: DefMeta) {
  def enumMemberName[$: P]: P[String] = idt.symbol

  def constInt[$: P]: P[Int] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit

    (P("-").? ~ Literals.Literals.Int).!.map {
      v =>
        // todo: .endsWith("L")
        v.toInt
    }
  }
  def enumVal[$: P]: P[RawEnumConst] = {
    import fastparse.ScalaWhitespace.whitespace
    (P("=") ~ constInt).map {
      v =>
        RawEnumConst.RawInt(v.toLong)
    }
  }
  def enumMemberWas[$: P]: P[String] = {
    import fastparse.SingleLineWhitespace.whitespace
    (LiteralStr(":") ~ kw.was ~ LiteralStr("[") ~ enumMemberName ~ LiteralStr("]")).map {
      prev =>
        prev
    }
  }
  def enumMember[$: P]: P[RawEnumMember] = {
    import fastparse.ScalaWhitespace.whitespace
    P(meta.withMeta(enumMemberName ~ enumVal.? ~ enumMemberWas.?)).map {
      case (meta, (name, enumVal, prevName)) =>
        RawEnumMember(name, enumVal, prevName, meta)
    }
  }

  def defEnum[$: P]: P[Seq[RawEnumMember]] = {
    import fastparse.ScalaWhitespace.whitespace
    P(enumMember.rep())
  }

  // def sepEnumFreeForm[$: P]: P[Unit] = P("|" | ";" | ",")

  def sepEnum[$: P]: P[Unit] = {
    import fastparse.ScalaWhitespace.whitespace
    P((sep.ws.rep(1) | sep.NLC).rep(1))
  }

  def enumEnclosed[$: P]: P[RawEnum] = {
    import fastparse.ScalaWhitespace.whitespace
    P(meta.member(kw.choice, meta.derived ~ struct.enclosed(defEnum(sepEnum)))).map {
      case (meta, name, (derived, members)) => RawEnum(RawTypeName(name), members, derived, meta)
    }
  }
}
