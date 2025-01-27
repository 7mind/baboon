package io.septimalmind.baboon.parser.defns

import fastparse.{ByNameOps, EagerOps, LiteralStr, P}
import io.septimalmind.baboon.parser.{ParserContext, model}
import io.septimalmind.baboon.parser.defns.base.{Literals, idt, kw, struct}
import io.septimalmind.baboon.parser.model.{RawForeign, RawForeignEntry, RawForeignEntryAttr, RawForeignEntryAttrs, RawTypeName}

import scala.annotation.unused

class DefForeign(@unused context: ParserContext, meta: DefMeta) {
  def kvPair[$: P]: P[RawForeignEntryAttr] = {
    import fastparse.ScalaWhitespace.whitespace
    (Literals.Literals.SimpleStr ~ "=" ~ Literals.Literals.SimpleStr).map {
      case (k, v) =>
        RawForeignEntryAttr(k, v)
    }
  }

  def foreignAttrs[$: P]: P[RawForeignEntryAttrs] = {
    import fastparse.ScalaWhitespace.whitespace
    (P("with") ~ "{" ~ kvPair.rep() ~ "}").map {
      a =>
        RawForeignEntryAttrs(a.toList)
    }
  }

  def foreignMember[$: P]: P[RawForeignEntry] = {
    import fastparse.ScalaWhitespace.whitespace
    (idt.symbol ~ "=" ~ Literals.Literals.SimpleStr ~ foreignAttrs.?).map {
      case (lang, tpe, attrs) =>
        RawForeignEntry(lang, tpe, attrs.getOrElse(RawForeignEntryAttrs.empty))
    }
  }

  def foreign[$: P]: P[Seq[RawForeignEntry]] = {
    import fastparse.ScalaWhitespace.whitespace
    P(foreignMember.rep())
  }

  def foreignEnclosed[$: P]: P[RawForeign] = {
    P(meta.member(kw.foreign, struct.enclosed(foreign))).map {
      case (meta, name, members) =>
        model.RawForeign(RawTypeName(name), members.toList, meta)
    }
  }
}
