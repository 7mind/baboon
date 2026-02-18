package io.septimalmind.baboon.parser.defns

import fastparse.{ByNameOps, EagerOps, LiteralStr, P}
import io.septimalmind.baboon.parser.{ParserContext, model}
import io.septimalmind.baboon.parser.defns.base.{Literals, idt, kw, struct}
import io.septimalmind.baboon.parser.model.{RawForeign, RawForeignDecl, RawForeignEntry, RawForeignEntryAttr, RawForeignEntryAttrs, RawTypeName}

import scala.annotation.unused

class DefForeign(@unused context: ParserContext, meta: DefMeta, defDto: DefDto) {
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

  def customForeignDecl[$: P]: P[RawForeignDecl.Custom] = {
    import fastparse.ScalaWhitespace.whitespace
    (Literals.Literals.SimpleStr ~ foreignAttrs.?).map {
      case (tpe, attrs) =>
        RawForeignDecl.Custom(tpe, attrs.getOrElse(RawForeignEntryAttrs.empty))
    }
  }

  def baboonRefDecl[$: P]: P[RawForeignDecl.BaboonRef] = {
    defDto.typeRef.map(RawForeignDecl.BaboonRef(_))
  }

  def foreignMember[$: P]: P[RawForeignEntry] = {
    import fastparse.ScalaWhitespace.whitespace
    (idt.symbol ~ "=" ~ (customForeignDecl | baboonRefDecl)).map {
      case (lang, decl) =>
        RawForeignEntry(lang, decl)
    }
  }

  def foreign[$: P]: P[Seq[RawForeignEntry]] = {
    import fastparse.ScalaWhitespace.whitespace
    P(foreignMember.rep())
  }

  def foreignEnclosed[$: P]: P[RawForeign] = {
    import fastparse.ScalaWhitespace.whitespace
    P(meta.member(kw.foreign, meta.derived ~ struct.enclosed(foreign))).map {
      case (meta, name, (derived, members)) =>
        model.RawForeign(RawTypeName(name), members.toList, derived, meta)
    }
  }
}
