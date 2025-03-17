package io.septimalmind.baboon.parser.defns

import fastparse.*
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.defns.base.{idt, kw, struct}
import io.septimalmind.baboon.parser.model.{RawContract, RawDtoMember, RawService, RawTypeName}
import izumi.fundamentals.platform.language.Quirks

class DefService(
  context: ParserContext,
  meta: DefMeta,
  dto: DefDto,
  adt: DefAdt,
  enm: DefEnum,
) {
  Quirks.discard(context)
  def service[$: P]: P[RawService] = {
    P(meta.member(kw.service, struct.enclosed(methods))).map {
      case (meta, name, members) =>
        RawService(RawTypeName(name), meta)
    }
  }

  def marker[$: P]: P[String] = {
    ("in" | "out" | "err").!
  }

  def sigstruct[$: P]: P[Unit] = {
    dto.typeRef
  }

  def inlineSigpart[$: P]: P[Unit] = {
    dto.dtoEnclosed | adt.adtEnclosed | enm.enumEnclosed
  }

  def sigpart[$: P]: P[Unit] = {
    import fastparse.ScalaWhitespace.whitespace
    (marker ~ "=" ~ sigstruct) | inlineSigpart
  }

  def method[$: P]: P[Unit] = {
    import fastparse.ScalaWhitespace.whitespace
    P(meta.member("def", struct.enclosed(sigpart.rep()))).map {
      case (meta, name, defns) =>

    }
  }

  def methods[$: P]: P[Unit] = {
    import fastparse.ScalaWhitespace.whitespace
    P(method.rep())
  }
}
