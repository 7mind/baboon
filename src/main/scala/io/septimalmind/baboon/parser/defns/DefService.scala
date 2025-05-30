package io.septimalmind.baboon.parser.defns

import fastparse.*
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.defns.base.{kw, struct}
import io.septimalmind.baboon.parser.model.{RawFunc, RawFuncArg, RawService, RawTypeName}
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
        RawService(RawTypeName(name), members, meta)
    }
  }

  def methods[$: P]: P[Seq[RawFunc]] = {
    import fastparse.ScalaWhitespace.whitespace
    P(method.rep())
  }

  def method[$: P]: P[RawFunc] = {
    import fastparse.ScalaWhitespace.whitespace
    P(meta.member("def", struct.enclosed(sigpart.rep()))).map {
      case (meta, name, member) =>
        RawFunc(name, member, meta)
    }
  }

  def sigpart[$: P]: P[RawFuncArg] = {
    sigstruct | inlineSigpart
  }

  def sigstruct[$: P]: P[RawFuncArg.Ref] = {
    import fastparse.ScalaWhitespace.whitespace

    meta.withMeta(marker ~ "=" ~ dto.typeRef).map {
      case (meta, (marker, ref)) =>
        RawFuncArg.Ref(ref, marker, meta)
    }
  }

  def inlineSigpart[$: P]: P[RawFuncArg.Struct] = {
    (dto.dtoEnclosed | adt.adtEnclosed | enm.enumEnclosed).map(defn => RawFuncArg.Struct(defn))
  }

  def marker[$: P]: P[String] = {
    ("in" | "out" | "err").!
  }

}
