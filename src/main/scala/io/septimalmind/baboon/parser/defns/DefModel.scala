package io.septimalmind.baboon.parser.defns

import fastparse.*
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.defns.base.{Literals, idt, kw}
import io.septimalmind.baboon.parser.model.*
import izumi.fundamentals.platform.language.Quirks.Discarder

class DefModel(context: ParserContext,
               meta: DefMeta,
               defEnum: DefEnum,
               defDto: DefDto,
               defAdt: DefAdt,
               defForeign: DefForeign,
) {
  context.discard()

  def model[$: P]: P[RawDomain] = {
    import fastparse.ScalaWhitespace.whitespace
    P(Start ~ header ~ version ~ content ~ End).map {
      case (decl, version, members) =>
        RawDomain(decl, version, members)
    }
  }

  def header[$: P]: P[RawHeader] =
    meta.withMeta(P(kw(kw.model, idt.symbolSeq))).map(RawHeader.tupled)

  def version[$: P]: P[RawVersion] =
    meta
      .withMeta(P(kw(kw.version, Literals.Literals.SimpleStr)))
      .map(RawVersion.tupled)

  def include[$: P]: P[RawInclude] =
    meta
      .withMeta(P(kw(kw.include, Literals.Literals.SimpleStr)))
      .map(RawInclude.tupled)

  def member[$: P]: P[RawTLDef] = {
    import fastparse.ScalaWhitespace.whitespace
    P(kw.root.!.? ~ (choice | dto | adt | foreign)).map {
      case (root, defn) =>
        defn.setRoot(root.nonEmpty)
    }
  }

  def content[$: P]: P[RawContent] = {
    import fastparse.ScalaWhitespace.whitespace
    (include.rep() ~ member.rep()).map(RawContent.tupled)
  }

  def choice[$: P]: P[RawTLDef.Enum] =
    defEnum.enumEnclosed.map(RawTLDef.Enum(false, _))
  def dto[$: P]: P[RawTLDef.DTO] =
    defDto.dtoEnclosed.map(RawTLDef.DTO(false, _))
  def adt[$: P]: P[RawTLDef.ADT] =
    defAdt.adtEnclosed.map(RawTLDef.ADT(false, _))
  def foreign[$: P]: P[RawTLDef.Foreign] =
    defForeign.foreignEnclosed.map(RawTLDef.Foreign(false, _))
}
