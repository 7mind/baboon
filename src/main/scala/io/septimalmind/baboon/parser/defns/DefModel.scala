package io.septimalmind.baboon.parser.defns

import fastparse.*
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.defns.base.{idt, kw, struct}
import io.septimalmind.baboon.parser.model.{
  RawDomain,
  RawHeader,
  RawNodeMeta,
  RawTLDef,
  TypeName
}

case class RawDto(name: TypeName, members: Seq[RawDtoMember], meta: RawNodeMeta)
case class RawDtoMember(field: RawField, meta: RawNodeMeta)
case class RawTypeRef(name: TypeName)
case class RawFieldName(name: String)
case class RawField(name: RawFieldName, tpe: RawTypeRef)

class DefDto(context: ParserContext, meta: DefMeta) {
  def typeRef[$: P]: P[RawTypeRef] =
    idt.symbol.map(name => RawTypeRef(TypeName(name)))
  def fieldName[$: P]: P[RawFieldName] =
    idt.symbol.map(name => RawFieldName(name))

  def fieldDef[$: P]: P[RawField] = {
    import fastparse.ScalaWhitespace.whitespace
    (fieldName ~ ":" ~ typeRef).map { case (n, t) => RawField(n, t) }
  }

  def dtoMember[$: P]: P[RawDtoMember] =
    P(meta.withMeta(fieldDef)).map {
      case (meta, name) =>
        RawDtoMember(name, meta)
    }

  def dto[$: P]: P[Seq[RawDtoMember]] = {
    import fastparse.ScalaWhitespace.whitespace
    P(dtoMember.rep())
  }

  def dtoEnclosed[$: P]: P[RawDto] = {
    P(meta.member(kw.data, struct.enclosed(dto))).map {
      case (meta, name, members) => RawDto(TypeName(name), members, meta)
    }
  }

}

class DefAdt(context: ParserContext, meta: DefMeta) {}

class DefModel(context: ParserContext,
               meta: DefMeta,
               defEnum: DefEnum,
               defDto: DefDto,
               defAdt: DefAdt) {

  def header[$: P]: P[RawHeader] =
    meta.withMeta(P(kw(kw.model, idt.symbolSeq))).map(RawHeader.tupled)

  def choice[$: P]: P[RawTLDef.Enum] = defEnum.enumEnclosed.map(RawTLDef.Enum)
  def dto[$: P]: P[RawTLDef.DTO] = defDto.dtoEnclosed.map(RawTLDef.DTO)
  def adt[$: P]: P[RawTLDef.ADT] = ???

  def member[$: P]: P[RawTLDef] = {
    P(choice | dto)
  }

  def model[$: P]: P[RawDomain] = {
    import fastparse.ScalaWhitespace.whitespace
    P(Start ~ header ~ member.rep() ~ End).map {
      case (decl, members) =>
        RawDomain(decl, members)
    }
  }
}
