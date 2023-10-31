package io.septimalmind.baboon.parser.defns

import fastparse.*
import io.septimalmind.baboon.parser.{ParserContext, model}
import io.septimalmind.baboon.parser.defns.base.{idt, kw, struct}
import io.septimalmind.baboon.parser.model.{
  RawDto,
  RawDtoMember,
  RawField,
  RawFieldName,
  RawTypeName,
  RawTypeRef,
  ScopedRef
}
import izumi.fundamentals.collections.nonempty.NEList

class DefDto(context: ParserContext, meta: DefMeta) {
  def typeParams[$: P]: P[NEList[RawTypeRef]] = {
    import fastparse.SingleLineWhitespace.whitespace
    ("[" ~ typeRef.rep(min = 1, sep = ",") ~ "]")
      .map(p => NEList.unsafeFrom(p.toList))
  }

  def scopedRef[$: P]: P[ScopedRef] = {
    idt.symbolSeq.map(
      s => ScopedRef(NEList.unsafeFrom(s.map(p => RawTypeName(p)).toList))
    )
  }

  def typeRef[$: P]: P[RawTypeRef] = {
    import fastparse.SingleLineWhitespace.whitespace
    (idt.symbol ~ typeParams.?).map {
      case (name, params) =>
        params match {
          case Some(value) =>
            RawTypeRef.Constructor(RawTypeName(name), value)
          case None =>
            RawTypeRef.Simple(RawTypeName(name))
        }
    }
  }

  def fieldName[$: P]: P[RawFieldName] =
    idt.symbol.map(name => RawFieldName(name))

  def fieldDef[$: P]: P[RawField] = {
    import fastparse.ScalaWhitespace.whitespace
    (fieldName ~ ":" ~ typeRef).map { case (n, t) => model.RawField(n, t) }
  }

  def parentDef[$: P]: P[ScopedRef] = {
    import fastparse.ScalaWhitespace.whitespace
    ("+" ~ scopedRef)
  }

  def unparentDef[$: P]: P[ScopedRef] = {
    import fastparse.ScalaWhitespace.whitespace
    ("-" ~ scopedRef)
  }

  def unfieldDef[$: P]: P[RawField] = {
    import fastparse.ScalaWhitespace.whitespace
    ("-" ~ fieldDef)
  }


  def dtoMember[$: P]: P[RawDtoMember] =
    (P(meta.withMeta(fieldDef)).map {
      case (meta, field) =>
        model.RawDtoMember.FieldDef(field, meta)
    } | P(meta.withMeta(parentDef)).map {
      case (meta, parent) =>
        model.RawDtoMember.ParentDef(parent, meta)
    } | P(meta.withMeta(unfieldDef)).map {
      case (meta, field) =>
        model.RawDtoMember.UnfieldDef(field, meta)
    } | P(meta.withMeta(unparentDef)).map {
      case (meta, parent) =>
        model.RawDtoMember.UnparentDef(parent, meta)
    })

  def dto[$: P]: P[Seq[RawDtoMember]] = {
    import fastparse.ScalaWhitespace.whitespace
    P(dtoMember.rep())
  }

  def dtoEnclosed[$: P]: P[RawDto] = {
    P(meta.member(kw.data, struct.enclosed(dto))).map {
      case (meta, name, members) => RawDto(RawTypeName(name), members, meta)
    }
  }

}
