package io.septimalmind.baboon.parser.defns

import fastparse.*
import io.septimalmind.baboon.parser.{ParserContext, model}
import io.septimalmind.baboon.parser.defns.base.{idt, kw, struct}
import io.septimalmind.baboon.parser.model.RawDtoMember.ContractRef
import io.septimalmind.baboon.parser.model.{RawContractRef, RawDto, RawDtoMember, RawField, RawFieldName, RawTypeName, RawTypeRef, ScopedRef}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.language.Quirks.Discarder

class DefDto(context: ParserContext, meta: DefMeta) {
  context.discard()

  def typeParams[$: P]: P[NEList[RawTypeRef]] = {
    import fastparse.SingleLineWhitespace.whitespace
    ("[" ~ typeRef.rep(min = 1, sep = ",") ~ "]")
      .map(p => NEList.unsafeFrom(p.toList))
  }

  def nonGenericTypeRef[$: P]: P[ScopedRef] = {
    idt.symbolSeq.map(s => ScopedRef(NEList.unsafeFrom(s.map(p => RawTypeName(p)).toList)))
  }

  def typeRef[$: P]: P[RawTypeRef] = {
    import fastparse.SingleLineWhitespace.whitespace
    (nonGenericTypeRef ~ typeParams.?).map {
      case (ref, params) =>
        val name   = ref.path.last.name
        val prefix = ref.path.toList.init

        params match {
          case Some(value) =>
            RawTypeRef.Constructor(RawTypeName(name), value, prefix)
          case None =>
            RawTypeRef.Simple(RawTypeName(name), prefix)
        }
    }
  }

  def fieldName[$: P]: P[RawFieldName] =
    idt.symbol.map(name => RawFieldName(name))

  def contractDef[$: P]: P[RawContractRef] = {
    import fastparse.ScalaWhitespace.whitespace
    ("is" ~ nonGenericTypeRef).map { case t => model.RawContractRef(t) }
  }

  def extendedContractRef[$: P]: P[ContractRef] = {
    meta
      .withMeta(contractDef)
      .map { case (meta, ref) => ContractRef(ref, meta) }
  }

  private def fieldWas[$: P]: P[RawFieldName] = {
    import fastparse.ScalaWhitespace.whitespace
    kw.was ~ fieldName
  }

  def fieldDef[$: P]: P[RawField] = {
    import fastparse.ScalaWhitespace.whitespace
    (fieldName ~ ":" ~ typeRef ~ fieldWas.?).map { case (n, t, prev) => model.RawField(n, t, prev) }
  }

  def parentDef[$: P]: P[ScopedRef] = {
    import fastparse.ScalaWhitespace.whitespace
    "+" ~ nonGenericTypeRef
  }

  def unparentDef[$: P]: P[ScopedRef] = {
    import fastparse.ScalaWhitespace.whitespace
    "-" ~ nonGenericTypeRef
  }

  def intersectionDef[$: P]: P[ScopedRef] = {
    import fastparse.ScalaWhitespace.whitespace
    "^" ~ nonGenericTypeRef
  }

  def unfieldDef[$: P]: P[RawField] = {
    import fastparse.ScalaWhitespace.whitespace
    "-" ~ fieldDef
  }

  def dtoMember[$: P]: P[RawDtoMember] =
    P(meta.withMeta(fieldDef)).map {
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
    } | P(meta.withMeta(intersectionDef)).map {
      case (meta, parent) =>
        model.RawDtoMember.IntersectionDef(parent, meta)
    } | extendedContractRef

  def dto[$: P]: P[Seq[RawDtoMember]] = {
    import fastparse.ScalaWhitespace.whitespace
    P(dtoMember.rep())
  }

  def dtoEnclosed[$: P]: P[RawDto] = {
    import fastparse.ScalaWhitespace.whitespace
    P(meta.member(kw.data, meta.derived ~ struct.enclosed(dto))).map {
      case (meta, name, (derived, members)) => RawDto(RawTypeName(name), members, derived, meta)
    }
  }

}
