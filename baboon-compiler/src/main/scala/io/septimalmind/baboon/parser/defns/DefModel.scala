package io.septimalmind.baboon.parser.defns

import fastparse.*
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.defns.base.{Literals, idt, kw, struct}
import io.septimalmind.baboon.parser.model.*
import izumi.fundamentals.platform.language.Quirks.Discarder

class DefModel(
  context: ParserContext,
  meta: DefMeta,
  defEnum: DefEnum,
  defDto: DefDto,
  defAdt: DefAdt,
  defForeign: DefForeign,
  defContract: DefContract,
  defService: DefService,
) {
  context.discard()

  def pragma[$: P]: P[RawPragma] = {
    import fastparse.ScalaWhitespace.whitespace
    P(kw(kw.pragma, pragmaKey ~ "=" ~ Literals.Literals.SimpleStr)).map {
      case (key, value) => RawPragma(key, value)
    }
  }

  def pragmaKey[$: P]: P[String] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    P(idt.symbol.rep(sep = fastparse.LiteralStr("."), min = 1).!).map(_.trim)
  }

  def model[$: P]: P[RawDomain] = {
    import fastparse.ScalaWhitespace.whitespace
    P(Start ~ header ~ version ~ pragma.rep() ~ `import`.? ~ content ~ End).map {
      case (decl, version, pragmas, imp, members) =>
        RawDomain(decl, version, pragmas, imp, members)
    }
  }

  def header[$: P]: P[RawHeader] =
    meta.withMeta(P(kw(kw.model, idt.symbolSeq))).map(RawHeader.apply.tupled)

  def version[$: P]: P[RawVersion] =
    meta
      .withMeta(P(kw(kw.version, Literals.Literals.SimpleStr)))
      .map(RawVersion.apply.tupled)

  def `import`[$: P]: P[RawImport] = {
    import fastparse.ScalaWhitespace.whitespace

    meta
      .withMeta(P(kw(kw.`import`, Literals.Literals.SimpleStr) ~ "{" ~ "*" ~ "}" ~ ("without" ~ struct.enclosed(idt.symbol.rep())).?))
      .map {
        case (meta, (id, drop)) =>
          RawImport(meta, id, drop.toSeq.flatten.map(s => RawTypeName(s)).toSet)
      }
  }

  def include[$: P]: P[RawInclude] =
    meta
      .withMeta(P(kw(kw.include, Literals.Literals.SimpleStr)))
      .map(RawInclude.apply.tupled)

  def member[$: P]: P[RawTLDef] = {
    import fastparse.ScalaWhitespace.whitespace

    val main = P(kw.root.!.? ~ (choice | dto | adt | foreign | contract | service)).map {
      case (root, defn) =>
        defn.setRoot(root.nonEmpty)
    }

    P(main | namespace)
  }

  def content[$: P]: P[RawContent] = {
    import fastparse.ScalaWhitespace.whitespace
    (include.rep() ~ member.rep()).map(RawContent.apply.tupled)
  }

  def contentEof[$: P]: P[RawContent] = {
    import fastparse.ScalaWhitespace.whitespace
    content ~ End
  }

  def namespaceDef[$: P]: P[RawNamespace] = {
    P(meta.member(kw.namespace, struct.enclosed(content))).map {
      case (meta, name, members) =>
        RawNamespace(RawTypeName(name), members.defs, meta)
    }
  }

  def choice[$: P]: P[RawTLDef.Enum] =
    defEnum.enumEnclosed.map(RawTLDef.Enum(false, _))
  def dto[$: P]: P[RawTLDef.DTO] =
    defDto.dtoEnclosed.map(RawTLDef.DTO(false, _))
  def adt[$: P]: P[RawTLDef.ADT] =
    defAdt.adtEnclosed.map(RawTLDef.ADT(false, _))
  def foreign[$: P]: P[RawTLDef.Foreign] =
    defForeign.foreignEnclosed.map(RawTLDef.Foreign(false, _))
  def contract[$: P]: P[RawTLDef.Contract] =
    defContract.contractEnclosed.map(RawTLDef.Contract(false, _))
  def service[$: P]: P[RawTLDef.Service] =
    defService.service.map(RawTLDef.Service(false, _))
  def namespace[$: P]: P[RawTLDef.Namespace] =
    namespaceDef.map(RawTLDef.Namespace(_))
}
