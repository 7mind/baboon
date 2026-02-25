package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.translator.typescript.TsValue.TsType
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

trait TsDomainTreeTools {
  def makeDataMeta(defn: DomainMember.User): List[TextTree[TsValue]]
  def makeCodecMeta(defn: DomainMember.User, source: TsType): List[TextTree[TsValue]]
}

object TsDomainTreeTools {
  final class TsDomainTreeToolsImpl(
    domain: Domain,
    evolution: BaboonEvolution,
    typeTranslator: TsTypeTranslator,
    tsFileTools: TsFileTools,
  ) extends TsDomainTreeTools {
    override def makeDataMeta(defn: DomainMember.User): List[TextTree[TsValue]] = {
      val source = typeTranslator.asTsType(defn.id, domain, evolution, tsFileTools.definitionsBasePkg)
      mainMeta(defn, source, isCodec = false) ++ sameInVersion(defn, source) ++ adtMeta(defn, source, isCodec = false)
    }

    override def makeCodecMeta(defn: DomainMember.User, source: TsType): List[TextTree[TsValue]] = {
      mainMeta(defn, source, isCodec = true) ++ adtMeta(defn, source, isCodec = true)
    }

    private def sameInVersion(defn: DomainMember.User, source: TsType): List[TextTree[TsValue]] = {
      val unmodifiedSince = evolution.typesUnchangedSince(domain.version)(defn.id)
      if (defn.isAdt) {
        List(
          q"BaboonSameInVersions: [${unmodifiedSince.sameIn.map(v => q"\"${v.v.toString}\"").toList.join(", ")}]"
        )
      } else {
        List(
          q"""public static readonly BaboonSameInVersions = [${unmodifiedSince.sameIn.map(v => q"\"${v.v.toString}\"").toList.join(", ")}]
             |public baboonSameInVersions() {
             |    return $source.BaboonSameInVersions
             |}""".stripMargin
        )
      }
    }

    private def adtMeta(defn: DomainMember.User, source: TsType, isCodec: Boolean): List[TextTree[TsValue]] = {
      val adtId = defn.id
      val tpe   = typeTranslator.asTsType(adtId, domain, evolution, tsFileTools.definitionsBasePkg)
      if (defn.isAdt && !isCodec) {
        List(
          q"BaboonAdtTypeIdentifier: '${adtId.toString}'"
        )
      } else if (defn.ownedByAdt || defn.isAdt) {
        List(q"""public static readonly BaboonAdtTypeIdentifier = '${adtId.toString}'
                |public baboonAdtTypeIdentifier() {
                |    return $source.BaboonAdtTypeIdentifier
                |}
                |""".stripMargin)
      } else Nil
    }

    private def mainMeta(defn: DomainMember.User, source: TsType, isCodec: Boolean): List[TextTree[TsValue]] = {
      if (defn.isAdt && !isCodec) {
        List(
          q"BaboonDomainVersion: '${domain.version.v.toString}'",
          q"BaboonDomainIdentifier: '${defn.id.pkg.toString}'",
          q"BaboonTypeIdentifier: '${defn.id.toString}'",
        )
      } else {
        List(
          q"""public static readonly BaboonDomainVersion = '${domain.version.v.toString}'
             |public baboonDomainVersion() {
             |    return $source.BaboonDomainVersion
             |}""".stripMargin,
          q"""public static readonly BaboonDomainIdentifier = '${defn.id.pkg.toString}'
             |public baboonDomainIdentifier() {
             |    return $source.BaboonDomainIdentifier
             |}""".stripMargin,
          q"""public static readonly BaboonTypeIdentifier = '${defn.id.toString}'
             |public baboonTypeIdentifier() {
             |    return $source.BaboonTypeIdentifier
             |}""".stripMargin,
        )
      }
    }
  }
}
