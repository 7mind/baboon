package io.septimalmind.baboon.translator.dart

import io.septimalmind.baboon.translator.dart.DtDomainTreeTools.MetaField
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

trait DtDomainTreeTools {
  def makeDataMeta(defn: DomainMember.User): List[MetaField]
  def makeCodecMeta(defn: DomainMember.User): List[MetaField]
}

object DtDomainTreeTools {
  final case class MetaField(
    signature: TextTree[DtValue],
    value: TextTree[DtValue],
    refValue: TextTree[DtValue],
  ) {
    def valueField: TextTree[DtValue]    = q"$signature = $value;"
    def refValueField: TextTree[DtValue] = q"$signature = $refValue;"
  }

  final class DtDomainTreeToolsImpl(
    domain: Domain,
    evolution: BaboonEvolution,
    typeTranslator: DtTypeTranslator,
  ) extends DtDomainTreeTools {
    override def makeDataMeta(defn: DomainMember.User): List[MetaField] = {
      mainMeta(defn) ++ sameInVersion(defn) ++ adtMeta(defn)
    }

    override def makeCodecMeta(defn: DomainMember.User): List[MetaField] = {
      mainMeta(defn) ++ adtMeta(defn)
    }

    private def mainMeta(defn: DomainMember.User): List[MetaField] = {
      val ref = typeTranslator.asDtType(defn.id, domain, evolution).asName
      val baboonDomainVersion = MetaField(
        q"static const String baboonDomainVersion",
        q"'${domain.version.v.toString}'",
        q"$ref.baboonDomainVersion",
      )
      val baboonDomainIdentifier = MetaField(
        q"static const String baboonDomainIdentifier",
        q"'${defn.id.pkg.toString}'",
        q"$ref.baboonDomainIdentifier",
      )
      val baboonTypeIdentifier = MetaField(
        q"static const String baboonTypeIdentifier",
        q"'${defn.id.toString}'",
        q"$ref.baboonTypeIdentifier",
      )
      List(baboonDomainVersion, baboonDomainIdentifier, baboonTypeIdentifier)
    }

    private def adtMeta(defn: DomainMember.User): List[MetaField] = {
      defn.id.owner match {
        case Owner.Adt(id) =>
          val adtRef = typeTranslator.asDtType(defn.id, domain, evolution).asName
          val adtTypeIdentifier = MetaField(
            q"static const String baboonAdtTypeIdentifier",
            q"'${id.toString}'",
            q"$adtRef.baboonAdtTypeIdentifier",
          )
          List(adtTypeIdentifier)
        case _ => Nil
      }
    }

    private def sameInVersion(defn: DomainMember.User): List[MetaField] = {
      val ref             = typeTranslator.asDtType(defn.id, domain, evolution).asName
      val unmodifiedSince = evolution.typesUnchangedSince(domain.version)(defn.id).sameIn.map(v => s"'${v.v.toString}'")
      val sameInVersion = MetaField(
        q"static const List<String> baboonSameInVersions",
        q"[${unmodifiedSince.mkString(", ")}]",
        q"$ref.baboonSameInVersions",
      )
      List(sameInVersion)
    }
  }
}
