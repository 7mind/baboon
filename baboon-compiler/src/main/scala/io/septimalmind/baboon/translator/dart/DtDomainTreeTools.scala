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
    name: String,
    typeAnn: TextTree[DtValue],
    isCodecData: Boolean,
  ) {
    def valueField: TextTree[DtValue]     = q"$signature = $value;"
    def refValueField: TextTree[DtValue]  = q"$signature = $refValue;"
    def valueGetter: TextTree[DtValue]    = q"@override\n$typeAnn get $name => $value;"
    def refValueGetter: TextTree[DtValue] = q"@override\n$typeAnn get $name => $refValue;"
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

    // MFACADE-PR-F: dart static metadata fields use a `Const` suffix so they don't
    // collide with the same-named instance getters required by BaboonMetaProvider.
    // Dart forbids a class from declaring static and instance members of the same name;
    // this rename keeps both addressable. The `name` field below stays as the
    // BaboonMetaProvider-required instance name; signature/value/refValue use the
    // `Const`-suffixed static identifier.
    private def mainMeta(defn: DomainMember.User): List[MetaField] = {
      val ref = typeTranslator.asDtType(defn.id, domain, evolution).asName
      val baboonDomainVersion = MetaField(
        q"static const String baboonDomainVersionConst",
        q"'${domain.version.v.toString}'",
        q"$ref.baboonDomainVersionConst",
        "baboonDomainVersion",
        q"String",
        isCodecData = true,
      )
      val baboonDomainIdentifier = MetaField(
        q"static const String baboonDomainIdentifierConst",
        q"'${defn.id.pkg.toString}'",
        q"$ref.baboonDomainIdentifierConst",
        "baboonDomainIdentifier",
        q"String",
        isCodecData = true,
      )
      val baboonTypeIdentifier = MetaField(
        q"static const String baboonTypeIdentifierConst",
        q"'${defn.id.toString}'",
        q"$ref.baboonTypeIdentifierConst",
        "baboonTypeIdentifier",
        q"String",
        isCodecData = true,
      )
      List(baboonDomainVersion, baboonDomainIdentifier, baboonTypeIdentifier)
    }

    private def adtMeta(defn: DomainMember.User): List[MetaField] = {
      defn.id.owner match {
        case Owner.Adt(id) =>
          val adtRef = typeTranslator.asDtType(defn.id, domain, evolution).asName
          val adtTypeIdentifier = MetaField(
            q"static const String baboonAdtTypeIdentifierConst",
            q"'${id.toString}'",
            q"$adtRef.baboonAdtTypeIdentifierConst",
            "baboonAdtTypeIdentifier",
            q"String",
            isCodecData = false,
          )
          List(adtTypeIdentifier)
        case _ => Nil
      }
    }

    private def sameInVersion(defn: DomainMember.User): List[MetaField] = {
      val ref             = typeTranslator.asDtType(defn.id, domain, evolution).asName
      val unmodifiedSince = evolution.typesUnchangedSince(domain.version)(defn.id).sameIn.map(v => s"'${v.v.toString}'")
      val sameInVersion = MetaField(
        q"static const List<String> baboonSameInVersionsConst",
        q"[${unmodifiedSince.mkString(", ")}]",
        q"$ref.baboonSameInVersionsConst",
        "baboonSameInVersions",
        q"List<String>",
        isCodecData = false,
      )
      List(sameInVersion)
    }
  }
}
