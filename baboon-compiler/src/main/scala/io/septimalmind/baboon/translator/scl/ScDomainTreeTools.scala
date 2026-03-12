package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.translator.scl.ScDomainTreeTools.MetaField
import io.septimalmind.baboon.translator.scl.ScTypes.{javaClass, scList, scString}
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

trait ScDomainTreeTools {
  def makeDataMeta(defn: DomainMember.User): List[MetaField]
  def makeCodecMeta(defn: DomainMember.User): List[MetaField]
}

object ScDomainTreeTools {
  final case class MetaField(
    signature: TextTree[ScValue],
    value: TextTree[ScValue],
    refValue: TextTree[ScValue],
  ) {
    def valueField: TextTree[ScValue]    = q"$signature = $value"
    def refValueField: TextTree[ScValue] = q"$signature = $refValue"
  }

  final class ScDomainTreeToolsImpl(
    domain: Domain,
    evolution: BaboonEvolution,
    typeTranslator: ScTypeTranslator,
  ) extends ScDomainTreeTools {
    override def makeDataMeta(defn: DomainMember.User): List[MetaField] = {
      mainMeta(defn) ++ sameInVersion(defn) ++ adtMeta(defn)
    }

    override def makeCodecMeta(defn: DomainMember.User): List[MetaField] = {
      mainMeta(defn) ++ adtMeta(defn)
    }

    private def mainMeta(defn: DomainMember.User): List[MetaField] = {
      val ref = typeTranslator.asScType(defn.id, domain, evolution)
      val baboonDomainVersion = MetaField(
        q"def baboonDomainVersion: $scString",
        q"\"${domain.version.v.toString}\"",
        q"$ref.baboonDomainVersion",
      )
      val baboonDomainIdentifier = MetaField(
        q"def baboonDomainIdentifier: $scString",
        q"\"${defn.id.pkg.toString}\"",
        q"$ref.baboonDomainIdentifier",
      )
      val baboonTypeIdentifier = MetaField(
        q"def baboonTypeIdentifier: $scString",
        q"\"${defn.id.toString}\"",
        q"$ref.baboonTypeIdentifier",
      )
      List(baboonDomainVersion, baboonDomainIdentifier, baboonTypeIdentifier)
    }

    private def adtMeta(defn: DomainMember.User): List[MetaField] = {
      defn.id.owner match {
        case Owner.Adt(id) =>
          val adtRef = typeTranslator.asScType(defn.id, domain, evolution)
          val adtTypeIdentifier = MetaField(
            q"def baboonAdtTypeIdentifier: $scString",
            q"\"${id.toString}\"",
            q"$adtRef.baboonAdtTypeIdentifier",
          )
          val baboonAdtType = MetaField(
            q"def baboonAdtType: $javaClass[?]",
            q"${typeTranslator.asScType(id, domain, evolution)}.getClass",
            q"$adtRef.baboonAdtType",
          )
          List(adtTypeIdentifier, baboonAdtType)
        case _ => Nil
      }
    }

    private def sameInVersion(defn: DomainMember.User): List[MetaField] = {
      val ref             = typeTranslator.asScType(defn.id, domain, evolution)
      val unmodifiedSince = evolution.typesUnchangedSince(domain.version)(defn.id).sameIn.map(v => s"\"${v.v.toString}\"")
      val sameInVersion = MetaField(
        q"def baboonSameInVersions: $scList[$scString]",
        q"$scList(${unmodifiedSince.mkString(", ")})",
        q"$ref.baboonSameInVersions",
      )
      List(sameInVersion)
    }
  }
}
