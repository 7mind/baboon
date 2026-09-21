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
    valueField: TextTree[ScValue],
    refValueField: TextTree[ScValue],
  )

  object MetaField {
    def apply(signature: TextTree[ScValue], value: TextTree[ScValue], refValue: TextTree[ScValue]): MetaField =
      MetaField(q"$signature = $value", q"$signature = $refValue")

    def cached(name: String, tpe: TextTree[ScValue], value: TextTree[ScValue], refValue: TextTree[ScValue]): MetaField = {
      val backing = s"_$name"
      MetaField(
        q"""private lazy val $backing: $tpe = $value
           |def $name: $tpe = $backing""".stripMargin,
        q"def $name: $tpe = $refValue",
      )
    }
  }

  final class ScDomainTreeToolsImpl(
    domain: Domain,
    evolution: BaboonEvolution,
    domainTypes: ScDomainTypes,
  ) extends ScDomainTreeTools {
    override def makeDataMeta(defn: DomainMember.User): List[MetaField] = {
      mainMeta(defn) ++ sameInVersion(defn) ++ adtMeta(defn)
    }

    override def makeCodecMeta(defn: DomainMember.User): List[MetaField] = {
      mainMeta(defn) ++ adtMeta(defn)
    }

    private def mainMeta(defn: DomainMember.User): List[MetaField] = {
      val ref = domainTypes.asScType(defn.id)
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
          val adtRef = domainTypes.asScType(defn.id)
          val adtTypeIdentifier = MetaField(
            q"def baboonAdtTypeIdentifier: $scString",
            q"\"${id.toString}\"",
            q"$adtRef.baboonAdtTypeIdentifier",
          )
          val baboonAdtType = MetaField(
            q"def baboonAdtType: _root_.${javaClass.fullyQualified}[?]",
            q"${domainTypes.asScType(id)}.getClass",
            q"$adtRef.baboonAdtType",
          )
          List(adtTypeIdentifier, baboonAdtType)
        case _ => Nil
      }
    }

    private def sameInVersion(defn: DomainMember.User): List[MetaField] = {
      val ref             = domainTypes.asScType(defn.id)
      val unmodifiedSince = evolution.typesUnchangedSince(domain.version)(defn.id).sameIn.map(v => s"\"${v.v.toString}\"")
      val sameInVersion = MetaField.cached(
        "baboonSameInVersions",
        q"$scList[$scString]",
        q"$scList(${unmodifiedSince.mkString(", ")})",
        q"$ref.baboonSameInVersions",
      )
      val forward = evolution.typesForwardReadable(domain.version)(defn.id)
      val forwardEntries = forward.readable.toList.map {
        case (v, tier) => s""""${v.v.toString}" -> "${tier.wireName}""""
      }
      val forwardReadable = MetaField.cached(
        "baboonForwardReadable",
        q"${ScTypes.scMap}[$scString, $scString]",
        q"${ScTypes.scMap}(${forwardEntries.mkString(", ")})",
        q"$ref.baboonForwardReadable",
      )
      val minReaderEntries = evolution.minReaders(domain.version, defn.id).toList.sortBy(_._1.weight).map {
        case (tier, v) => s""""${tier.wireName}" -> "${v.v.toString}""""
      }
      val minReaders = MetaField.cached(
        "baboonMinReaderVersions",
        q"${ScTypes.scMap}[$scString, $scString]",
        q"${ScTypes.scMap}(${minReaderEntries.mkString(", ")})",
        q"$ref.baboonMinReaderVersions",
      )
      List(sameInVersion, forwardReadable, minReaders)
    }
  }
}
