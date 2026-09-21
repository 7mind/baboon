package io.septimalmind.baboon.translator.java

import io.septimalmind.baboon.translator.java.JvDomainTreeTools.MetaField
import io.septimalmind.baboon.translator.java.JvTypes.{javaClass, jvList, jvMap, jvString}
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote
import io.septimalmind.baboon.translator.DomainEvolution

trait JvDomainTreeTools {
  def makeDataMeta(defn: DomainMember.User): List[MetaField]
  def makeCodecMeta(defn: DomainMember.User): List[MetaField]
}

object JvDomainTreeTools {
  final case class MetaField(
    signature: TextTree[JvValue],
    value: TextTree[JvValue],
    refValue: TextTree[JvValue],
  ) {
    def valueField: TextTree[JvValue]    = q"$signature = $value;"
    def refValueField: TextTree[JvValue] = q"$signature = $refValue;"
  }

  final class JvDomainTreeToolsImpl(
    domain: Domain,
    domainEvolution: DomainEvolution,
    domainTypes: JvDomainTypes,
  ) extends JvDomainTreeTools {
    override def makeDataMeta(defn: DomainMember.User): List[MetaField] = {
      mainMeta(defn) ++ sameInVersion(defn) ++ adtMeta(defn)
    }

    override def makeCodecMeta(defn: DomainMember.User): List[MetaField] = {
      mainMeta(defn) ++ adtMeta(defn)
    }

    private def mainMeta(defn: DomainMember.User): List[MetaField] = {
      val ref = domainTypes.asJvType(defn.id).fullyQualified
      val baboonDomainVersion = MetaField(
        q"public static final $jvString baboonDomainVersion",
        q"\"${domain.version.v.toString}\"",
        q"$ref.baboonDomainVersion",
      )
      val baboonDomainIdentifier = MetaField(
        q"public static final $jvString baboonDomainIdentifier",
        q"\"${defn.id.pkg.toString}\"",
        q"$ref.baboonDomainIdentifier",
      )
      val baboonTypeIdentifier = MetaField(
        q"public static final $jvString baboonTypeIdentifier",
        q"\"${defn.id.toString}\"",
        q"$ref.baboonTypeIdentifier",
      )
      List(baboonDomainVersion, baboonDomainIdentifier, baboonTypeIdentifier)
    }

    private def adtMeta(defn: DomainMember.User): List[MetaField] = {
      defn.id.owner match {
        case Owner.Adt(id) =>
          val adtRef = domainTypes.asJvType(defn.id).fullyQualified
          val adtTypeIdentifier = MetaField(
            q"public static final $jvString baboonAdtTypeIdentifier",
            q"\"${id.toString}\"",
            q"$adtRef.baboonAdtTypeIdentifier",
          )
          val baboonAdtType = MetaField(
            q"public static final ${javaClass.fullyQualified}<?> baboonAdtType",
            q"${domainTypes.asJvType(id)}.class",
            q"$adtRef.baboonAdtType",
          )
          List(adtTypeIdentifier, baboonAdtType)
        case _ => Nil
      }
    }

    private def sameInVersion(defn: DomainMember.User): List[MetaField] = {
      val ref             = domainTypes.asJvType(defn.id).fullyQualified
      val unmodifiedSince = domainEvolution.typesUnchangedSince(defn.id).sameIn.map(v => s"\"${v.v.toString}\"")
      val sameInVersion = MetaField(
        q"public static final $jvList<$jvString> baboonSameInVersions",
        q"$jvList.of(${unmodifiedSince.mkString(", ")})",
        q"$ref.baboonSameInVersions",
      )
      val forward = domainEvolution.typesForwardReadable(defn.id)
      val forwardEntries = forward.readable.toList.map {
        case (v, tier) => s"""java.util.Map.entry("${v.v.toString}", "${tier.wireName}")"""
      }
      val forwardReadable = MetaField(
        q"public static final $jvMap<$jvString, $jvString> baboonForwardReadable",
        q"$jvMap.ofEntries(${forwardEntries.mkString(", ")})",
        q"$ref.baboonForwardReadable",
      )
      val minReaderEntries = domainEvolution.minReaders(defn.id).toList.sortBy(_._1.weight).map {
        case (tier, v) => s"""java.util.Map.entry("${tier.wireName}", "${v.v.toString}")"""
      }
      val minReaders = MetaField(
        q"public static final $jvMap<$jvString, $jvString> baboonMinReaderVersions",
        q"$jvMap.ofEntries(${minReaderEntries.mkString(", ")})",
        q"$ref.baboonMinReaderVersions",
      )
      List(sameInVersion, forwardReadable, minReaders)
    }
  }
}
