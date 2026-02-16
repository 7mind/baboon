package io.septimalmind.baboon.translator.java

import io.septimalmind.baboon.translator.java.JvDomainTreeTools.MetaField
import io.septimalmind.baboon.translator.java.JvTypes.{javaClass, jvList, jvString}
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

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
    evolution: BaboonEvolution,
    typeTranslator: JvTypeTranslator,
  ) extends JvDomainTreeTools {
    override def makeDataMeta(defn: DomainMember.User): List[MetaField] = {
      mainMeta(defn) ++ sameInVersion(defn) ++ adtMeta(defn)
    }

    override def makeCodecMeta(defn: DomainMember.User): List[MetaField] = {
      mainMeta(defn) ++ adtMeta(defn)
    }

    private def mainMeta(defn: DomainMember.User): List[MetaField] = {
      val ref = typeTranslator.asJvType(defn.id, domain, evolution).fullyQualified
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
          val adtRef = typeTranslator.asJvType(defn.id, domain, evolution).fullyQualified
          val adtTypeIdentifier = MetaField(
            q"public static final $jvString baboonAdtTypeIdentifier",
            q"\"${id.toString}\"",
            q"$adtRef.baboonAdtTypeIdentifier",
          )
          val baboonAdtType = MetaField(
            q"public static final $javaClass<?> baboonAdtType",
            q"${typeTranslator.asJvType(id, domain, evolution)}.class",
            q"$adtRef.baboonAdtType",
          )
          List(adtTypeIdentifier, baboonAdtType)
        case _ => Nil
      }
    }

    private def sameInVersion(defn: DomainMember.User): List[MetaField] = {
      val ref             = typeTranslator.asJvType(defn.id, domain, evolution).fullyQualified
      val unmodifiedSince = evolution.typesUnchangedSince(domain.version)(defn.id).sameIn.map(v => s"\"${v.v.toString}\"")
      val sameInVersion = MetaField(
        q"public static final $jvList<$jvString> baboonSameInVersions",
        q"$jvList.of(${unmodifiedSince.mkString(", ")})",
        q"$ref.baboonSameInVersions",
      )
      List(sameInVersion)
    }
  }
}
