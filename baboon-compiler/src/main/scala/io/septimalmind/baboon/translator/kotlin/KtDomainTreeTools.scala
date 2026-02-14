package io.septimalmind.baboon.translator.kotlin

import io.septimalmind.baboon.translator.kotlin.KtDomainTreeTools.MetaField
import io.septimalmind.baboon.translator.kotlin.KtTypes.{javaClass, ktList, ktString}
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

trait KtDomainTreeTools {
  def makeDataMeta(defn: DomainMember.User): List[MetaField]
  def makeCodecMeta(defn: DomainMember.User): List[MetaField]
}

object KtDomainTreeTools {
  final case class MetaField(
    signature: TextTree[KtValue],
    value: TextTree[KtValue],
    refValue: TextTree[KtValue],
  ) {
    def valueField: TextTree[KtValue]    = q"$signature = $value"
    def refValueField: TextTree[KtValue] = q"$signature = $refValue"
  }

  final class KtDomainTreeToolsImpl(
    domain: Domain,
    evolution: BaboonEvolution,
    typeTranslator: KtTypeTranslator,
  ) extends KtDomainTreeTools {
    override def makeDataMeta(defn: DomainMember.User): List[MetaField] = {
      mainMeta(defn) ++ sameInVersion(defn) ++ adtMeta(defn)
    }

    override def makeCodecMeta(defn: DomainMember.User): List[MetaField] = {
      mainMeta(defn).map(f => f.copy(signature = q"override ${f.signature}")) ++ adtMeta(defn)
    }

    private def mainMeta(defn: DomainMember.User): List[MetaField] = {
      val ref = typeTranslator.asKtType(defn.id, domain, evolution).fullyQualified
      val baboonDomainVersion = MetaField(
        q"val baboonDomainVersion: $ktString",
        q"\"${domain.version.v.toString}\"",
        q"$ref.baboonDomainVersion",
      )
      val baboonDomainIdentifier = MetaField(
        q"val baboonDomainIdentifier: $ktString",
        q"\"${defn.id.pkg.toString}\"",
        q"$ref.baboonDomainIdentifier",
      )
      val baboonTypeIdentifier = MetaField(
        q"val baboonTypeIdentifier: $ktString",
        q"\"${defn.id.toString}\"",
        q"$ref.baboonTypeIdentifier",
      )
      List(baboonDomainVersion, baboonDomainIdentifier, baboonTypeIdentifier)
    }

    private def adtMeta(defn: DomainMember.User): List[MetaField] = {
      defn.id.owner match {
        case Owner.Adt(id) =>
          val adtRef = typeTranslator.asKtType(defn.id, domain, evolution).fullyQualified
          val adtTypeIdentifier = MetaField(
            q"val baboonAdtTypeIdentifier: $ktString",
            q"\"${id.toString}\"",
            q"$adtRef.baboonAdtTypeIdentifier",
          )
          val baboonAdtType = MetaField(
            q"val baboonAdtType: $javaClass<*>",
            q"${typeTranslator.asKtType(id, domain, evolution)}::class.java",
            q"$adtRef.baboonAdtType",
          )
          List(adtTypeIdentifier, baboonAdtType)
        case _ => Nil
      }
    }

    private def sameInVersion(defn: DomainMember.User): List[MetaField] = {
      val ref             = typeTranslator.asKtType(defn.id, domain, evolution).fullyQualified
      val unmodifiedSince = evolution.typesUnchangedSince(domain.version)(defn.id).sameIn.map(v => s"\"${v.v.toString}\"")
      val sameInVersion = MetaField(
        q"val baboonSameInVersions: $ktList<$ktString>",
        q"listOf(${unmodifiedSince.mkString(", ")})",
        q"$ref.baboonSameInVersions",
      )
      List(sameInVersion)
    }
  }
}
