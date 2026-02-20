package io.septimalmind.baboon.translator.swift

import io.septimalmind.baboon.translator.swift.SwDomainTreeTools.MetaField
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

trait SwDomainTreeTools {
  def makeDataMeta(defn: DomainMember.User): List[MetaField]
  def makeCodecMeta(defn: DomainMember.User): List[MetaField]
}

object SwDomainTreeTools {
  final case class MetaField(
    signature: TextTree[SwValue],
    value: TextTree[SwValue],
    refValue: TextTree[SwValue],
  ) {
    def valueField: TextTree[SwValue]    = q"$signature = $value"
    def refValueField: TextTree[SwValue] = q"$signature = $refValue"
  }

  final class SwDomainTreeToolsImpl(
    domain: Domain,
    evolution: BaboonEvolution,
    typeTranslator: SwTypeTranslator,
  ) extends SwDomainTreeTools {
    override def makeDataMeta(defn: DomainMember.User): List[MetaField] = {
      mainMeta(defn) ++ sameInVersion(defn) ++ adtMeta(defn)
    }

    override def makeCodecMeta(defn: DomainMember.User): List[MetaField] = {
      mainMeta(defn) ++ adtMeta(defn)
    }

    private def mainMeta(defn: DomainMember.User): List[MetaField] = {
      val ref = typeTranslator.asSwType(defn.id, domain, evolution).asName
      val baboonDomainVersion = MetaField(
        q"""static let baboonDomainVersion: String""",
        q""""${domain.version.v.toString}"""",
        q"$ref.baboonDomainVersion",
      )
      val baboonDomainIdentifier = MetaField(
        q"""static let baboonDomainIdentifier: String""",
        q""""${defn.id.pkg.toString}"""",
        q"$ref.baboonDomainIdentifier",
      )
      val baboonTypeIdentifier = MetaField(
        q"""static let baboonTypeIdentifier: String""",
        q""""${defn.id.toString}"""",
        q"$ref.baboonTypeIdentifier",
      )
      List(baboonDomainVersion, baboonDomainIdentifier, baboonTypeIdentifier)
    }

    private def adtMeta(defn: DomainMember.User): List[MetaField] = {
      defn.id.owner match {
        case Owner.Adt(id) =>
          val adtRef = typeTranslator.asSwType(defn.id, domain, evolution).asName
          val adtTypeIdentifier = MetaField(
            q"""static let baboonAdtTypeIdentifier: String""",
            q""""${id.toString}"""",
            q"$adtRef.baboonAdtTypeIdentifier",
          )
          List(adtTypeIdentifier)
        case _ => Nil
      }
    }

    private def sameInVersion(defn: DomainMember.User): List[MetaField] = {
      val ref             = typeTranslator.asSwType(defn.id, domain, evolution).asName
      val unmodifiedSince = evolution.typesUnchangedSince(domain.version)(defn.id).sameIn.map(v => s""""${v.v.toString}"""")
      val sameInVersion = MetaField(
        q"static let baboonSameInVersions: [String]",
        q"[${unmodifiedSince.mkString(", ")}]",
        q"$ref.baboonSameInVersions",
      )
      List(sameInVersion)
    }
  }
}
