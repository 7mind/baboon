package io.septimalmind.baboon.translator.swift

import io.septimalmind.baboon.translator.swift.SwDomainTreeTools.MetaField
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote
import io.septimalmind.baboon.translator.DomainEvolution

trait SwDomainTreeTools {
  def makeDataMeta(defn: DomainMember.User): List[MetaField]
  def makeCodecMeta(defn: DomainMember.User): List[MetaField]
}

object SwDomainTreeTools {
  final case class MetaField(
    signature: TextTree[SwValue],
    value: TextTree[SwValue],
    refValue: TextTree[SwValue],
    name: String,
    typeAnn: String,
  ) {
    def valueField: TextTree[SwValue]    = q"$signature = $value"
    def refValueField: TextTree[SwValue] = q"$signature = $refValue"
    /** Instance computed property that forwards to the static metadata. Used to satisfy
      * `BaboonMetaProvider` conformance without relying on a protocol-level default
      * extension (which would require static reqs that hand-written test stubs don't have).
      */
    def instanceForwarder: TextTree[SwValue] =
      q"public var $name: $typeAnn { Self.$name }"
  }

  final class SwDomainTreeToolsImpl(
    domain: Domain,
    domainEvolution: DomainEvolution,
    domainTypes: SwDomainTypes,
  ) extends SwDomainTreeTools {
    override def makeDataMeta(defn: DomainMember.User): List[MetaField] = {
      mainMeta(defn) ++ sameInVersion(defn) ++ adtMeta(defn)
    }

    override def makeCodecMeta(defn: DomainMember.User): List[MetaField] = {
      mainMeta(defn) ++ adtMeta(defn)
    }

    private def mainMeta(defn: DomainMember.User): List[MetaField] = {
      val ref = domainTypes.asSwType(defn.id).asDeclName
      val baboonDomainVersion = MetaField(
        q"""public static let baboonDomainVersion: String""",
        q""""${domain.version.v.toString}"""",
        q"$ref.baboonDomainVersion",
        "baboonDomainVersion",
        "String",
      )
      val baboonDomainIdentifier = MetaField(
        q"""public static let baboonDomainIdentifier: String""",
        q""""${defn.id.pkg.toString}"""",
        q"$ref.baboonDomainIdentifier",
        "baboonDomainIdentifier",
        "String",
      )
      val baboonTypeIdentifier = MetaField(
        q"""public static let baboonTypeIdentifier: String""",
        q""""${defn.id.toString}"""",
        q"$ref.baboonTypeIdentifier",
        "baboonTypeIdentifier",
        "String",
      )
      List(baboonDomainVersion, baboonDomainIdentifier, baboonTypeIdentifier)
    }

    private def adtMeta(defn: DomainMember.User): List[MetaField] = {
      defn.id.owner match {
        case Owner.Adt(id) =>
          val adtRef = domainTypes.asSwType(defn.id).asDeclName
          val adtTypeIdentifier = MetaField(
            q"""public static let baboonAdtTypeIdentifier: String""",
            q""""${id.toString}"""",
            q"$adtRef.baboonAdtTypeIdentifier",
            "baboonAdtTypeIdentifier",
            "String",
          )
          List(adtTypeIdentifier)
        case _ => Nil
      }
    }

    private def sameInVersion(defn: DomainMember.User): List[MetaField] = {
      val ref             = domainTypes.asSwType(defn.id).asDeclName
      val unmodifiedSince = domainEvolution.typesUnchangedSince(defn.id).sameIn.map(v => s""""${v.v.toString}"""")
      val sameInVersion = MetaField(
        q"public static let baboonSameInVersions: [String]",
        q"[${unmodifiedSince.mkString(", ")}]",
        q"$ref.baboonSameInVersions",
        "baboonSameInVersions",
        "[String]",
      )
      val forward = domainEvolution.typesForwardReadable(defn.id)
      val forwardEntries = forward.readable.toList.map {
        case (v, tier) => s""""${v.v.toString}": "${tier.wireName}""""
      }
      val forwardReadable = MetaField(
        q"public static let baboonForwardReadable: [String: String]",
        q"[${forwardEntries.mkString(", ")}]",
        q"$ref.baboonForwardReadable",
        "baboonForwardReadable",
        "[String: String]",
      )
      val minReaderEntries = domainEvolution.minReaders(defn.id).toList.sortBy(_._1.weight).map {
        case (tier, v) => s""""${tier.wireName}": "${v.v.toString}""""
      }
      val minReaders = MetaField(
        q"public static let baboonMinReaderVersions: [String: String]",
        q"[${minReaderEntries.mkString(", ")}]",
        q"$ref.baboonMinReaderVersions",
        "baboonMinReaderVersions",
        "[String: String]",
      )
      List(sameInVersion, forwardReadable, minReaders)
    }
  }
}
