package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.translator.csharp.CSTypes.{csDictionary, csIReadOnlyDictionary, csIReadOnlyList, csList, csString, csTpe}
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait CSDomainTreeTools {
  def makeDataMeta(defn: DomainMember.User): Seq[TextTree[CSValue]]
  def makeCodecMeta(defn: DomainMember.User): Seq[TextTree[CSValue]]

  def metaMethodFlags(defn: DomainMember.User, isCodec: Boolean): String
}

object CSDomainTreeTools {
  class CSDomainTreeToolsImpl(
    domainTypes: CSDomainTypes,
    domain: Domain,
    evo: BaboonEvolution,
  ) extends CSDomainTreeTools {

    def metaMethodFlags(defn: DomainMember.User, isCodec: Boolean): String = {
      val isNested = defn.id.owner match {
        case Owner.Adt(_) => true
        case _            => false
      }
      val fix = if (!isCodec && isNested) {
        " new "
      } else {
        " "
      }

      fix
    }

    def makeDataMeta(defn: DomainMember.User): Seq[TextTree[CSValue]] = {
      makeFullMeta(defn, isCodec = false)
    }

    def makeCodecMeta(defn: DomainMember.User): Seq[TextTree[CSValue]] = {
      defn.defn match {
        case _: Typedef.Enum    => makeFullMeta(defn, isCodec = true)
        case _: Typedef.Foreign => makeFullMeta(defn, isCodec = true)
        case _                  => makeRefMeta(defn)
      }
    }

    private def makeFullMeta(defn: DomainMember.User, isCodec: Boolean): Seq[TextTree[CSValue.CSType]] = {
      val propFix   = metaMethodFlags(defn, isCodec)
      val methodFix = propFix + (if (isCodec) "override " else "")

      val adtMethods = defn.id.owner match {
        case Owner.Adt(id) =>
          List(
            q"""public const $csString BaboonAdtTypeIdentifierValue = "${id.toString}";
               |public $csString BaboonAdtTypeIdentifier() => BaboonAdtTypeIdentifierValue;
               |public ${csTpe.fullyQualified} BaboonAdtType() => typeof(${domainTypes.asCsType(id)});
               |""".stripMargin
          )
        case _ => List.empty
      }

      val version = domain.version

      val unmodifiedMethods = if (!isCodec) {
        val unmodifiedSince = evo.typesUnchangedSince(version)(defn.id)
        val forward         = evo.typesForwardReadable(version)(defn.id)
        val forwardEntries = forward.readable.toList
          .map { case (v, tier) => s"""{ "${v.v.toString}", "${tier.wireName}" }""" }
          .mkString(", ")
        val minReaderEntries = evo.minReaders(version, defn.id).toList.sortBy(_._1.weight)
          .map { case (tier, v) => s"""{ "${tier.wireName}", "${v.v.toString}" }""" }
          .mkString(", ")
        List(
          q"""public${propFix}static readonly $csIReadOnlyList<$csString> BaboonSameInVersionsValue = new $csList<$csString> { ${unmodifiedSince.sameIn
              .map(_.v.toString).map(s => q"\"$s\"").toList.join(", ")} };
             |public$methodFix$csIReadOnlyList<$csString> BaboonSameInVersions() => BaboonSameInVersionsValue;
             |""".stripMargin,
          q"""public${propFix}static readonly $csIReadOnlyDictionary<$csString, $csString> BaboonForwardReadableValue = new $csDictionary<$csString, $csString> { $forwardEntries };
             |public$methodFix$csIReadOnlyDictionary<$csString, $csString> BaboonForwardReadable() => BaboonForwardReadableValue;
             |""".stripMargin,
          q"""public${propFix}static readonly $csIReadOnlyDictionary<$csString, $csString> BaboonMinReaderVersionsValue = new $csDictionary<$csString, $csString> { $minReaderEntries };
             |public$methodFix$csIReadOnlyDictionary<$csString, $csString> BaboonMinReaderVersions() => BaboonMinReaderVersionsValue;
             |""".stripMargin,
        )
      } else {
        List.empty
      }

      Seq(
        q"""public${propFix}static readonly $csString BaboonDomainVersionValue = "${version.v.toString}";
           |public$methodFix$csString BaboonDomainVersion() => BaboonDomainVersionValue;
           |""".stripMargin,
        q"""public${propFix}static readonly $csString BaboonDomainIdentifierValue = "${defn.id.pkg.toString}";
           |public$methodFix$csString BaboonDomainIdentifier() => BaboonDomainIdentifierValue;
           |""".stripMargin,
        q"""public${propFix}static readonly $csString BaboonTypeIdentifierValue = "${defn.id.toString}";
           |public$methodFix$csString BaboonTypeIdentifier() => BaboonTypeIdentifierValue;
           |""".stripMargin,
      ) ++ unmodifiedMethods ++ adtMethods
    }

    private def makeRefMeta(defn: DomainMember.User): Seq[TextTree[CSValue.CSType]] = {
      val csType = domainTypes.asCsType(defn.id).fullyQualified

      val adtMethods = defn.id.owner match {
        case Owner.Adt(id) =>
          List(
            q"""public override $csString BaboonAdtTypeIdentifier() => $csType.BaboonAdtTypeIdentifierValue;
               |public override ${csTpe.fullyQualified} BaboonAdtType() => typeof(${domainTypes.asCsType(id)});
               |""".stripMargin
          )
        case _ => List.empty
      }

      Seq(
        q"""public override $csString BaboonDomainVersion() => $csType.BaboonDomainVersionValue;
           |""".stripMargin,
        q"""public override $csString BaboonDomainIdentifier() => $csType.BaboonDomainIdentifierValue;
           |""".stripMargin,
        q"""public override $csString BaboonTypeIdentifier() => $csType.BaboonTypeIdentifierValue;
           |""".stripMargin,
      ) ++ adtMethods
    }
  }
}
