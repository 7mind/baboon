package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.translator.csharp.CSTypes.{csIReadOnlyList, csList, csString, csTpe}
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
    trans: CSTypeTranslator,
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
               |public $csTpe BaboonAdtType() => typeof(${trans.asCsType(id, domain, evo)});
               |""".stripMargin
          )
        case _ => List.empty
      }

      val version = domain.version

      val unmodifiedMethods = if (!isCodec) {
        val unmodifiedSince = evo.typesUnchangedSince(version)(defn.id)
        List(
          q"""public${propFix}static readonly $csIReadOnlyList<$csString> BaboonSameInVersionsValue = new $csList<$csString> { ${unmodifiedSince.sameIn
              .map(_.version).map(s => q"\"$s\"").toList.join(", ")} };
             |public$methodFix$csIReadOnlyList<$csString> BaboonSameInVersions() => BaboonSameInVersionsValue;
             |""".stripMargin
        )
      } else {
        List.empty
      }

      Seq(
        q"""public${propFix}static readonly $csString BaboonDomainVersionValue = "${version.version}";
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
      val csType = trans.asCsType(defn.id, domain, evo).fullyQualified

      val adtMethods = defn.id.owner match {
        case Owner.Adt(id) =>
          List(
            q"""public override $csString BaboonAdtTypeIdentifier() => $csType.BaboonAdtTypeIdentifierValue;
               |public override $csTpe BaboonAdtType() => typeof(${trans.asCsType(id, domain, evo)});
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
