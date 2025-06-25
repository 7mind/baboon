package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.CompilerTarget.CSTarget
import io.septimalmind.baboon.translator.csharp.CSTypes.csString
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
    target: CSTarget,
    domain: Domain,
    evo: BaboonEvolution,
  ) extends CSDomainTreeTools {

    def metaMethodFlags(defn: DomainMember.User, isCodec: Boolean): String = {
      val isNested = defn.id.owner match {
        case Owner.Adt(_) => true
        case _            => false
      }
      val fix = if (target.language.useCompactAdtForm && !isCodec && isNested) {
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
      val fix = metaMethodFlags(defn, isCodec = false)

      val adtMethods = defn.id.owner match {
        case Owner.Adt(id) =>
          List(
            q"""public const $csString BaboonAdtTypeIdentifierValue = "${id.toString}";
               |public $csString BaboonAdtTypeIdentifier() => BaboonAdtTypeIdentifierValue;
               |""".stripMargin
          )
        case _ => List.empty
      }

      val version         = domain.version
      val unmodifiedSince = evo.typesUnchangedSince(version)(defn.id)

      Seq(
        q"""public${fix}const $csString BaboonDomainVersionValue = "${version.version}";
           |public$fix$csString BaboonDomainVersion() => BaboonDomainVersionValue;
           |""".stripMargin,
        q"""public${fix}const $csString BaboonUnmodifiedSinceVersionValue = "${unmodifiedSince.version}";
           |public$fix$csString BaboonUnmodifiedSinceVersion() => BaboonUnmodifiedSinceVersionValue;
           |""".stripMargin,
        q"""public${fix}const $csString BaboonDomainIdentifierValue = "${defn.id.pkg.toString}";
           |public$fix$csString BaboonDomainIdentifier() => BaboonDomainIdentifierValue;
           |""".stripMargin,
        q"""public${fix}const $csString BaboonTypeIdentifierValue = "${defn.id.toString}";
           |public$fix$csString BaboonTypeIdentifier() => BaboonTypeIdentifierValue;
           |""".stripMargin,
      ) ++ adtMethods
    }

    private def makeRefMeta(defn: DomainMember.User): Seq[TextTree[CSValue.CSType]] = {
      val csType = trans.asCsType(defn.id, domain, evo)

      val adtMethods = defn.id.owner match {
        case Owner.Adt(_) =>
          List(
            q"""public $csString BaboonAdtTypeIdentifier() => $csType.BaboonAdtTypeIdentifierValue;
               |""".stripMargin
          )
        case _ => List.empty
      }

      Seq(
        q"""public $csString BaboonDomainVersion() => $csType.BaboonDomainVersionValue;
           |""".stripMargin,
        q"""public $csString BaboonUnmodifiedSinceVersion() => $csType.BaboonUnmodifiedSinceVersionValue;
           |""".stripMargin,
        q"""public $csString BaboonDomainIdentifier() => $csType.BaboonDomainIdentifierValue;
           |""".stripMargin,
        q"""public $csString BaboonTypeIdentifier() => $csType.BaboonTypeIdentifierValue;
           |""".stripMargin,
      ) ++ adtMethods
    }
  }
}
