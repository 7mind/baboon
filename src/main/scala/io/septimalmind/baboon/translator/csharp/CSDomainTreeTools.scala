package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.CompilerOptions
import io.septimalmind.baboon.translator.csharp.CSTypes.csString
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait CSDomainTreeTools {
  def makeMeta(defn: DomainMember.User, isCodec: Boolean): Seq[TextTree[CSValue]]

  def metaMethodFlags(defn: DomainMember.User, isCodec: Boolean): String
}

object CSDomainTreeTools {
  class CSDomainTreeToolsImpl(options: CompilerOptions, domain: Domain, evo: BaboonEvolution) extends CSDomainTreeTools {

    def metaMethodFlags(defn: DomainMember.User, isCodec: Boolean): String = {
      val isNested = defn.id.owner match {
        case Owner.Adt(_) => true
        case _            => false
      }
      val fix = if (options.csOptions.useCompactAdtForm && !isCodec && isNested) {
        " new "
      } else {
        " "
      }

      fix
    }

    def makeMeta(defn: DomainMember.User, isCodec: Boolean): Seq[TextTree[CSValue]] = {
      val fix = metaMethodFlags(defn, isCodec)

      val adtMethods = defn.id.owner match {
        case Owner.Adt(id) =>
          List(
            q"""public const String BaboonAdtTypeIdentifierValue = "${id.toString}";
               |public $csString BaboonAdtTypeIdentifier() => BaboonAdtTypeIdentifierValue;
               |""".stripMargin
          )
        case _ => List.empty
      }

      val version         = domain.version
      val unmodifiedSince = evo.typesUnchangedSince(version)(defn.id)

      Seq(
        q"""public${fix}const String BaboonDomainVersionValue = "${version.version}";
           |public${fix}String BaboonDomainVersion() => BaboonDomainVersionValue;
           |""".stripMargin,
        q"""public${fix}const String BaboonUnmodifiedSinceVersionValue = "${unmodifiedSince.version}";
           |public${fix}String BaboonUnmodifiedSinceVersion() => BaboonUnmodifiedSinceVersionValue;
           |""".stripMargin,
        q"""public${fix}const String BaboonDomainIdentifierValue = "${defn.id.pkg.toString}";
           |public${fix}String BaboonDomainIdentifier() => BaboonDomainIdentifierValue;
           |""".stripMargin,
        q"""public${fix}const String BaboonTypeIdentifierValue = "${defn.id.toString}";
           |public${fix}String BaboonTypeIdentifier() => BaboonTypeIdentifierValue;
           |""".stripMargin,
      ) ++ adtMethods
    }
  }
}
