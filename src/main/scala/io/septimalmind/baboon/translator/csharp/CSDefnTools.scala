package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.BaboonCompiler.CompilerOptions
import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator.csString
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait CSDefnTools {
  def inNs(nss: Seq[String], tree: TextTree[CSValue]): TextTree[CSValue]

  def basename(dom: Domain,
               evolution: BaboonEvolution,
               options: CompilerOptions): String = {
    basename(
      dom,
      options.omitMostRecentVersionSuffixFromPaths && evolution.latest == dom.version
    )
  }

  def basename(dom: Domain, omitVersion: Boolean): String

  def makeMeta(defn: DomainMember.User,
               version: Version,
               isCodec: Boolean): Seq[TextTree[CSValue]]

  def makeFix(defn: DomainMember.User, isCodec: Boolean): String
}

object CSDefnTools {
  class CSDefnToolsImpl(options: CompilerOptions) extends CSDefnTools {
    def basename(dom: Domain, omitVersion: Boolean): String = {
      val base = dom.id.path.map(_.capitalize)
      val segments = if (omitVersion) {
        base
      } else {
        base ++ Seq(dom.version.version)
      }

      segments.mkString("-")
    }

    def makeFix(defn: DomainMember.User, isCodec: Boolean): String = {
      val isNested = defn.id.owner match {
        case Owner.Toplevel => false
        case Owner.Adt(_)   => true
      }
      val fix = if (options.csUseCompactAdtForm && !isCodec && isNested) {
        " new "
      } else {
        " "
      }
      fix
    }

    def makeMeta(defn: DomainMember.User,
                 version: Version,
                 isCodec: Boolean): Seq[TextTree[CSValue]] = {
      val fix = makeFix(defn, isCodec)

      val adtMethods = defn.id.owner match {
        case Owner.Toplevel => List.empty
        case Owner.Adt(id) =>
          List(
            q"""public const String BaboonAdtTypeIdentifierValue = "${id.toString}";
               |public $csString BaboonAdtTypeIdentifier() => BaboonAdtTypeIdentifierValue;
               |""".stripMargin,
          )
      }

      Seq(
        q"""public${fix}const String BaboonDomainVersionValue = "${version.version}";
           |public${fix}String BaboonDomainVersion() => BaboonDomainVersionValue;
           |""".stripMargin,
        q"""public${fix}const String BaboonDomainIdentifierValue = "${defn.id.pkg.toString}";
           |public${fix}String BaboonDomainIdentifier() => BaboonDomainIdentifierValue;
           |""".stripMargin,
        q"""public${fix}const String BaboonTypeIdentifierValue = "${defn.id.toString}";
           |public${fix}String BaboonTypeIdentifier() => BaboonTypeIdentifierValue;
           |""".stripMargin
      ) ++ adtMethods
    }

    def inNs(nss: Seq[String], tree: TextTree[CSValue]): TextTree[CSValue] = {
      if (nss.isEmpty) {
        tree
      } else {
        q"""namespace ${nss.mkString(".")} {
           |    ${tree.shift(4).trim}
           |}""".stripMargin
      }
    }
  }
}
