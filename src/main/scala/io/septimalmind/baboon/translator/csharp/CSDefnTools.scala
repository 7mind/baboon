package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.BaboonCompiler.CompilerOptions
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait CSDefnTools {
  def inNs(nss: Seq[String], tree: TextTree[CSValue]): TextTree[CSValue]

  def basename(dom: Domain, evolution: BaboonEvolution, options: CompilerOptions): String = {
    basename(dom, options.omitMostRecentVersionSuffixFromPaths && evolution.latest == dom.version)
  }

  def basename(dom: Domain, omitVersion: Boolean): String

  def makeMeta(defn: DomainMember.User,
               version: Version): Seq[TextTree[CSValue]]
}

object CSDefnTools {
  class CSDefnToolsImpl extends CSDefnTools {
    def basename(dom: Domain, omitVersion: Boolean): String = {
      val base = dom.id.path.map(_.capitalize)
      val segments = if (omitVersion) {
        base
      } else {
        base ++ Seq(dom.version.version)
      }

      segments.mkString("-")
    }

    def makeMeta(defn: DomainMember.User,
                 version: Version): Seq[TextTree[CSValue]] = {
      Seq(
        q"""public const String BaboonDomainVersionValue = "${version.version}";
           |public String BaboonDomainVersion() => BaboonDomainVersionValue;
           |""".stripMargin,
        q"""public const String BaboonDomainIdentifierValue = "${defn.id.pkg.toString}";
           |public String BaboonDomainIdentifier() => BaboonDomainIdentifierValue;
           |""".stripMargin,
        q"""public const String BaboonTypeIdentifierValue = "${defn.id.toString}";
           |public String BaboonTypeIdentifier() => BaboonTypeIdentifierValue;
           |""".stripMargin)
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
