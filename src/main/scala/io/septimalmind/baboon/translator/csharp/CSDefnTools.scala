package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait CSDefnTools {
  def inNs(nss: Seq[String], tree: TextTree[CSValue]): TextTree[CSValue]

  def basename(dom: Domain): String

  def makeMeta(defn: DomainMember.User,
               version: Version): Seq[TextTree[CSValue]]
}

object CSDefnTools {
  class CSDefnToolsImpl extends CSDefnTools {
    def basename(dom: Domain): String = {
      (dom.id.path.map(_.capitalize) ++ Seq(dom.version.version))
        .mkString("-")
    }

    def makeMeta(defn: DomainMember.User,
                 version: Version): Seq[TextTree[CSValue]] = {
      Seq(
        q"""public static String BaboonDomainVersionValue = "${version.version}";
           |public String BaboonDomainVersion() => BaboonDomainVersionValue;
           |""".stripMargin,
        q"""public static String BaboonDomainIdentifierValue = "${defn.id.pkg.toString}";
           |public String BaboonDomainIdentifier() => BaboonDomainIdentifierValue;
           |""".stripMargin,
        q"""public static String BaboonTypeIdentifierValue = "${defn.id.toString}";
           |public String BaboonTypeIdentifier() => BaboonTypeIdentifierValue;
           |""".stripMargin)
    }

    private def inNs(name: String, tree: TextTree[CSValue]): TextTree[CSValue] = {
      q"""namespace ${name} {
         |    ${tree.shift(4).trim}
         |}""".stripMargin
    }

    def inNs(nss: Seq[String], tree: TextTree[CSValue]): TextTree[CSValue] = {
      nss.foldRight(tree) {
        case (ns, acc) =>
          inNs(ns, acc)
      }
    }
  }
}
