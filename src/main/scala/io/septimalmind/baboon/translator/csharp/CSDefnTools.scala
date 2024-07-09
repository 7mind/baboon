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
        q"""public String BaboonDomainVersion()
           |{
           |    return "${version.version}";
           |}""".stripMargin,
        q"""public String BaboonDomainIdentifier() {
           |    return "${defn.id.pkg.toString}";
           |}""".stripMargin,
        q"""public String BaboonTypeIdentifier() {
           |    return "${defn.id.toString}";
           |}""".stripMargin)
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
