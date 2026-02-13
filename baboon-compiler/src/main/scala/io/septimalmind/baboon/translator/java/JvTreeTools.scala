package io.septimalmind.baboon.translator.java

import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait JvTreeTools {
  def inPkg(nss: Seq[String], tree: TextTree[JvValue]): TextTree[JvValue]
}

object JvTreeTools {
  class JvTreeToolsImpl extends JvTreeTools {
    def inPkg(nss: Seq[String], tree: TextTree[JvValue]): TextTree[JvValue] = {
      if (nss.isEmpty) {
        tree
      } else {
        q"""package ${nss.mkString(".")};
           |
           |${tree.trim}""".stripMargin
      }
    }
  }
}
