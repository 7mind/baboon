package io.septimalmind.baboon.translator.csharp

import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait CSTreeTools {
  def inNs(nss: Seq[String], tree: TextTree[CSValue]): TextTree[CSValue]
}

object CSTreeTools {
  class CSTreeToolsImpl() extends CSTreeTools {
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
