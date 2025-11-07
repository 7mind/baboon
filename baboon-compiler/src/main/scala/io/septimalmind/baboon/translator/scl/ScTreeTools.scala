package io.septimalmind.baboon.translator.scl

import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait ScTreeTools {
  def inNs(nss: Seq[String], tree: TextTree[ScValue]): TextTree[ScValue]
}

object ScTreeTools {
  class ScTreeToolsImpl() extends ScTreeTools {
    def inNs(nss: Seq[String], tree: TextTree[ScValue]): TextTree[ScValue] = {
      if (nss.isEmpty) {
        tree
      } else {
        q"""package ${nss.mkString(".")} {
           |  ${tree.shift(2).trim}
           |}""".stripMargin
      }
    }
  }
}
