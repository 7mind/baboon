package io.septimalmind.baboon.translator.kotlin

import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait KtTreeTools {
  def inPkg(nss: Seq[String], tree: TextTree[KtValue]): TextTree[KtValue]
}

object KtTreeTools {
  class KtTreeToolsImpl extends KtTreeTools {
    def inPkg(nss: Seq[String], tree: TextTree[KtValue]): TextTree[KtValue] = {
      if (nss.isEmpty) {
        tree
      } else {
        q"""package ${nss.mkString(".")}
           |
           |${tree.trim}""".stripMargin
      }
    }
  }
}
