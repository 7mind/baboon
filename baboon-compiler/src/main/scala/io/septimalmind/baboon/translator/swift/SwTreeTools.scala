package io.septimalmind.baboon.translator.swift

import io.septimalmind.baboon.typer.model.Owner
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

trait SwTreeTools {
  def inLib(tree: TextTree[SwValue], owner: Owner): TextTree[SwValue]
}

object SwTreeTools {
  class SwTreeToolsImpl(trans: SwTypeTranslator) extends SwTreeTools {
    def inLib(tree: TextTree[SwValue], owner: Owner): TextTree[SwValue] = {
      owner match {
        case Owner.Toplevel | _: Owner.Adt => tree
        case Owner.Ns(path) =>
          val nsPath = path.map(s => trans.escapeSwiftKeyword(s.name.toLowerCase)).mkString(".")
          q"""extension $nsPath {
             |${tree.shift(4).trim}
             |}""".stripMargin
      }
    }
  }
}
