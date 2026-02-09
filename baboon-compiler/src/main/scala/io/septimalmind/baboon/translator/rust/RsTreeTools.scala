package io.septimalmind.baboon.translator.rust

import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait RsTreeTools {
  def inMod(modPath: Seq[String], tree: TextTree[RsValue]): TextTree[RsValue]
}

object RsTreeTools {
  class RsTreeToolsImpl extends RsTreeTools {
    def inMod(modPath: Seq[String], tree: TextTree[RsValue]): TextTree[RsValue] = {
      // In Rust, we don't wrap in nested modules in the same file.
      // The module hierarchy is determined by the file path.
      // So we just return the tree as-is.
      tree
    }
  }
}
