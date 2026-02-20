package io.septimalmind.baboon.translator.swift

import izumi.fundamentals.platform.strings.TextTree

trait SwTreeTools {
  def inLib(tree: TextTree[SwValue]): TextTree[SwValue]
}

object SwTreeTools {
  class SwTreeToolsImpl extends SwTreeTools {
    def inLib(tree: TextTree[SwValue]): TextTree[SwValue] = {
      tree
    }
  }
}
