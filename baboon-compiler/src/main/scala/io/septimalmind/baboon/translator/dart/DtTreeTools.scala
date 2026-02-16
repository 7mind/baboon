package io.septimalmind.baboon.translator.dart

import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait DtTreeTools {
  def inLib(tree: TextTree[DtValue]): TextTree[DtValue]
}

object DtTreeTools {
  class DtTreeToolsImpl extends DtTreeTools {
    def inLib(tree: TextTree[DtValue]): TextTree[DtValue] = {
      tree
    }
  }
}
