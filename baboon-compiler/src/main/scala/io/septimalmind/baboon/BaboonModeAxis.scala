package io.septimalmind.baboon

import izumi.distage.model.definition.Axis

object BaboonModeAxis extends Axis {
  case object Lsp extends AxisChoiceDef
  case object Compiler extends AxisChoiceDef
  case object Explorer extends AxisChoiceDef
}
