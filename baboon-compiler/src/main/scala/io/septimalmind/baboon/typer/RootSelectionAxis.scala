package io.septimalmind.baboon.typer

import izumi.distage.model.definition.Axis

object RootSelectionAxis extends Axis {
  case object Default extends AxisChoiceDef
  case object Lsp extends AxisChoiceDef
}
