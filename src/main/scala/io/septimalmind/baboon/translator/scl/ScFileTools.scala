package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.CompilerTarget.ScTarget
import io.septimalmind.baboon.typer.model.{BaboonEvolution, Domain}

trait ScFileTools {
  def basename(dom: Domain, evolution: BaboonEvolution): String
}

object ScFileTools {
  class ScFileToolsImpl(target: ScTarget) extends ScFileTools {
    def basename(dom: Domain, evolution: BaboonEvolution): String = {
      basename(
        dom,
        evolution.latest == dom.version,
      )
    }

    private def basename(dom: Domain, omitVersion: Boolean): String = {
      val base = dom.id.path.map(_.toLowerCase)
      val segments = if (omitVersion) {
        base
      } else {
        base ++ Seq(dom.version.version)
      }

      segments.mkString("-")
    }

  }
}
