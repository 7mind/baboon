package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.CompilerTarget.CSTarget
import io.septimalmind.baboon.typer.model.{BaboonEvolution, Domain}

trait CSFileTools {
  def basename(dom: Domain, evolution: BaboonEvolution): String
}

object CSFileTools {
  class CSFileToolsImpl(target: CSTarget) extends CSFileTools {
    def basename(dom: Domain, evolution: BaboonEvolution): String = {
      basename(
        dom,
        target.language.omitMostRecentVersionSuffixFromPaths && evolution.latest == dom.version,
      )
    }

    private def basename(dom: Domain, omitVersion: Boolean): String = {
      val base = dom.id.path.map(_.capitalize)
      val segments = if (omitVersion) {
        base
      } else {
        base ++ Seq(dom.version.v.toString)
      }

      segments.mkString("-")
    }

  }
}
