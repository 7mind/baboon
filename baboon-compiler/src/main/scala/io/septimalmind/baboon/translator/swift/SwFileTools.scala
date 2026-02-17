package io.septimalmind.baboon.translator.swift

import io.septimalmind.baboon.typer.model.{BaboonEvolution, Domain}

trait SwFileTools {
  def basename(dom: Domain, evolution: BaboonEvolution): String
}

object SwFileTools {
  class SwFileToolsImpl extends SwFileTools {
    def basename(dom: Domain, evolution: BaboonEvolution): String = {
      basename(
        dom,
        evolution.latest == dom.version,
      )
    }

    private def basename(dom: Domain, omitVersion: Boolean): String = {
      val base = dom.id.path.map(_.toLowerCase)
      val segments =
        if (omitVersion) base
        else base ++ Seq(dom.version.v.toString)
      segments.mkString("/")
    }
  }
}
