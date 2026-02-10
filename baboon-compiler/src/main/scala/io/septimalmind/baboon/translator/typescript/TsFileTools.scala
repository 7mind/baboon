package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.typer.model.{BaboonEvolution, Domain}

trait TsFileTools {
  def basename(dom: Domain, evolution: BaboonEvolution): String
}

object TsFileTools {
  class TsFileToolsImpl extends TsFileTools {
    def basename(dom: Domain, evolution: BaboonEvolution): String = {
      basename(dom, evolution.latest == dom.version)
    }

    private def basename(dom: Domain, omitVersion: Boolean): String = {
      val base = dom.id.path.map(_.toLowerCase)
      val segments =
        if (omitVersion) base
        else base ++ Seq("v" + dom.version.v.toString.replace('.', '_'))
      segments.mkString("/")
    }
  }
}
