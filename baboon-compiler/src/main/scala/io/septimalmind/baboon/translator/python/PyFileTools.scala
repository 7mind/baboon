package io.septimalmind.baboon.translator.python

import io.septimalmind.baboon.CompilerTarget.PyTarget
import io.septimalmind.baboon.typer.model.{BaboonEvolution, Domain}

trait PyFileTools {
  def basename(dom: Domain, evolution: BaboonEvolution): String
  def definitionsBasePkg: List[String]
  def fixturesBasePkg: List[String]
  def testsBasePkg: List[String]
}

object PyFileTools {
  class ScFileToolsImpl(pyTarget: PyTarget) extends PyFileTools {
    override val definitionsBasePkg: List[String] = pyTarget.output.packageBases.definitions
    override val fixturesBasePkg: List[String]    = pyTarget.output.packageBases.fixtures
    override val testsBasePkg: List[String]       = pyTarget.output.packageBases.tests

    def basename(dom: Domain, evolution: BaboonEvolution): String = {
      basename(
        dom,
        evolution.latest == dom.version,
      )
    }

    private def basename(dom: Domain, omitVersion: Boolean): String = {
      val base     = dom.id.path.map(_.toLowerCase)
      val version  = dom.version.format(prefix = "v", delimiter = "_")
      val segments = if (omitVersion) base else base ++ Seq(version)
      segments.mkString("/")
    }
  }
}
