package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.CompilerTarget.TsTarget
import io.septimalmind.baboon.typer.model.{BaboonEvolution, Domain}

trait TsFileTools {
  def basename(dom: Domain, evolution: BaboonEvolution): String
  def definitionsBasePkg: List[String]
  def fixturesBasePkg: List[String]
  def testBasePkg: List[String]
}

object TsFileTools {
  final class TsFileToolsImpl(tsTarget: TsTarget) extends TsFileTools {
    override val definitionsBasePkg: List[String] = tsTarget.output.packageBases.definitions
    override val fixturesBasePkg: List[String]    = tsTarget.output.packageBases.fixtures
    override val testBasePkg: List[String]        = tsTarget.output.packageBases.tests

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
