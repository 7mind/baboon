package io.septimalmind.baboon.translator.python

import io.septimalmind.baboon.CompilerTarget.PyTarget
import io.septimalmind.baboon.typer.model.{BaboonEvolution, Domain}
import izumi.fundamentals.collections.nonempty.NEString

trait PyFileTools {
  def basename(dom: Domain, evolution: BaboonEvolution): String
  def definitionsBasePkg: List[String]
  def fixturesBasePkg: List[String]
  def testsBasePkg: List[String]
}

object PyFileTools {
  class ScFileToolsImpl(pyTarget: PyTarget) extends PyFileTools {
    private val (
      definitionsBase,
      fixturesBase,
      testsBase,
    ) = collectPackagesBases

    override val definitionsBasePkg: List[String] = definitionsBase.map(_.mkString)
    override val fixturesBasePkg: List[String]    = fixturesBase.map(_.mkString)
    override val testsBasePkg: List[String]       = testsBase.map(_.mkString)

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

    private def collectPackagesBases: (List[NEString], List[NEString], List[NEString]) = {
      val basePaths = List(
        pyTarget.output.output.segments.toList,
        pyTarget.output.fixturesOutput.map(_.segments).getOrElse(Nil),
        pyTarget.output.testsOutput.map(_.segments).getOrElse(Nil),
      )

      val longestCommonPrefix = basePaths.reduceLeft {
        (currentPrefix, nextSequence) =>
          currentPrefix
            .zip(nextSequence)
            .takeWhile { case (a, b) => a == b }
            .map(_._1)
      }

      if (longestCommonPrefix.nonEmpty) {
        (
          pyTarget.output.output.segments.drop(longestCommonPrefix.size).toList,
          pyTarget.output.fixturesOutput.map(_.segments).getOrElse(Nil).drop(longestCommonPrefix.size).toList,
          pyTarget.output.testsOutput.map(_.segments).getOrElse(Nil).drop(longestCommonPrefix.size).toList,
        )
      } else (List(pyTarget.output.output.name), Nil, Nil)
    }
  }
}
