package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.CompilerTarget.TsTarget
import io.septimalmind.baboon.typer.model.{BaboonEvolution, Domain}
import izumi.fundamentals.collections.nonempty.NEString

trait TsFileTools {
  def basename(dom: Domain, evolution: BaboonEvolution): String
  def definitionsBasePkg: List[String]
  def fixturesBasePkg: List[String]
  def testBasePkg: List[String]
}

object TsFileTools {
  final class TsFileToolsImpl(tsTarget: TsTarget) extends TsFileTools {
    private val (
      definitionsBase,
      fixturesBase,
      testsBase,
    ) = collectPackagesBases

    override val definitionsBasePkg: List[String] = definitionsBase.map(_.mkString)
    override val fixturesBasePkg: List[String]    = fixturesBase.map(_.mkString)
    override val testBasePkg: List[String]        = testsBase.map(_.mkString)

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

    private def getBasePath: List[String] = {
      val basePaths = List(
        tsTarget.output.output.segments.toList,
        tsTarget.output.fixturesOutput.map(_.segments).getOrElse(Nil),
        tsTarget.output.testsOutput.map(_.segments).getOrElse(Nil),
      )

      val longestCommonPrefix = basePaths.reduceLeft {
        (currentPrefix, nextSequence) =>
          currentPrefix
            .zip(nextSequence)
            .takeWhile { case (a, b) => a == b }
            .map(_._1)
      }

      longestCommonPrefix.map(_.mkString).toList
    }

    private def collectPackagesBases: (List[NEString], List[NEString], List[NEString]) = {
      val basePath = getBasePath

      if (basePath.nonEmpty) {
        (
          tsTarget.output.output.segments.drop(basePath.size).toList,
          tsTarget.output.fixturesOutput.map(_.segments).getOrElse(Nil).drop(basePath.size).toList,
          tsTarget.output.testsOutput.map(_.segments).getOrElse(Nil).drop(basePath.size).toList,
        )
      } else (List(tsTarget.output.output.name), Nil, Nil)
    }
  }
}
