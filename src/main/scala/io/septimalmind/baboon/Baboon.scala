package io.septimalmind.baboon
import caseapp.*
import izumi.fundamentals.platform.strings.IzString.*

import java.nio.file.Paths
import java.util.UUID
import scala.util.Random

case class Options(model: List[String], output: String)

object Baboon {
  def main(args: Array[String]): Unit = {

    CaseApp.parse[Options](args.toSeq) match {
      case Left(value) =>
        System.err.println(value.message)
        CaseApp.printHelp[Options]()
        System.exit(1)
      case Right(value) =>
        val opts = value._1
        val compiler = new BaboonCompiler.BaboonCompilerImpl()
        val inputModels = opts.model.map(s => Paths.get(s)).toSet
        val outDir = Paths.get(opts.output)
        compiler.run(inputModels, outDir) match {
          case Left(value) =>
            System.err.println("Compiler failed")
            System.err.println(value.toList.niceList())
          case Right(_) =>
            println("Done")
        }
    }
  }
}
