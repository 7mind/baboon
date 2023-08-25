package io.septimalmind.baboon
import caseapp.*
import io.septimalmind.baboon.BaboonCompiler.CompilerOptions
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.{
  IzArtifact,
  IzArtifactMaterializer
}
import izumi.fundamentals.platform.strings.IzString.*

import java.nio.file.Paths

case class Options(model: List[String],
                   modelDir: List[String],
                   output: String,
                   debug: Option[Boolean],
                   csObsoleteErrors: Option[Boolean],
)

object Baboon {
  def main(args: Array[String]): Unit = {
    val artifact = implicitly[IzArtifactMaterializer]
    println(s"Baboon ${artifact.get.shortInfo}")

    CaseApp.parse[Options](args.toSeq) match {
      case Left(value) =>
        System.err.println(value.message)
        CaseApp.printHelp[Options]()
        System.exit(1)
      case Right(value) =>
        val opts = value._1
        val compiler = new BaboonCompiler.BaboonCompilerImpl()
        val inputModels = opts.model
          .map(s => Paths.get(s))
          .toSet ++ opts.modelDir.flatMap { dir =>
          IzFiles
            .walk(Paths.get(dir).toFile)
            .filter(_.toFile.getName.endsWith(".baboon"))
        }
        val outDir = Paths.get(opts.output)
        println(
          s"Inputs: ${inputModels.map(_.toFile.getCanonicalPath).toList.sorted.niceList()}"
        )
        println(s"Target: ${outDir.toFile.getCanonicalPath}")
        compiler.run(
          inputModels,
          outDir,
          CompilerOptions(
            opts.debug.getOrElse(false),
            opts.csObsoleteErrors.getOrElse(false)
          )
        ) match {
          case Left(value) =>
            System.err.println("Compiler failed")
            System.err.println(value.toList.niceList())
          case Right(_) =>
            println("Done")
        }
    }
  }
}
