package io.septimalmind.baboon.lsp.state

import io.septimalmind.baboon.lsp.util.PathOps
import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import izumi.fundamentals.collections.nonempty.NEString
import izumi.fundamentals.platform.files.IzFiles

import java.nio.file.{Files, Path}

/** JVM implementation of InputProvider that scans the filesystem for .baboon files */
class JvmInputProvider(
  modelDirs: Set[Path],
  pathOps: PathOps
) extends InputProvider {

  override def getWorkspaceInputs: Seq[BaboonParser.Input] = {
    modelDirs.toSeq.flatMap { folder =>
      if (Files.exists(folder)) {
        IzFiles.walk(folder.toFile)
          .filter(_.toFile.getName.endsWith(".baboon"))
          .flatMap { file =>
            val path = file.toAbsolutePath
            scala.util.Try {
              val content = IzFiles.readString(file.toFile)
              BaboonParser.Input(
                FSPath.parse(NEString.unsafeFrom(path.toString)),
                content
              )
            }.toOption
          }
      } else {
        Seq.empty
      }
    }
  }

  override def pathToUri(path: String): String = pathOps.pathToUri(path)

  override def uriToPath(uri: String): String = pathOps.uriToPath(uri)
}
