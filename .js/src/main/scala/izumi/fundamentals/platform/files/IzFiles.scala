package izumi.fundamentals.platform.files

import java.io.File
import java.nio.file.Path
import scala.util.Try

object IzFiles {
  def readString(file: File): String = {
    throw new UnsupportedOperationException("File I/O not supported in JavaScript environment")
  }

  def readString(path: Path): String = {
    throw new UnsupportedOperationException("File I/O not supported in JavaScript environment")
  }

  def writeUtfString(path: Path, content: String): Unit = {
    throw new UnsupportedOperationException("File I/O not supported in JavaScript environment")
  }

  def walk(path: Path): Seq[Path] = {
    throw new UnsupportedOperationException("File I/O not supported in JavaScript environment")
  }

  def erase(path: Path): Unit = {
    throw new UnsupportedOperationException("File I/O not supported in JavaScript environment")
  }
}
