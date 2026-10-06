package io.septimalmind.baboon.scheme

import izumi.fundamentals.collections.nonempty.NEList

import io.septimalmind.baboon.util.StoredZipWriter

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}
import scala.util.Try

/** Writes a `:scheme --zip-output` archive (docs/cli-reference.md `:scheme`) with [[StoredZipWriter]]:
  * stored UTF-8 entries in path order, so identical inputs give identical bytes in any time zone.
  */
object SchemeZipWriter {
  def toBytes(entries: NEList[SchemeArchiveEntry]): Array[Byte] = {
    StoredZipWriter.write(entries.toList.sortBy(_.path).map(entry => entry.path -> entry.content.getBytes(StandardCharsets.UTF_8)))
  }

  /** Publishes `bytes` at `target` only once they are completely written: a sibling temporary file is
    * moved into place atomically, and removed when anything fails.
    */
  def writeAtomically(target: Path, bytes: Array[Byte]): Either[String, Path] = {
    val absolute = target.toAbsolutePath.normalize()
    Try {
      val parent = absolute.getParent
      Files.createDirectories(parent)
      val temporary = Files.createTempFile(parent, s".${absolute.getFileName}.", ".tmp")
      try {
        Files.write(temporary, bytes)
        Files.move(temporary, absolute, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
      } finally {
        val _ = Files.deleteIfExists(temporary)
      }
      absolute
    }.toEither.left.map(e => s"cannot write $absolute: $e")
  }
}
