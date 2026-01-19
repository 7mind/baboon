package io.septimalmind.baboon.util

import io.septimalmind.baboon.PathTools.FsPathExt
import io.septimalmind.baboon.parser.model.FSPath
import izumi.fundamentals.platform.files.IzFiles

final class JvmFileContentProvider extends FileContentProvider {
  override def read(path: FSPath): Option[String] = {
    val file = path.toFile
    if (file.exists() && file.isFile) {
      Some(IzFiles.readString(file))
    } else {
      None
    }
  }
}
