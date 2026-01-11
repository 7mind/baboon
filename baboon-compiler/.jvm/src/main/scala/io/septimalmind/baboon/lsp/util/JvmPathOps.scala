package io.septimalmind.baboon.lsp.util

import java.net.URI
import java.nio.file.Paths

/** JVM implementation of PathOps using java.nio.file and java.net.URI */
object JvmPathOps extends PathOps {

  override def pathToUri(path: String): String = {
    Paths.get(path).toUri.toString
  }

  override def uriToPath(uri: String): String = {
    new URI(uri).getPath
  }

  override def normalizePath(path: String): String = {
    Paths.get(path).toAbsolutePath.normalize().toString
  }
}
