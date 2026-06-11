package io.septimalmind.baboon.lsp.util

import java.net.{URI, URISyntaxException}
import java.nio.file.{FileSystemNotFoundException, Paths}

/** JVM implementation of PathOps using java.nio.file and java.net.URI */
object JvmPathOps extends PathOps {

  override def pathToUri(path: String): String = {
    Paths.get(path).toUri.toString
  }

  override def uriToPath(uri: String): String = {
    Paths.get(new URI(uri)).toString
  }

  /** Returns None for malformed URIs (URISyntaxException), non-file schemes
    * (IllegalArgumentException, FileSystemNotFoundException), and any other
    * unexpected failure. Logs nothing — callers are responsible for logging.
    */
  override def uriToPathSafe(uri: String): Option[String] = {
    try {
      Some(Paths.get(new URI(uri)).toString)
    } catch {
      case _: URISyntaxException          => None
      case _: IllegalArgumentException    => None
      case _: FileSystemNotFoundException => None
    }
  }

  override def normalizePath(path: String): String = {
    Paths.get(path).toAbsolutePath.normalize().toString
  }
}
