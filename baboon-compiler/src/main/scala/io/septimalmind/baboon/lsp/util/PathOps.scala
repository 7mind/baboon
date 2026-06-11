package io.septimalmind.baboon.lsp.util

import scala.util.Try

/** Platform-agnostic path/URI operations.
  *
  * JVM implementation uses java.nio.file.Paths and java.net.URI.
  * JS implementation would use appropriate Node.js or browser APIs.
  */
trait PathOps {

  /** Convert a file system path to a URI string (file:// scheme) */
  def pathToUri(path: String): String

  /** Convert a URI string to a file system path */
  def uriToPath(uri: String): String

  /** Convert a URI string to a file system path, returning None for malformed or non-file URIs.
    *
    * Implementations should catch URISyntaxException (malformed URI),
    * IllegalArgumentException (non-file scheme), and FileSystemNotFoundException
    * at a minimum. This is the safe variant for use at client-supplied URI boundaries.
    */
  def uriToPathSafe(uri: String): Option[String] = Try(uriToPath(uri)).toOption

  /** Normalize a path (resolve . and .., make absolute) */
  def normalizePath(path: String): String

  /** Compare two paths for equality after normalization */
  def pathsEqual(a: String, b: String): Boolean = {
    normalizePath(a) == normalizePath(b)
  }
}
