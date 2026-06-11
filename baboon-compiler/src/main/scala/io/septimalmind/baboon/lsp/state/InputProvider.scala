package io.septimalmind.baboon.lsp.state

import io.septimalmind.baboon.parser.BaboonParser

/** Platform-agnostic provider of baboon source files.
  *
  * JVM implementation scans the filesystem for .baboon files.
  * JS implementation receives inputs from the editor/environment.
  */
trait InputProvider {

  /** Get all .baboon inputs from configured workspace directories */
  def getWorkspaceInputs: Seq[BaboonParser.Input]

  /** Convert a file path to a URI string */
  def pathToUri(path: String): String
}
