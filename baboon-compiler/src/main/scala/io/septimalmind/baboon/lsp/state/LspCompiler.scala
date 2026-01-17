package io.septimalmind.baboon.lsp.state

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.fundamentals.collections.nonempty.NEList

/** Platform-agnostic compiler interface for LSP.
  *
  * Takes pre-loaded inputs (path + content pairs) and compiles them.
  * This avoids filesystem dependencies in the compilation pipeline.
  */
trait LspCompiler {
  def reload(inputs: Seq[BaboonParser.Input]): Either[NEList[BaboonIssue], BaboonFamily]
}
