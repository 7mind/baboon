package io.septimalmind.baboon.translator

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.fundamentals.collections.nonempty.NEList

/** Per-language MCP server generator seam (T6 dispatch hook).
  *
  * Each per-language [[BaboonAbstractTranslator]] implementation holds an
  * instance of this trait and calls [[generateMcpServer]] when its
  * `generateMcpServer` language option is `true`. The returned [[Sources]] are
  * merged into the translator's normal output.
  */
trait McpServerGeneratorHook[F[+_, +_]] {
  def generateMcpServer(family: BaboonFamily): F[NEList[BaboonIssue], Sources]
}
