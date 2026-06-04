package io.septimalmind.baboon.translator

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList

/** Per-language MCP server generator seam (T6 dispatch hook).
  *
  * Each per-language [[BaboonAbstractTranslator]] implementation holds an
  * instance of this trait and calls [[generateMcpServer]] when its
  * `generateMcpServer` language option is `true`. The returned [[Sources]] are
  * merged into the translator's normal output.
  *
  * Downstream generator tasks (T8/T10/T12-T18) replace [[McpServerGeneratorHookStub]]
  * with their language-specific implementations; they do NOT need to re-wire the
  * dispatch: the call site in the translator is already present.
  */
trait McpServerGeneratorHook[F[+_, +_]] {
  def generateMcpServer(family: BaboonFamily): F[NEList[BaboonIssue], Sources]
}

object McpServerGeneratorHook {

  /** No-op stub: emits nothing. Used for all languages until the real per-language
    * generator is implemented. With this stub in place, toggling `--<lang>-generate-mcp-server`
    * is accepted by the CLI but produces no additional output — the compiler
    * behaves identically to the pre-T6 baseline.
    */
  class McpServerGeneratorHookStub[F[+_, +_]: Error2] extends McpServerGeneratorHook[F] {
    override def generateMcpServer(family: BaboonFamily): F[NEList[BaboonIssue], Sources] =
      F.pure(Sources(Map.empty))
  }
}
