package io.septimalmind.baboon.lsp.state

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.BaboonFamily

case class CompilationResult(
  family: Option[BaboonFamily],
  issues: Seq[BaboonIssue],
  fileIssues: Map[String, Seq[BaboonIssue]]
)

object CompilationResult {
  def empty: CompilationResult = CompilationResult(None, Seq.empty, Map.empty)
}
