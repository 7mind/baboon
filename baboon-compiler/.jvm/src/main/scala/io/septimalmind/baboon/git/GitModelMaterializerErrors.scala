package io.septimalmind.baboon.git

import io.septimalmind.baboon.git.GitModelMaterializer.MaterializeFailure

/** The impure edge of [[GitModelMaterializer]]: renders a [[MaterializeFailure]] to `System.err`
  * and terminates the process with a DISTINCT nonzero exit code per taxonomy case (Q60).
  *
  * Exit codes 4/5/6 are already taken by the `:diff` seam (loader / not-found / comparison), so
  * this uses fresh codes 7..12, each distinct per failure mode, consistent with the diff error
  * style (`System.err.println` then `sys.exit`).
  *
  * Kept SEPARATE from the pure component so the taxonomy stays unit-testable via the
  * [[GitInvokerDummy]] without a `System.exit` side effect (T197). Only the CLI seam calls
  * [[reportAndExit]]. */
object GitModelMaterializerErrors {

  /** git binary absent. */
  final val ExitGitMissing = 7

  /** anchor dir is not inside a git working tree. */
  final val ExitNotAGitRepo = 8

  /** ref does not resolve to a commit. */
  final val ExitRefUnresolvable = 9

  /** an absolute `--model-dir`/`--model` is not under the repo root. */
  final val ExitPathOutsideRepo = 10

  /** no `.baboon` under the re-rooted inputs at the ref. */
  final val ExitPathAbsentAtRef = 11

  /** worktree add / remove / cleanup failure. */
  final val ExitWorktreeOp = 12

  /** Render `failure` to `System.err` (all lines) and return its distinct exit code WITHOUT
    * exiting — the pure half, so callers/tests can assert on the mapping. */
  def render(failure: MaterializeFailure): Int = failure match {
    case MaterializeFailure.GitBinaryMissing =>
      System.err.println("git binary not found on PATH: pinning models at a ref requires a working `git`.")
      ExitGitMissing

    case MaterializeFailure.NotAGitRepo(anchor, detail) =>
      System.err.println(s"not a git repository: $anchor is not inside a git working tree.")
      System.err.println(s"  $detail")
      ExitNotAGitRepo

    case MaterializeFailure.RefUnresolvable(ref, detail) =>
      System.err.println(s"git ref could not be resolved: '$ref'")
      System.err.println(s"  $detail")
      ExitRefUnresolvable

    case MaterializeFailure.PathOutsideRepo(input, repoRoot) =>
      System.err.println(s"--model-dir/--model path is outside the repository root, so it cannot be pinned at a ref:")
      System.err.println(s"  path:      $input")
      System.err.println(s"  repo root: $repoRoot")
      ExitPathOutsideRepo

    case MaterializeFailure.PathAbsentAtRef(ref, paths) =>
      System.err.println(s"no .baboon models found at ref '$ref' under: $paths")
      ExitPathAbsentAtRef

    case MaterializeFailure.WorktreeAddFailed(worktree, ref, detail) =>
      System.err.println(s"failed to create a detached worktree at '$worktree' for ref '$ref':")
      System.err.println(s"  $detail")
      ExitWorktreeOp

    case MaterializeFailure.WorktreeCleanupFailed(worktree, detail) =>
      System.err.println(s"failed to remove the temporary worktree at '$worktree' (cleanup surfaced, not swallowed):")
      System.err.println(s"  $detail")
      ExitWorktreeOp

    case MaterializeFailure.MultipleFailures(primary, cleanup) =>
      // Report the primary failure first (it determines the exit code), then the cleanup failure
      // so neither is lost.
      val code = render(primary)
      System.err.println("additionally, worktree cleanup failed:")
      render(cleanup)
      code
  }

  /** Render `failure` and terminate the process with its distinct exit code. The single call site
    * is the CLI seam. */
  def reportAndExit(failure: MaterializeFailure): Nothing = {
    val code = render(failure)
    sys.exit(code)
  }
}
