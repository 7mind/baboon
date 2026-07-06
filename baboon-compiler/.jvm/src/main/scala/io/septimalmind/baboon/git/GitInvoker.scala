package io.septimalmind.baboon.git

import java.nio.file.Path

/** Abstraction seam over the `git` binary.
  *
  * Every git access in the compiler flows through this single interface so it can be
  * exercised against a hand-written in-memory dummy in tests and a real ProcessBuilder
  * shell-out in production. No jgit / Java-git library is embedded (hard constraint): the
  * only production implementation, [[GitInvokerImpl]], shells out to the system `git`.
  *
  * The trait is intentionally standalone — it references only [[java.nio.file.Path]] and its
  * own result types ([[GitVersion]], [[GitInvokeFailure]]). It carries NO knowledge of any
  * consumer's domain (e.g. a `:diff` command), so it can be reused verbatim.
  *
  * All operations return `F[GitInvokeFailure, A]`: a nonzero exit code, a spawn exception, or
  * an unresolvable ref surfaces as a TYPED failure in the error channel rather than a thrown
  * exception or a swallowed error.
  */
trait GitInvoker[F[+_, +_]] {

  /** Whether a usable `git` binary is present on the PATH (`command -v git` semantics via a
    * successful `git --version`). Never fails: absence is reported as `false`, not an error. */
  def isAvailable: F[Nothing, Boolean]

  /** The version reported by `git --version`. Fails with [[GitInvokeFailure.GitUnavailable]]
    * when no git binary can be executed. */
  def version: F[GitInvokeFailure, GitVersion]

  /** The absolute repository root containing `fromDir`, via `git -C <fromDir> rev-parse
    * --show-toplevel`. Fails when `fromDir` is not inside a git working tree. */
  def repoRoot(fromDir: Path): F[GitInvokeFailure, Path]

  /** Resolve `ref` to a concrete commit inside `root`, via
    * `git -C <root> rev-parse --verify <ref>^{commit}`. Fails with
    * [[GitInvokeFailure.RefUnresolvable]] when the ref does not name a commit. */
  def resolveRef(root: Path, ref: String): F[GitInvokeFailure, GitCommit]

  /** Add a detached worktree at `dest` checked out at `ref`, via
    * `git -C <root> worktree add --detach <dest> <ref>`. */
  def addWorktree(root: Path, ref: String, dest: Path): F[GitInvokeFailure, Unit]

  /** Remove the worktree at `dest`, via `git -C <root> worktree remove <dest>`. */
  def removeWorktree(root: Path, dest: Path): F[GitInvokeFailure, Unit]
}

/** The version string reported by `git --version` (e.g. `git version 2.43.0`). */
final case class GitVersion(raw: String)

/** A resolved commit object name (full 40-hex SHA-1, or the SHA `git rev-parse` returns). */
final case class GitCommit(sha: String)

/** The typed failures a [[GitInvoker]] operation can report.
  *
  * Standalone by design — no consumer domain types leak in. Each case captures enough context
  * (the command, exit code, and captured stderr/stdout) to produce an actionable diagnostic at
  * the call site.
  */
sealed trait GitInvokeFailure
object GitInvokeFailure {

  /** No usable `git` binary could be executed (spawn failed, or `git --version` did not
    * return exit 0). */
  final case class GitUnavailable(reason: String) extends GitInvokeFailure

  /** The ref could not be resolved to a commit (e.g. unknown ref / not a commit-ish). */
  final case class RefUnresolvable(root: Path, ref: String, stderr: String) extends GitInvokeFailure

  /** A git subprocess exited with a nonzero status. Carries the executed argv, the exit code
    * and the captured stdout/stderr for diagnosis. */
  final case class NonZeroExit(command: Seq[String], exitCode: Int, stdout: String, stderr: String)
      extends GitInvokeFailure

  /** Spawning the git subprocess threw (e.g. binary missing, permission denied, interrupted). */
  final case class ProcessFailed(command: Seq[String], reason: String) extends GitInvokeFailure
}
