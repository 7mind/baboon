package io.septimalmind.baboon.git

import izumi.functional.bio.{Error2, F}

import java.nio.file.Path

/** Hand-written in-memory [[GitInvoker]] for tests — no subprocess, no jgit.
  *
  * Written by hand (not auto-generated) per the dual-tests discipline: it is a real, minimal
  * implementation of the same contract, configurable via [[GitInvokerDummy.Behavior]] to
  * reproduce the three states a consumer must handle:
  *
  *   - [[GitInvokerDummy.Behavior.GitMissing]] — no git binary; `isAvailable` is `false`, every
  *     failing op reports [[GitInvokeFailure.GitUnavailable]].
  *   - [[GitInvokerDummy.Behavior.RefUnresolvable]] — git present, but [[resolveRef]] fails with
  *     [[GitInvokeFailure.RefUnresolvable]].
  *   - [[GitInvokerDummy.Behavior.Success]] — git present and every op succeeds, returning the
  *     configured canned values.
  */
final class GitInvokerDummy[F[+_, +_]: Error2](behavior: GitInvokerDummy.Behavior) extends GitInvoker[F] {
  import GitInvokerDummy.Behavior

  override def isAvailable: F[Nothing, Boolean] = F.pure(behavior != Behavior.GitMissing)

  override def version: F[GitInvokeFailure, GitVersion] = behavior match {
    case Behavior.GitMissing => F.fail(GitInvokeFailure.GitUnavailable("dummy: git binary not present"))
    case _                   => F.pure(GitVersion("git version 0.0.0-dummy"))
  }

  override def repoRoot(fromDir: Path): F[GitInvokeFailure, Path] = whenAvailable {
    F.pure(behavior.repoRoot.getOrElse(fromDir))
  }

  override def resolveRef(root: Path, ref: String): F[GitInvokeFailure, GitCommit] = whenAvailable {
    behavior match {
      case Behavior.RefUnresolvable => F.fail(GitInvokeFailure.RefUnresolvable(root, ref, "dummy: unknown revision"))
      case Behavior.Success(_, sha) => F.pure(GitCommit(sha))
      case Behavior.GitMissing      => gitMissing // unreachable: guarded by whenAvailable
    }
  }

  override def addWorktree(root: Path, ref: String, dest: Path): F[GitInvokeFailure, Unit] = whenAvailable(F.unit)

  override def removeWorktree(root: Path, dest: Path): F[GitInvokeFailure, Unit] = whenAvailable(F.unit)

  private def whenAvailable[A](op: => F[GitInvokeFailure, A]): F[GitInvokeFailure, A] = behavior match {
    case Behavior.GitMissing => gitMissing
    case _                   => op
  }

  private def gitMissing[A]: F[GitInvokeFailure, A] =
    F.fail(GitInvokeFailure.GitUnavailable("dummy: git binary not present"))
}

object GitInvokerDummy {

  /** Configures the state a [[GitInvokerDummy]] reproduces. */
  sealed trait Behavior {

    /** Repo root returned by [[GitInvoker.repoRoot]] in the non-missing states; `None` echoes the
      * caller's `fromDir`. */
    def repoRoot: Option[Path] = this match {
      case Behavior.Success(root, _) => Some(root)
      case _                         => None
    }
  }
  object Behavior {

    /** No git binary present. */
    case object GitMissing extends Behavior

    /** Git present, but ref resolution always fails. */
    case object RefUnresolvable extends Behavior

    /** Git present and all ops succeed, returning `repoRoot` and `resolvedSha` as canned values. */
    final case class Success(root: Path, resolvedSha: String) extends Behavior
  }

  /** Convenience: an always-succeeding dummy over the given canned values. */
  def success[F[+_, +_]: Error2](root: Path, resolvedSha: String): GitInvokerDummy[F] =
    new GitInvokerDummy[F](Behavior.Success(root, resolvedSha))
}
