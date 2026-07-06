package io.septimalmind.baboon.git

import io.septimalmind.baboon.git.GitInvokerImpl.ProcResult
import izumi.functional.bio.unsafe.MaybeSuspend2
import izumi.functional.bio.{Error2, F}

import java.io.InputStream
import java.nio.charset.StandardCharsets
import java.nio.file.Path

/** Production [[GitInvoker]] that shells out to the system `git` binary via
  * [[java.lang.ProcessBuilder]].
  *
  * NO jgit / `org.eclipse.jgit` is used or imported — git access is exclusively subprocess
  * invocation of the `git` executable.
  *
  * Every subprocess run captures stdout, stderr and the exit code; a nonzero exit or a spawn
  * exception is mapped to a typed [[GitInvokeFailure]] (never thrown, never swallowed).
  */
class GitInvokerImpl[F[+_, +_]: Error2: MaybeSuspend2] extends GitInvoker[F] {

  private val gitBinary = "git"

  override def isAvailable: F[Nothing, Boolean] = {
    run(Seq(gitBinary, "--version")).map(_.exitCode == 0).catchAll(_ => F.pure(false))
  }

  override def version: F[GitInvokeFailure, GitVersion] = {
    for {
      res <- run(Seq(gitBinary, "--version"))
      out <- res.exitCode match {
        case 0 => F.pure(GitVersion(res.stdout.trim))
        case _ => F.fail(GitInvokeFailure.GitUnavailable(s"`git --version` exited with ${res.exitCode}: ${res.stderr.trim}"))
      }
    } yield out
  }.catchAll {
    case f: GitInvokeFailure.ProcessFailed => F.fail(GitInvokeFailure.GitUnavailable(f.reason))
    case other                             => F.fail(other)
  }

  override def repoRoot(fromDir: Path): F[GitInvokeFailure, Path] = {
    for {
      res  <- run(Seq(gitBinary, "-C", fromDir.toString, "rev-parse", "--show-toplevel"))
      root <- ensureZero(res)
    } yield Path.of(root.stdout.trim)
  }

  override def resolveRef(root: Path, ref: String): F[GitInvokeFailure, GitCommit] = {
    for {
      res <- run(Seq(gitBinary, "-C", root.toString, "rev-parse", "--verify", s"$ref^{commit}"))
      commit <- res.exitCode match {
        case 0 => F.pure(GitCommit(res.stdout.trim))
        case _ => F.fail(GitInvokeFailure.RefUnresolvable(root, ref, res.stderr.trim))
      }
    } yield commit
  }

  override def addWorktree(root: Path, ref: String, dest: Path): F[GitInvokeFailure, Unit] = {
    for {
      res <- run(Seq(gitBinary, "-C", root.toString, "worktree", "add", "--detach", dest.toString, ref))
      _   <- ensureZero(res)
    } yield ()
  }

  override def removeWorktree(root: Path, dest: Path): F[GitInvokeFailure, Unit] = {
    for {
      res <- run(Seq(gitBinary, "-C", root.toString, "worktree", "remove", dest.toString))
      _   <- ensureZero(res)
    } yield ()
  }

  private def ensureZero(res: ProcResult): F[GitInvokeFailure, ProcResult] = {
    res.exitCode match {
      case 0 => F.pure(res)
      case _ => F.fail(GitInvokeFailure.NonZeroExit(res.command, res.exitCode, res.stdout.trim, res.stderr.trim))
    }
  }

  /** Spawn `command`, drain stdout+stderr fully (draining both prevents a pipe-buffer deadlock
    * on large output), await termination, and return the captured [[ProcResult]]. A thrown
    * spawn exception is mapped to [[GitInvokeFailure.ProcessFailed]]. */
  private def run(command: Seq[String]): F[GitInvokeFailure, ProcResult] = {
    F.maybeSuspend {
      val proc   = new ProcessBuilder(command: _*).start()
      // Read stdout and stderr concurrently: reading one to completion before the other can
      // deadlock when the unread stream fills its OS pipe buffer.
      val outT   = readAsync(proc.getInputStream)
      val errT   = readAsync(proc.getErrorStream)
      val code   = proc.waitFor()
      val stdout = outT.get()
      val stderr = errT.get()
      ProcResult(command, code, stdout, stderr)
    }.catchAll { (t: Throwable) =>
      F.fail(GitInvokeFailure.ProcessFailed(command, s"${t.getClass.getName}: ${t.getMessage}"))
    }
  }

  private def readAsync(is: InputStream): java.util.concurrent.CompletableFuture[String] = {
    java.util.concurrent.CompletableFuture.supplyAsync { () =>
      new String(is.readAllBytes(), StandardCharsets.UTF_8)
    }
  }
}

object GitInvokerImpl {

  /** Captured result of one git subprocess invocation. */
  final case class ProcResult(command: Seq[String], exitCode: Int, stdout: String, stderr: String)
}
