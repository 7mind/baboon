package io.septimalmind.baboon.git

import io.septimalmind.baboon.git.GitModelMaterializer.{Materialized, MaterializeFailure}
import izumi.functional.bio.unsafe.MaybeSuspend2
import izumi.functional.bio.{Error2, F}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** Materialize a set of `--model-dir` / `--model` inputs AT A GIT REF, re-rooted under a
  * throwaway detached worktree, and hand the re-rooted [[java.nio.file.Path]] list to a
  * continuation.
  *
  * Strategy A (Q57): the models at `ref` are checked out via `git worktree add --detach` into a
  * temp directory, so [[io.septimalmind.baboon.BaboonLoader]] needs NO change — it stays
  * Path-based and simply loads from the re-rooted paths this component returns. There is no
  * `git show`/blob-streaming and no in-memory VFS.
  *
  * Re-rooting (Q58): the repo root is resolved via `git -C <dir-of-first-model-dir> rev-parse
  * --show-toplevel`. Each working-tree `--model-dir` / `--model` is relativized against that root
  * and re-rooted under the temp worktree; every entry for the pinned side is taken AT THE REF. An
  * absolute input that does not live under the repo root is a fail-fast error.
  *
  * Lifecycle (Q57): the worktree is created, used, and removed inside a bracket, so a crash in the
  * continuation still triggers `git worktree remove`. A cleanup FAILURE is surfaced (folded into
  * the error channel), never swallowed.
  *
  * Fail-fast taxonomy (Q60): the git-binary / repo-root / ref-verify checks run as a PRE-FLIGHT —
  * before the worktree is created and before any heavy consumer work — and each distinct failure
  * mode is a distinct [[MaterializeFailure]] case that [[GitModelMaterializerErrors]] renders as an
  * actionable message with a distinct nonzero exit code.
  *
  * TESTABILITY: the component is parametric in [[GitInvoker]]; driving it with a
  * [[GitInvokerDummy]] configured `GitMissing` / `RefUnresolvable` exercises the git-missing and
  * ref-unresolvable branches with no subprocess (T197 owns that suite).
  */
final class GitModelMaterializer[F[+_, +_]: Error2: MaybeSuspend2](
  git: GitInvoker[F],
  tempDirs: TempWorktreeFactory[F],
) {

  /** Pre-flight the git binary + repo root + ref, then materialize the inputs at `ref` under a
    * detached worktree and run `use` against the re-rooted paths. The worktree is removed in a
    * bracket regardless of how `use` finishes; a cleanup failure is surfaced.
    *
    * @param ref       the git ref whose model tree to pin (e.g. a branch, tag, or SHA).
    * @param modelDirs the working-tree `--model-dir` inputs (directories scanned for `.baboon`).
    * @param models    the working-tree `--model` inputs (individual `.baboon` files).
    * @param cwd       the process working directory; used only to resolve relative inputs.
    * @param use       continuation receiving the re-rooted, at-ref paths.
    */
  def withModelsAtRef[A](
    ref: String,
    modelDirs: List[Path],
    models: List[Path],
    cwd: Path,
  )(use: Materialized => F[MaterializeFailure, A]): F[MaterializeFailure, A] = {
    for {
      // --- PRE-FLIGHT (before any worktree / heavy injector) ---
      available <- git.isAvailable
      _         <- if (available) F.unit else F.fail(MaterializeFailure.GitBinaryMissing)
      anchor    <- firstAnchorDir(modelDirs, models, cwd)
      root      <- git.repoRoot(anchor).catchAll(f => F.fail(MaterializeFailure.NotAGitRepo(anchor, describe(f))))
      _ <- git.resolveRef(root, ref).catchAll {
        case GitInvokeFailure.RefUnresolvable(_, r, stderr) => F.fail(MaterializeFailure.RefUnresolvable(r, stderr))
        case other                                          => F.fail(MaterializeFailure.RefUnresolvable(ref, describe(other)))
      }
      // Relativize each pinned input to the repo root up front so an out-of-root absolute path
      // fails fast BEFORE the worktree is created.
      relModelDirs <- F.fromEither(traverseRelativize(root, cwd, modelDirs))
      relModels    <- F.fromEither(traverseRelativize(root, cwd, models))
      result <- bracketWorktree(root, ref) {
        worktree =>
          val rerootedDirs   = relModelDirs.map(worktree.resolve)
          val rerootedModels = relModels.map(worktree.resolve)
          for {
            _ <- assertBaboonPresent(ref, rerootedDirs, rerootedModels)
            a <- use(Materialized(worktree, rerootedDirs, rerootedModels))
          } yield a
      }
    } yield result
  }

  /** Resolve the directory to anchor `git rev-parse --show-toplevel` on: the dir of the first
    * `--model-dir`, else the parent of the first `--model`, else the CWD. Relative inputs are
    * resolved against `cwd`. */
  private def firstAnchorDir(modelDirs: List[Path], models: List[Path], cwd: Path): F[MaterializeFailure, Path] = {
    val anchor = modelDirs.headOption
      .map(abs(cwd, _))
      .orElse(models.headOption.map(m => abs(cwd, m).getParent))
      .getOrElse(cwd)
    F.pure(anchor)
  }

  /** Relativize every input to `root`; an absolute path not under `root` is a fail-fast error. */
  private def traverseRelativize(root: Path, cwd: Path, inputs: List[Path]): Either[MaterializeFailure, List[Path]] = {
    inputs.foldRight[Either[MaterializeFailure, List[Path]]](Right(Nil)) {
      (in, acc) =>
        for {
          tail <- acc
          rel  <- relativizeUnderRoot(root, cwd, in)
        } yield rel :: tail
    }
  }

  /** Relativize a single working-tree input to `root`. A relative input is resolved against `cwd`
    * first; an input whose normalized absolute form escapes `root` is rejected. */
  private def relativizeUnderRoot(root: Path, cwd: Path, in: Path): Either[MaterializeFailure, Path] = {
    val absIn    = abs(cwd, in)
    val normRoot = root.toAbsolutePath.normalize()
    val normIn   = absIn.normalize()
    if (!normIn.startsWith(normRoot)) {
      Left(MaterializeFailure.PathOutsideRepo(in, normRoot))
    } else {
      Right(normRoot.relativize(normIn))
    }
  }

  private def abs(cwd: Path, p: Path): Path =
    if (p.isAbsolute) p else cwd.resolve(p)

  /** Bracket the worktree lifecycle over the typed error channel. `release` (`git worktree
    * remove`) runs whether `use` succeeds or fails; a use-success with a release-failure surfaces
    * the release failure, and a use-failure with a release-failure surfaces BOTH (release chained
    * into the message) rather than swallowing either.
    *
    * The `Either[Throwable, _]` effect this compiler runs on has only `Error2` (no `Bracket2`), so
    * the guarantee is built from `redeem`; it covers every path that stays in the typed
    * `MaterializeFailure` channel. */
  private def bracketWorktree[A](
    root: Path,
    ref: String,
  )(use: Path => F[MaterializeFailure, A]): F[MaterializeFailure, A] = {
    for {
      worktree <- tempDirs.create(ref)
      _ <- git
        .addWorktree(root, ref, worktree)
        .catchAll(f => cleanup(root, worktree).flatMap(_ => F.fail(MaterializeFailure.WorktreeAddFailed(worktree, ref, describe(f)))))
      result <- use(worktree).redeem(
        err =>
          cleanup(root, worktree).flatMap {
            case Right(())      => F.fail(err)
            case Left(cleanErr) => F.fail(MaterializeFailure.chained(err, cleanErr))
          },
        ok =>
          cleanup(root, worktree).flatMap {
            case Right(())      => F.pure(ok)
            case Left(cleanErr) => F.fail(cleanErr)
          },
      )
    } yield result
  }

  /** `git worktree remove`, reified into the value channel so the bracket can decide whether the
    * primary result or the cleanup failure wins. Cleanup is NEVER swallowed. */
  private def cleanup(root: Path, worktree: Path): F[Nothing, Either[MaterializeFailure, Unit]] = {
    git
      .removeWorktree(root, worktree)
      .redeemPure(
        f => Left(MaterializeFailure.WorktreeCleanupFailed(worktree, describe(f))),
        _ => Right(()),
      )
  }

  /** `path-absent-at-ref`: after re-rooting, no `.baboon` file exists under the pinned inputs. */
  private def assertBaboonPresent(
    ref: String,
    rerootedDirs: List[Path],
    rerootedModels: List[Path],
  ): F[MaterializeFailure, Unit] = {
    F.maybeSuspend {
      val dirHits   = rerootedDirs.exists(dir => Try(walkBaboon(dir)).getOrElse(Nil).nonEmpty)
      val modelHits = rerootedModels.exists(m => Files.isRegularFile(m) && m.getFileName.toString.endsWith(".baboon"))
      dirHits || modelHits
    }.flatMap {
      present =>
        if (present) F.unit
        else {
          val where = (rerootedDirs ++ rerootedModels).map(_.toString).sorted.mkString(", ")
          F.fail(MaterializeFailure.PathAbsentAtRef(ref, where))
        }
    }
  }

  private def walkBaboon(dir: Path): List[Path] = {
    if (!Files.isDirectory(dir)) Nil
    else {
      val stream = Files.walk(dir)
      try stream.iterator().asScala.filter(p => Files.isRegularFile(p) && p.getFileName.toString.endsWith(".baboon")).toList
      finally stream.close()
    }
  }

  private def describe(f: GitInvokeFailure): String = f match {
    case GitInvokeFailure.GitUnavailable(reason)          => reason
    case GitInvokeFailure.RefUnresolvable(_, ref, stderr) => s"ref '$ref': $stderr"
    case GitInvokeFailure.NonZeroExit(cmd, code, _, err)  => s"`${cmd.mkString(" ")}` exited $code: $err"
    case GitInvokeFailure.ProcessFailed(cmd, reason)      => s"`${cmd.mkString(" ")}` failed: $reason"
  }
}

object GitModelMaterializer {

  /** The re-rooted, at-ref inputs handed to the continuation. */
  final case class Materialized(worktree: Path, modelDirs: List[Path], models: List[Path])

  /** Fail-fast taxonomy (Q60). Each case is a DISTINCT actionable failure; the exit-code mapping
    * lives in [[GitModelMaterializerErrors]] so this type stays pure and unit-testable. */
  sealed trait MaterializeFailure
  object MaterializeFailure {

    /** No usable `git` binary on the PATH. */
    case object GitBinaryMissing extends MaterializeFailure

    /** `git rev-parse --show-toplevel` failed for the anchor dir — not inside a git working tree. */
    final case class NotAGitRepo(anchor: Path, detail: String) extends MaterializeFailure

    /** The requested ref does not resolve to a commit. Names the ref. */
    final case class RefUnresolvable(ref: String, detail: String) extends MaterializeFailure

    /** An absolute `--model-dir` / `--model` does not live under the repo root, so it cannot be
      * re-rooted at the ref. */
    final case class PathOutsideRepo(input: Path, repoRoot: Path) extends MaterializeFailure

    /** No `.baboon` file exists under the re-rooted inputs at the ref. Names ref + paths. */
    final case class PathAbsentAtRef(ref: String, paths: String) extends MaterializeFailure

    /** `git worktree add --detach` failed. */
    final case class WorktreeAddFailed(worktree: Path, ref: String, detail: String) extends MaterializeFailure

    /** `git worktree remove` failed — surfaced, never swallowed. */
    final case class WorktreeCleanupFailed(worktree: Path, detail: String) extends MaterializeFailure

    /** A primary failure AND a cleanup failure both occurred; both are preserved. */
    final case class MultipleFailures(primary: MaterializeFailure, cleanup: MaterializeFailure) extends MaterializeFailure

    /** Chain a cleanup failure onto a primary failure without losing either. */
    def chained(primary: MaterializeFailure, cleanup: MaterializeFailure): MaterializeFailure =
      MultipleFailures(primary, cleanup)
  }
}

/** Allocates a fresh throwaway directory to host the detached worktree. Abstracted so tests can
  * inject a deterministic location and production uses a real temp dir. */
trait TempWorktreeFactory[F[+_, +_]] {

  /** Create (but do NOT `git worktree add` into) a fresh empty directory keyed loosely by `ref`.
    * The git worktree-add target must not pre-exist, so this returns a path whose leaf does not
    * yet exist as a worktree checkout. */
  def create(ref: String): F[Nothing, Path]
}

object TempWorktreeFactory {

  /** Production factory: a per-invocation subdirectory under `java.io.tmpdir`. The directory is
    * created empty; `git worktree add` populates it. */
  final class SystemTempWorktreeFactory[F[+_, +_]: Error2: MaybeSuspend2] extends TempWorktreeFactory[F] {
    override def create(ref: String): F[Nothing, Path] = F.maybeSuspend {
      val safeRef = ref.replaceAll("[^A-Za-z0-9._-]", "_")
      Files.createTempDirectory(s"baboon-git-$safeRef-")
    }
  }
}
