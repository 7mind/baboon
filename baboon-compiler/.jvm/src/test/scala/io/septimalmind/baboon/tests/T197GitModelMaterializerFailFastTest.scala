package io.septimalmind.baboon.tests

import io.septimalmind.baboon.diff.VersionRef
import io.septimalmind.baboon.diff.VersionRef.ParseError
import io.septimalmind.baboon.git.{GitInvokerDummy, GitModelMaterializer, TempWorktreeFactory}
import io.septimalmind.baboon.git.GitModelMaterializer.MaterializeFailure
import izumi.functional.bio.Error2
import izumi.functional.bio.impl.BioEither
import izumi.functional.bio.unsafe.MaybeSuspend2
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}

/** ScalaTest suite for T197 (G33/Q62): fail-fast behaviour of [[GitModelMaterializer]] driven by the
  * [[GitInvokerDummy]] — no real git binary required.
  *
  * Covers:
  *   (A) [[VersionRef.parse]] cases NOT already in T189VersionRefParserTest: specifically the
  *       `2.0.0@deadbeef` and `2.0.0@HEAD~1` forms named in Q62 (T189 covers
  *       `HEAD@{1}`, bare version, leading/trailing `@`, and bad version, so only
  *       the `deadbeef` hex-sha and `HEAD~1` shorthand cases are added here).
  *   (B) [[GitModelMaterializer.withModelsAtRef]] pre-flight via [[GitInvokerDummy]]:
  *       (B1) `GitMissing` behavior → `MaterializeFailure.GitBinaryMissing` (distinct, singleton).
  *       (B2) `RefUnresolvable` behavior → `MaterializeFailure.RefUnresolvable` naming the ref.
  *
  * Both (B) assertions are unconditional `assert`/`fail` — not vacuous.
  */
final class T197GitModelMaterializerFailFastTest extends AnyWordSpec {

  // ── BIO effect: Either on the JVM ──────────────────────────────────────────
  private type EitherF[+E, +A] = Either[E, A]
  private implicit val error2: Error2[EitherF]               = BioEither
  private implicit val maybeSuspend2: MaybeSuspend2[EitherF] = new MaybeSuspend2[EitherF]

  // ── (A) VersionRef.parse supplementary cases ───────────────────────────────

  "VersionRef.parse" should {

    "parse '2.0.0@deadbeef' as version=2.0.0 and ref=deadbeef (hex-sha shorthand)" in {
      val result = VersionRef.parse("2.0.0@deadbeef")
      result match {
        case Right(vr) =>
          assert(vr.version.toString == "2.0.0", s"version mismatch: ${vr.version}")
          assert(vr.ref.contains("deadbeef"), s"expected ref=deadbeef but got ref=${vr.ref}")
        case Left(err) =>
          fail(s"expected Right for '2.0.0@deadbeef', got Left(${err.message})")
      }
    }

    "parse '2.0.0@HEAD~1' as version=2.0.0 and ref=HEAD~1 (tilde shorthand)" in {
      val result = VersionRef.parse("2.0.0@HEAD~1")
      result match {
        case Right(vr) =>
          assert(vr.version.toString == "2.0.0", s"version mismatch: ${vr.version}")
          assert(vr.ref.contains("HEAD~1"), s"expected ref=HEAD~1 but got ref=${vr.ref}")
        case Left(err) =>
          fail(s"expected Right for '2.0.0@HEAD~1', got Left(${err.message})")
      }
    }
  }

  // ── (B) GitModelMaterializer fail-fast via GitInvokerDummy ─────────────────

  /** A [[TempWorktreeFactory]] that returns a fixed path; never actually creates anything on disk.
    * This path is never reached in the git-missing / ref-unresolvable pre-flight paths, so it
    * is safe to point at a non-existent location. */
  private val dummyTempFactory: TempWorktreeFactory[EitherF] = new TempWorktreeFactory[EitherF] {
    override def create(ref: String): EitherF[Nothing, java.nio.file.Path] =
      Right(Paths.get("/tmp/t197-dummy-worktree"))
  }

  private val cwd = Paths.get(System.getProperty("user.dir"))

  /** Exercises [[GitModelMaterializer.withModelsAtRef]] and returns the failure. The continuation
    * is never reached in the pre-flight-failure scenarios; it always fails with `unexpected continuation call`. */
  private def runMaterializer(
    behavior: GitInvokerDummy.Behavior,
    ref: String,
    modelDirs: List[java.nio.file.Path] = List(cwd),
    models: List[java.nio.file.Path]    = Nil,
  ): Either[MaterializeFailure, Nothing] = {
    val git          = new GitInvokerDummy[EitherF](behavior)
    val materializer = new GitModelMaterializer[EitherF](git, dummyTempFactory)
    materializer.withModelsAtRef(ref, modelDirs, models, cwd) { _ =>
      Left(throw new AssertionError("continuation must not be reached in pre-flight-failure scenarios"))
    }
  }

  "GitModelMaterializer.withModelsAtRef" when {

    "GitInvokerDummy is configured as GitMissing" should {

      "return MaterializeFailure.GitBinaryMissing (distinct singleton case)" in {
        val result = runMaterializer(GitInvokerDummy.Behavior.GitMissing, ref = "main")
        result match {
          case Left(MaterializeFailure.GitBinaryMissing) =>
            // correct — the pre-flight detected git absent and returned the expected singleton case
            succeed
          case Left(other) =>
            fail(s"expected GitBinaryMissing but got ${other.getClass.getSimpleName}: $other")
          case Right(_) =>
            fail("expected a Left(GitBinaryMissing) but the continuation succeeded")
        }
      }

      "not yield any RefUnresolvable failure when git is missing" in {
        // Negative: the failure must NOT be RefUnresolvable — git absence is detected first.
        val result = runMaterializer(GitInvokerDummy.Behavior.GitMissing, ref = "some-ref")
        result match {
          case Left(MaterializeFailure.GitBinaryMissing) =>
            succeed
          case Left(MaterializeFailure.RefUnresolvable(_, _)) =>
            fail("got RefUnresolvable but should have gotten GitBinaryMissing (git-missing detected first)")
          case Left(other) =>
            fail(s"unexpected failure case: ${other.getClass.getSimpleName}: $other")
          case Right(_) =>
            fail("expected a Left failure but got Right")
        }
      }
    }

    "GitInvokerDummy is configured as RefUnresolvable" should {

      "return MaterializeFailure.RefUnresolvable naming the requested ref" in {
        val targetRef = "deadbeef01234567"
        val result    = runMaterializer(GitInvokerDummy.Behavior.RefUnresolvable, ref = targetRef)
        result match {
          case Left(MaterializeFailure.RefUnresolvable(ref, _)) =>
            assert(
              ref == targetRef,
              s"RefUnresolvable failure must name the requested ref '$targetRef' but named '$ref'",
            )
          case Left(MaterializeFailure.GitBinaryMissing) =>
            fail("got GitBinaryMissing but dummy is RefUnresolvable — git is available in this mode")
          case Left(other) =>
            fail(s"expected RefUnresolvable but got ${other.getClass.getSimpleName}: $other")
          case Right(_) =>
            fail("expected a Left(RefUnresolvable) but the continuation succeeded")
        }
      }

      "name the symbolic ref 'HEAD~3' in the failure" in {
        val symbolicRef = "HEAD~3"
        val result      = runMaterializer(GitInvokerDummy.Behavior.RefUnresolvable, ref = symbolicRef)
        result match {
          case Left(MaterializeFailure.RefUnresolvable(ref, _)) =>
            assert(ref == symbolicRef, s"expected ref='$symbolicRef' in failure but got ref='$ref'")
          case Left(other) =>
            fail(s"expected RefUnresolvable but got ${other.getClass.getSimpleName}: $other")
          case Right(_) =>
            fail("expected Left(RefUnresolvable) but got Right")
        }
      }
    }
  }
}
