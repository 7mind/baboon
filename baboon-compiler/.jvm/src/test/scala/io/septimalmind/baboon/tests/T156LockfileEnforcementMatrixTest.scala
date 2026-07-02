package io.septimalmind.baboon.tests

import io.circe.syntax.*
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.parser.model.issues.VerificationIssue.LockedVersionModified
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.{Pkg, Version}
import io.septimalmind.baboon.{
  CompilerOptions,
  LockCodecs,
  LockfileEnforcement,
  LockfileManagerImpl,
  LockfileUpdate,
  Locks,
  SigId,
  VersionLock,
}
import izumi.functional.bio.Error2
import izumi.functional.bio.impl.BioEither
import izumi.functional.bio.unsafe.MaybeSuspend2
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/** T156: enforcement-then-update matrix over present/absent × {create-only,force} × {none,legacy,all}.
  *
  * Drives the `LockfileManagerImpl.enforce` seam (the model-independent core of `validateLock`) against
  * a real filesystem temp file, exercising every matrix cell named in the acceptance criterion.
  */
class T156LockfileEnforcementMatrixTest extends AnyWordSpec with Matchers {

  import LockCodecs.*

  private type EitherF[+E, +A] = Either[E, A]
  private implicit val error2: Error2[EitherF]               = BioEither
  private implicit val maybeSuspend2: MaybeSuspend2[EitherF] = new MaybeSuspend2[EitherF]

  // A single package with a non-latest version (1.0.0) and the latest version (2.0.0).
  private val pkg   = Pkg(NEList("test", "pkg"))
  private val vLow  = Version.parse("1.0.0")
  private val vHigh = Version.parse("2.0.0")

  // `latestOf` projection used by the matrix: 2.0.0 is the latest of `pkg`.
  private val latestOf: Pkg => Version = _ => vHigh

  // The model's current signatures.
  private def currentSigs: Locks =
    Locks(Map(pkg -> List(VersionLock(vLow, SigId("cur-low")), VersionLock(vHigh, SigId("cur-high")))))

  // An on-disk lock with the SAME low sig but a DIFFERENT high sig => only the LATEST version drifted.
  private def existingLatestDrift: Locks =
    Locks(Map(pkg -> List(VersionLock(vLow, SigId("cur-low")), VersionLock(vHigh, SigId("old-high")))))

  // An on-disk lock with a DIFFERENT low sig but the SAME high sig => only a NON-LATEST version drifted.
  private def existingNonLatestDrift: Locks =
    Locks(Map(pkg -> List(VersionLock(vLow, SigId("old-low")), VersionLock(vHigh, SigId("cur-high")))))

  // An on-disk lock identical to currentSigs => no drift.
  private def existingNoDrift: Locks = currentSigs

  private def tmpLockPath(): Path = {
    val dir = Files.createTempDirectory("t156-lock")
    dir.resolve("baboon.lock")
  }

  private def fsPathOf(p: Path): FSPath = FSPath.parse(NEString.unsafeFrom(p.toAbsolutePath.toString))

  private def writeLock(p: Path, locks: Locks): Unit = {
    Files.write(p, locks.asJson.spaces2.getBytes(StandardCharsets.UTF_8))
    ()
  }

  private def readBytes(p: Path): Array[Byte] = Files.readAllBytes(p)

  private def options(lockPath: Path, update: LockfileUpdate, enforcement: LockfileEnforcement): CompilerOptions =
    CompilerOptions(
      individualInputs = Set.empty,
      directoryInputs = Set.empty,
      lockfile = Some(fsPathOf(lockPath)),
      debug = false,
      targets = Seq.empty,
      metaWriteEvolutionJsonTo = None,
      emitOnly = None,
      lockfileUpdate = update,
      lockfileEnforcement = enforcement,
    )

  private def manager(lockPath: Path, update: LockfileUpdate, enforcement: LockfileEnforcement): LockfileManagerImpl[EitherF] =
    new LockfileManagerImpl[EitherF](options(lockPath, update, enforcement), null.asInstanceOf[BaboonEnquiries])

  private def run(m: LockfileManagerImpl[EitherF]): Either[NEList[BaboonIssue], Unit] =
    m.enforce(currentSigs, latestOf)

  private def isLockedVersionModified(issues: NEList[BaboonIssue]): Boolean =
    issues.toList.exists {
      case BaboonIssue.Verification(_: LockedVersionModified) => true
      case _                                                  => false
    }

  "LockfileManagerImpl enforcement-then-update matrix" should {

    // Cell (1)
    "absent + create-only => file created with sorted currentSigs" in {
      val p = tmpLockPath()
      Files.exists(p) shouldBe false
      run(manager(p, LockfileUpdate.CreateOnly, LockfileEnforcement.LegacyVersions)) shouldBe Right(())
      Files.exists(p) shouldBe true
      new String(readBytes(p), StandardCharsets.UTF_8) shouldBe currentSigs.asJson.spaces2
    }

    // Cell (2)
    "absent + force => file created with sorted currentSigs" in {
      val p = tmpLockPath()
      Files.exists(p) shouldBe false
      run(manager(p, LockfileUpdate.Force, LockfileEnforcement.AllVersions)) shouldBe Right(())
      Files.exists(p) shouldBe true
      new String(readBytes(p), StandardCharsets.UTF_8) shouldBe currentSigs.asJson.spaces2
    }

    // Cell (3) — present, no drift
    "present + no drift + (legacy|all|none) => no failure" in {
      Seq(LockfileEnforcement.LegacyVersions, LockfileEnforcement.AllVersions, LockfileEnforcement.None).foreach { enf =>
        val p = tmpLockPath()
        writeLock(p, existingNoDrift)
        run(manager(p, LockfileUpdate.CreateOnly, enf)) shouldBe Right(())
      }
    }

    "present + no drift + create-only leaves bytes unchanged" in {
      val p = tmpLockPath()
      // Valid JSON that parses to no-drift content, but in a 4-space layout distinct from the
      // canonical 2-space sorted form. Create-only must leave these exact bytes untouched.
      val originalText = existingNoDrift.asJson.spaces4
      assert(originalText != currentSigs.asJson.spaces2, "fixture must differ from the canonical write form")
      Files.write(p, originalText.getBytes(StandardCharsets.UTF_8))
      run(manager(p, LockfileUpdate.CreateOnly, LockfileEnforcement.LegacyVersions)) shouldBe Right(())
      new String(readBytes(p), StandardCharsets.UTF_8) shouldBe originalText
    }

    "present + no drift + force rewrites to sorted form" in {
      val p = tmpLockPath()
      writeLock(p, existingNoDrift)
      // Pre-clobber the file with messy bytes that still parse to no-drift content.
      Files.write(p, existingNoDrift.asJson.spaces4.getBytes(StandardCharsets.UTF_8))
      run(manager(p, LockfileUpdate.Force, LockfileEnforcement.LegacyVersions)) shouldBe Right(())
      new String(readBytes(p), StandardCharsets.UTF_8) shouldBe currentSigs.asJson.spaces2
    }

    // Cell (4) — present, NON-latest drift
    "present + non-latest drift + legacy-versions => FAILS with LockedVersionModified" in {
      val p = tmpLockPath()
      writeLock(p, existingNonLatestDrift)
      val res = run(manager(p, LockfileUpdate.CreateOnly, LockfileEnforcement.LegacyVersions))
      res.isLeft shouldBe true
      isLockedVersionModified(res.swap.toOption.get) shouldBe true
    }

    "present + non-latest drift + force => FAILS and does NOT rewrite the file" in {
      val p = tmpLockPath()
      writeLock(p, existingNonLatestDrift)
      val before = readBytes(p)
      val res    = run(manager(p, LockfileUpdate.Force, LockfileEnforcement.LegacyVersions))
      res.isLeft shouldBe true
      isLockedVersionModified(res.swap.toOption.get) shouldBe true
      readBytes(p) shouldBe before // force did NOT silence the violation
    }

    "present + non-latest drift + none => PASSES" in {
      val p = tmpLockPath()
      writeLock(p, existingNonLatestDrift)
      run(manager(p, LockfileUpdate.CreateOnly, LockfileEnforcement.None)) shouldBe Right(())
    }

    "present + non-latest drift + all-versions => FAILS" in {
      val p = tmpLockPath()
      writeLock(p, existingNonLatestDrift)
      val res = run(manager(p, LockfileUpdate.CreateOnly, LockfileEnforcement.AllVersions))
      res.isLeft shouldBe true
      isLockedVersionModified(res.swap.toOption.get) shouldBe true
    }

    // Cell (5) — present, LATEST-only drift
    "present + latest-only drift + legacy-versions => PASSES (latest excluded)" in {
      val p = tmpLockPath()
      writeLock(p, existingLatestDrift)
      run(manager(p, LockfileUpdate.CreateOnly, LockfileEnforcement.LegacyVersions)) shouldBe Right(())
    }

    "present + latest-only drift + all-versions => FAILS" in {
      val p = tmpLockPath()
      writeLock(p, existingLatestDrift)
      val res = run(manager(p, LockfileUpdate.CreateOnly, LockfileEnforcement.AllVersions))
      res.isLeft shouldBe true
      isLockedVersionModified(res.swap.toOption.get) shouldBe true
    }

    "present + latest-only drift + force + all-versions => does NOT rewrite (enforcement authoritative)" in {
      val p = tmpLockPath()
      writeLock(p, existingLatestDrift)
      val before = readBytes(p)
      val res    = run(manager(p, LockfileUpdate.Force, LockfileEnforcement.AllVersions))
      res.isLeft shouldBe true
      readBytes(p) shouldBe before
    }

    // Bonus: latest-only drift under legacy + force DOES rewrite (enforcement passed, force applies).
    "present + latest-only drift + force + legacy-versions => rewrites to sorted form (enforcement passed)" in {
      val p = tmpLockPath()
      writeLock(p, existingLatestDrift)
      run(manager(p, LockfileUpdate.Force, LockfileEnforcement.LegacyVersions)) shouldBe Right(())
      new String(readBytes(p), StandardCharsets.UTF_8) shouldBe currentSigs.asJson.spaces2
    }
  }
}
