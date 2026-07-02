package io.septimalmind.baboon.tests

import io.septimalmind.baboon.{Baboon, CompilerOptions, LockfileEnforcement, LockfileUpdate}
import org.scalatest.wordspec.AnyWordSpec

/** Unit tests for T158: --lockfile-update / --lockfile-enforcement global CLI flags
  * + threading at the CLI CompilerOptions construction site.
  *
  * Exercises the assembly/validation function `Baboon.parseLockfileOptions` directly
  * (end-to-end CaseApp parsing is impractical to drive from a unit test):
  *   (a) absent flags  => CreateOnly / LegacyVersions
  *   (b) force + all-versions threads through as Force / AllVersions
  *   (c) bad enum value => Left (Q37c hard CLI error)
  *   (d) flag without --lockfile => Left (Q37b fail-fast)
  *
  * Plus: feeding the parsed pair into a CLI-shaped CompilerOptions construction yields
  * the expected lockfileUpdate / lockfileEnforcement (mirrors the threading at the
  * createCompilerOptions site in Baboon.scala).
  */
class T158LockfileCliFlagsTest extends AnyWordSpec {

  private val lock = Some("./target/baboon.lock")

  "Baboon.parseLockfileOptions" should {

    "yield (CreateOnly, LegacyVersions) when both flags are absent" in {
      val result = Baboon.parseLockfileOptions(lock, None, None)
      assert(result == Right((LockfileUpdate.CreateOnly, LockfileEnforcement.LegacyVersions)))
    }

    "yield (CreateOnly, LegacyVersions) when both flags AND --lockfile are absent" in {
      val result = Baboon.parseLockfileOptions(None, None, None)
      assert(result == Right((LockfileUpdate.CreateOnly, LockfileEnforcement.LegacyVersions)))
    }

    "thread force + all-versions through as (Force, AllVersions)" in {
      val result = Baboon.parseLockfileOptions(lock, Some("force"), Some("all-versions"))
      assert(result == Right((LockfileUpdate.Force, LockfileEnforcement.AllVersions)))
    }

    "reject an unrecognized lockfile-update value as a hard error (Q37c)" in {
      val result = Baboon.parseLockfileOptions(lock, Some("foo"), None)
      assert(result.isLeft, s"expected Left for --lockfile-update=foo, got: $result")
      val msg = result.left.getOrElse(throw new AssertionError()).toList.mkString
      assert(msg.contains("foo"), s"error should mention the bad value, got: $msg")
    }

    "reject an unrecognized lockfile-enforcement value as a hard error (Q37c)" in {
      val result = Baboon.parseLockfileOptions(lock, None, Some("sometimes"))
      assert(result.isLeft, s"expected Left for --lockfile-enforcement=sometimes, got: $result")
      val msg = result.left.getOrElse(throw new AssertionError()).toList.mkString
      assert(msg.contains("sometimes"), s"error should mention the bad value, got: $msg")
    }

    "reject --lockfile-update without --lockfile as a hard error (Q37b)" in {
      val result = Baboon.parseLockfileOptions(None, Some("force"), None)
      assert(result.isLeft, s"expected Left for --lockfile-update without --lockfile, got: $result")
      val msg = result.left.getOrElse(throw new AssertionError()).toList.mkString
      assert(msg.contains("--lockfile"), s"error should mention --lockfile, got: $msg")
    }

    "reject --lockfile-enforcement without --lockfile as a hard error (Q37b)" in {
      val result = Baboon.parseLockfileOptions(None, None, Some("all-versions"))
      assert(result.isLeft, s"expected Left for --lockfile-enforcement without --lockfile, got: $result")
      val msg = result.left.getOrElse(throw new AssertionError()).toList.mkString
      assert(msg.contains("--lockfile"), s"error should mention --lockfile, got: $msg")
    }
  }

  "the CLI CompilerOptions construction (threading parsed flags)" should {

    def construct(pair: (LockfileUpdate, LockfileEnforcement)): CompilerOptions =
      CompilerOptions(
        individualInputs         = Set.empty,
        directoryInputs          = Set.empty,
        lockFile                 = None,
        debug                    = false,
        targets                  = Seq.empty,
        metaWriteEvolutionJsonTo = None,
        emitOnly                 = None,
        lockfileUpdate           = pair._1,
        lockfileEnforcement      = pair._2,
      )

    "carry CreateOnly / LegacyVersions for the absent-flags case" in {
      val pair = Baboon.parseLockfileOptions(lock, None, None).getOrElse(throw new AssertionError())
      val opts = construct(pair)
      assert(opts.lockfileUpdate == LockfileUpdate.CreateOnly)
      assert(opts.lockfileEnforcement == LockfileEnforcement.LegacyVersions)
    }

    "carry Force / AllVersions when --lockfile-update=force --lockfile-enforcement=all-versions" in {
      val pair = Baboon.parseLockfileOptions(lock, Some("force"), Some("all-versions")).getOrElse(throw new AssertionError())
      val opts = construct(pair)
      assert(opts.lockfileUpdate == LockfileUpdate.Force)
      assert(opts.lockfileEnforcement == LockfileEnforcement.AllVersions)
    }
  }
}
