package io.septimalmind.baboon.tests

import io.septimalmind.baboon.{LockfileEnforcement, LockfileUpdate}
import org.scalatest.wordspec.AnyWordSpec

/** Unit tests for T154: LockfileUpdate / LockfileEnforcement sealed enums + defaulted CompilerOptions fields.
  *
  * Verifies:
  *   - parse round-trips for every valid CLI string
  *   - parse("foo") yields Left (unrecognized-value error)
  *   - CompilerOptions constructed without the two new args yields CreateOnly / LegacyVersions defaults
  */
class T154LockfileEnumTest extends AnyWordSpec {

  "LockfileUpdate.parse" should {

    "parse 'create-only' as CreateOnly" in {
      assert(LockfileUpdate.parse("create-only") == Right(LockfileUpdate.CreateOnly))
    }

    "parse 'force' as Force" in {
      assert(LockfileUpdate.parse("force") == Right(LockfileUpdate.Force))
    }

    "return Left for an unrecognized value" in {
      val result = LockfileUpdate.parse("foo")
      assert(result.isLeft, s"expected Left for 'foo', got: $result")
      val msg = result.left.getOrElse("")
      assert(msg.contains("foo"), s"error message should mention the unrecognized value, got: $msg")
    }
  }

  "LockfileEnforcement.parse" should {

    "parse 'none' as None" in {
      assert(LockfileEnforcement.parse("none") == Right(LockfileEnforcement.None))
    }

    "parse 'legacy-versions' as LegacyVersions" in {
      assert(LockfileEnforcement.parse("legacy-versions") == Right(LockfileEnforcement.LegacyVersions))
    }

    "parse 'all-versions' as AllVersions" in {
      assert(LockfileEnforcement.parse("all-versions") == Right(LockfileEnforcement.AllVersions))
    }

    "return Left for an unrecognized value" in {
      val result = LockfileEnforcement.parse("bar")
      assert(result.isLeft, s"expected Left for 'bar', got: $result")
      val msg = result.left.getOrElse("")
      assert(msg.contains("bar"), s"error message should mention the unrecognized value, got: $msg")
    }
  }

  "CompilerOptions default field values" should {

    "yield CreateOnly for lockfileUpdate when constructed without that arg" in {
      import io.septimalmind.baboon.CompilerOptions
      val opts = CompilerOptions(
        individualInputs       = Set.empty,
        directoryInputs        = Set.empty,
        lockfile               = scala.None,
        debug                  = false,
        targets                = Seq.empty,
        metaWriteEvolutionJsonTo = scala.None,
        emitOnly               = scala.None,
      )
      assert(opts.lockfileUpdate == LockfileUpdate.CreateOnly)
    }

    "yield LegacyVersions for lockfileEnforcement when constructed without that arg" in {
      import io.septimalmind.baboon.CompilerOptions
      val opts = CompilerOptions(
        individualInputs       = Set.empty,
        directoryInputs        = Set.empty,
        lockfile               = scala.None,
        debug                  = false,
        targets                = Seq.empty,
        metaWriteEvolutionJsonTo = scala.None,
        emitOnly               = scala.None,
      )
      assert(opts.lockfileEnforcement == LockfileEnforcement.LegacyVersions)
    }
  }
}
