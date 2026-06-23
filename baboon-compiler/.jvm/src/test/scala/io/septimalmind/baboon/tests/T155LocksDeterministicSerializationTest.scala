package io.septimalmind.baboon.tests

import io.circe.syntax.*
import io.septimalmind.baboon.{LockCodecs, Locks, SigId, VersionLock}
import io.septimalmind.baboon.typer.model.{Pkg, Version}
import izumi.fundamentals.collections.nonempty.NEList
import org.scalatest.wordspec.AnyWordSpec

/** Unit tests for T155: Locks serialization must be deterministic.
  *
  * Verifies:
  *   - inner VersionLock list is emitted in semver order (1.2.0 before 1.10.0)
  *   - outer JSON object keys appear in alphabetical Pkg order
  *   - round-trip decode of the emitted JSON yields a value equal (as a set) to the input
  */
class T155LocksDeterministicSerializationTest extends AnyWordSpec {

  import LockCodecs.*

  // Two packages added in reverse-alphabetical order so a naive Map iteration
  // might produce them in the wrong order.
  private val pkgAlpha = Pkg(NEList("alpha", "pkg"))
  private val pkgZeta  = Pkg(NEList("zeta", "pkg"))

  // Versions in scrambled order; 1.10.0 must sort AFTER 1.2.0 (semver, not lexicographic).
  private val v100  = Version.parse("1.0.0")
  private val v120  = Version.parse("1.2.0")
  private val v1100 = Version.parse("1.10.0")
  private val v200  = Version.parse("2.0.0")

  private val locks = Locks(
    locks = Map(
      pkgZeta  -> List(VersionLock(v200, SigId("sig-z2")), VersionLock(v100, SigId("sig-z1"))),
      pkgAlpha -> List(VersionLock(v1100, SigId("sig-a3")), VersionLock(v100, SigId("sig-a1")), VersionLock(v120, SigId("sig-a2"))),
    )
  )

  "Locks encoder" should {

    "emit outer packages in alphabetical Pkg order" in {
      val json    = locks.asJson
      val locksObj = json.hcursor.downField("locks").focus.getOrElse(fail("no 'locks' field"))
      val keys    = locksObj.asObject.getOrElse(fail("'locks' is not a JSON object")).keys.toList
      assert(keys == List("alpha.pkg", "zeta.pkg"),
        s"expected alphabetical pkg order [alpha.pkg, zeta.pkg] but got: $keys")
    }

    "emit inner versions in semver order (1.2.0 before 1.10.0)" in {
      val json    = locks.asJson
      val cursor  = json.hcursor.downField("locks").downField("alpha.pkg")
      val versions = cursor.focus
        .getOrElse(fail("no alpha.pkg field"))
        .asArray
        .getOrElse(fail("alpha.pkg is not an array"))
        .map { entry =>
          entry.hcursor.downField("version").as[String].fold(e => fail(s"no version: $e"), identity)
        }
        .toList
      assert(versions == List("1.0.0", "1.2.0", "1.10.0"),
        s"expected semver order [1.0.0, 1.2.0, 1.10.0] but got: $versions")
    }

    "round-trip decode yields a value equal (as a set) to the input" in {
      val json    = locks.asJson
      val decoded = json.as[Locks].fold(e => fail(s"decode failed: $e"), identity)
      // Compare as sets: order of versions list and map iteration are irrelevant to equality.
      val inputAsSetMap  = locks.locks.map { case (k, vs) => k -> vs.toSet }
      val decodedAsSetMap = decoded.locks.map { case (k, vs) => k -> vs.toSet }
      assert(inputAsSetMap == decodedAsSetMap,
        s"round-trip mismatch:\n  input:   $inputAsSetMap\n  decoded: $decodedAsSetMap")
    }
  }
}
