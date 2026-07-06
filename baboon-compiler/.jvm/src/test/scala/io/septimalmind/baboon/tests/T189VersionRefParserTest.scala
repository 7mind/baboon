package io.septimalmind.baboon.tests

import io.septimalmind.baboon.diff.VersionRef
import io.septimalmind.baboon.diff.VersionRef.ParseError
import org.scalatest.wordspec.AnyWordSpec

/** Unit tests for [[VersionRef.parse]] (T189 / G33 / Q59).
  *
  * Covers all five acceptance-criterion rules:
  *  1. No `@` → ref=None, version=Version.parse(raw).
  *  2. Split on FIRST `@` → left=version, right=ENTIRE remainder verbatim
  *     (e.g., `2.0.0@HEAD@{1}` → ref `HEAD@{1}`).
  *  3. Leading `@` (empty version) → ParseError.EmptyVersion.
  *  4. Trailing `@` (empty ref) → ParseError.EmptyRef.
  *  5. Bad version string → ParseError.BadVersion naming the bad string.
  */
final class T189VersionRefParserTest extends AnyWordSpec {

  // ─── Rule 1: no '@' — ref = None ────────────────────────────────────────────

  "VersionRef.parse" should {

    "parse a bare version string with no '@' as ref=None" in {
      val result = VersionRef.parse("1.2.3")
      result match {
        case Right(vr) =>
          assert(vr.version.toString == "1.2.3", s"version mismatch: ${vr.version}")
          assert(vr.ref.isEmpty, s"expected ref=None but got ref=${vr.ref}")
        case Left(err) =>
          fail(s"expected Right for '1.2.3', got Left(${err.message})")
      }
    }

    "parse '10.0.0' (no '@') as ref=None" in {
      val result = VersionRef.parse("10.0.0")
      assert(result.isRight, s"expected Right, got $result")
      assert(result.toOption.get.ref.isEmpty)
    }

    // ─── Rule 2: first '@' split — remainder is verbatim ref ──────────────────

    "parse '2.0.0@main' as version=2.0.0 and ref=main" in {
      val result = VersionRef.parse("2.0.0@main")
      result match {
        case Right(vr) =>
          assert(vr.version.toString == "2.0.0", s"version mismatch: ${vr.version}")
          assert(vr.ref.contains("main"), s"expected ref=main but got ref=${vr.ref}")
        case Left(err) =>
          fail(s"expected Right for '2.0.0@main', got Left(${err.message})")
      }
    }

    "split on FIRST '@': '2.0.0@HEAD@{1}' → version=2.0.0, ref=HEAD@{1}" in {
      val result = VersionRef.parse("2.0.0@HEAD@{1}")
      result match {
        case Right(vr) =>
          assert(vr.version.toString == "2.0.0", s"version mismatch: ${vr.version}")
          assert(vr.ref.contains("HEAD@{1}"), s"expected ref=HEAD@{1} but got ref=${vr.ref}")
        case Left(err) =>
          fail(s"expected Right for '2.0.0@HEAD@{1}', got Left(${err.message})")
      }
    }

    "split on FIRST '@': '3.1.4@refs/tags/v3.1.4@2026' → ref=refs/tags/v3.1.4@2026" in {
      val result = VersionRef.parse("3.1.4@refs/tags/v3.1.4@2026")
      result match {
        case Right(vr) =>
          assert(vr.version.toString == "3.1.4")
          assert(vr.ref.contains("refs/tags/v3.1.4@2026"))
        case Left(err) =>
          fail(s"expected Right, got Left(${err.message})")
      }
    }

    // ─── Rule 3: leading '@' (empty version) → ParseError.EmptyVersion ────────

    "reject leading '@' (empty version) with ParseError.EmptyVersion" in {
      val result = VersionRef.parse("@main")
      result match {
        case Left(err: ParseError.EmptyVersion) =>
          assert(err.raw == "@main", s"expected raw='@main' in error, got raw='${err.raw}'")
        case Left(other) =>
          fail(s"expected EmptyVersion error but got ${other.getClass.getSimpleName}: ${other.message}")
        case Right(vr) =>
          fail(s"expected Left for '@main', got Right($vr)")
      }
    }

    "reject '@' alone with ParseError.EmptyVersion (empty version and empty ref but version is checked first)" in {
      // '@' alone: version part is empty → EmptyVersion takes precedence.
      val result = VersionRef.parse("@")
      assert(
        result.isLeft,
        s"expected Left for '@' but got Right",
      )
      result match {
        case Left(err: ParseError.EmptyVersion) => // correct
        case Left(other) =>
          fail(s"expected EmptyVersion error but got ${other.getClass.getSimpleName}: ${other.message}")
        case Right(_) =>
          fail("expected Left")
      }
    }

    // ─── Rule 4: trailing '@' (empty ref) → ParseError.EmptyRef ──────────────

    "reject trailing '@' (empty ref) with ParseError.EmptyRef" in {
      val result = VersionRef.parse("1.0.0@")
      result match {
        case Left(err: ParseError.EmptyRef) =>
          assert(err.raw == "1.0.0@", s"expected raw='1.0.0@' in error, got raw='${err.raw}'")
        case Left(other) =>
          fail(s"expected EmptyRef error but got ${other.getClass.getSimpleName}: ${other.message}")
        case Right(vr) =>
          fail(s"expected Left for '1.0.0@', got Right($vr)")
      }
    }

    // ─── Rule 5: bad version string → ParseError.BadVersion ───────────────────

    "reject a non-version string (no '@') with ParseError.BadVersion" in {
      val result = VersionRef.parse("notaversion")
      result match {
        case Left(err: ParseError.BadVersion) =>
          assert(
            err.versionStr == "notaversion",
            s"expected versionStr='notaversion' but got '${err.versionStr}'",
          )
        case Left(other) =>
          fail(s"expected BadVersion error but got ${other.getClass.getSimpleName}: ${other.message}")
        case Right(vr) =>
          fail(s"expected Left for 'notaversion', got Right($vr)")
      }
    }

    "reject a bad version in 'badver@main' with ParseError.BadVersion naming 'badver'" in {
      val result = VersionRef.parse("badver@main")
      result match {
        case Left(err: ParseError.BadVersion) =>
          assert(
            err.versionStr == "badver",
            s"expected versionStr='badver' but got '${err.versionStr}'",
          )
        case Left(other) =>
          fail(s"expected BadVersion error but got ${other.getClass.getSimpleName}: ${other.message}")
        case Right(vr) =>
          fail(s"expected Left for 'badver@main', got Right($vr)")
      }
    }

  }
}
