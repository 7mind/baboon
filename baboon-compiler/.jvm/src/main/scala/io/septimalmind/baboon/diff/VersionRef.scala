package io.septimalmind.baboon.diff

import io.septimalmind.baboon.typer.model.Version

/** Represents a parsed `version@ref` string used by the diff CLI.
  *
  * Rules (Q59 / T189):
  *  - No `@` → `ref = None`, version parsed from the whole string.
  *  - Split on FIRST `@` → left must be a valid version; right is the ENTIRE
  *    remainder verbatim (so `2.0.0@HEAD@{1}` produces ref `HEAD@{1}`).
  *  - Leading `@` (empty version part) → [[VersionRef.ParseError.EmptyVersion]].
  *  - Trailing `@` (empty ref part) → [[VersionRef.ParseError.EmptyRef]].
  *  - Unparseable version string → [[VersionRef.ParseError.BadVersion]].
  */
final case class VersionRef(version: Version, ref: Option[String])

object VersionRef {

  sealed trait ParseError {
    def message: String
  }

  object ParseError {
    final case class EmptyVersion(raw: String) extends ParseError {
      def message: String = s"version part is empty in '$raw'"
    }
    final case class EmptyRef(raw: String) extends ParseError {
      def message: String = s"ref part is empty in '$raw'"
    }
    final case class BadVersion(versionStr: String) extends ParseError {
      def message: String = s"invalid version string: '$versionStr'"
    }
  }

  /** Parses a raw `version` or `version@ref` string into a [[VersionRef]].
    *
    * Referentially transparent; no IO.
    */
  def parse(raw: String): Either[ParseError, VersionRef] = {
    val atIdx = raw.indexOf('@')
    if (atIdx < 0) {
      // No '@': the whole string is the version, ref = None.
      parseVersion(raw).map(v => VersionRef(v, None))
    } else {
      val versionStr = raw.substring(0, atIdx)
      val refStr     = raw.substring(atIdx + 1)
      if (versionStr.isEmpty) {
        Left(ParseError.EmptyVersion(raw))
      } else if (refStr.isEmpty) {
        Left(ParseError.EmptyRef(raw))
      } else {
        parseVersion(versionStr).map(v => VersionRef(v, Some(refStr)))
      }
    }
  }

  private def parseVersion(versionStr: String): Either[ParseError, Version] = {
    val v = Version.parse(versionStr)
    v.v match {
      case _: izumi.fundamentals.platform.versions.Version.Unknown =>
        Left(ParseError.BadVersion(versionStr))
      case _ =>
        Right(v)
    }
  }
}
