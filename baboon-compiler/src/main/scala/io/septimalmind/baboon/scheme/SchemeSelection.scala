package io.septimalmind.baboon.scheme

import io.septimalmind.baboon.typer.model.{BaboonFamily, Pkg, Version}
import izumi.fundamentals.collections.nonempty.NEList

/** One component of a `domain@version` selector: the whole-component wildcard `*`, or an exact value. */
sealed trait SelectorComponent[+T]
object SelectorComponent {
  case object Wildcard extends SelectorComponent[Nothing]
  final case class Exact[T](value: T) extends SelectorComponent[T]
}

final case class SchemeSelector(raw: String, domain: SelectorComponent[Pkg], version: SelectorComponent[Version]) {
  def matches(pkg: Pkg, ver: Version): Boolean = {
    val domainMatches = domain match {
      case SelectorComponent.Wildcard     => true
      case SelectorComponent.Exact(value) => value == pkg
    }
    val versionMatches = version match {
      case SelectorComponent.Wildcard     => true
      case SelectorComponent.Exact(value) => value == ver
    }
    domainMatches && versionMatches
  }
}

final case class SchemeDomainVersion(pkg: Pkg, version: Version)

object SchemeSelection {
  private val WildcardToken: String = "*"
  private val Identifier            = "[A-Za-z_][A-Za-z0-9_]*"
  private val DomainPattern         = s"$Identifier(\\.$Identifier)*".r

  /** Parses `--domains`: comma-separated `domain@version` items, each trimmed; `*` may replace a whole component. */
  def parseSelectors(raw: String): Either[NEList[String], NEList[SchemeSelector]] = {
    val parsed = raw.split(",", -1).toList.map(item => parseSelector(item.trim))
    val errors = parsed.collect { case Left(error) => error }
    NEList.from(errors) match {
      case Some(nel) => Left(nel)
      case None      => Right(NEList.unsafeFrom(parsed.collect { case Right(selector) => selector }))
    }
  }

  private def parseSelector(item: String): Either[String, SchemeSelector] = {
    item.split("@", -1).toList match {
      case domain :: version :: Nil =>
        for {
          d <- parseComponent(item, "domain", domain)(DomainPattern.matches)(value => Pkg(NEList.unsafeFrom(value.split("\\.").toList)))
          v <- parseComponent(item, "version", version)(isCanonicalVersion)(Version.parse)
        } yield SchemeSelector(item, d, v)
      case _ if item.isEmpty => Left("empty selector: expected 'domain@version' (use '*' for either component)")
      case _                 => Left(s"malformed selector '$item': expected exactly one '@' as in 'my.domain@1.0.0' (use '*' for either component)")
    }
  }

  private def parseComponent[T](item: String, what: String, raw: String)(valid: String => Boolean)(make: String => T): Either[String, SelectorComponent[T]] = {
    if (raw == WildcardToken) {
      Right(SelectorComponent.Wildcard)
    } else if (raw.contains(WildcardToken)) {
      Left(s"malformed selector '$item': only a whole $what may be the wildcard '*'")
    } else if (valid(raw)) {
      Right(SelectorComponent.Exact(make(raw)))
    } else {
      Left(s"malformed selector '$item': '$raw' is not a valid $what")
    }
  }

  /** `Version.parse` accepts any string, so the canonical round trip is the check. */
  private def isCanonicalVersion(raw: String): Boolean = {
    val parsed = Version.parse(raw)
    !parsed.v.isInstanceOf[izumi.fundamentals.platform.versions.Version.Unknown] && parsed.toString == raw
  }

  /** Union of the selectors' matches, sorted by domain then version.
    *
    * Fails when a selector matches nothing, or when a domain's selection skips a version between two
    * selected ones: a rendered schema records evolution (`was` renames included) against the version
    * immediately before it, so reloading a selection with a gap would compare non-adjacent versions
    * and lose that information.
    */
  def resolve(family: BaboonFamily, selectors: NEList[SchemeSelector]): Either[NEList[String], NEList[SchemeDomainVersion]] = {
    val available = family.domains.toMap.toList.flatMap {
      case (pkg, lineage) => lineage.versions.toMap.keys.map(SchemeDomainVersion(pkg, _))
    }
    val unmatched = selectors.toList.filterNot(s => available.exists(dv => s.matches(dv.pkg, dv.version))).map {
      selector =>
        s"selector '${selector.raw}' matches no loaded domain version; available: ${describe(available)}"
    }
    val selected = sort(available.filter(dv => selectors.exists(_.matches(dv.pkg, dv.version))))
    val gaps = selected.groupBy(_.pkg).toList.sortBy(_._1.toString).flatMap {
      case (pkg, chosen) =>
        val lineageVersions = family.domains.toMap(pkg).versions.toMap.keys.toList.sorted
        val chosenVersions  = chosen.map(_.version).sorted
        val skipped         = lineageVersions.filter(v => v > chosenVersions.head && v < chosenVersions.last && !chosenVersions.contains(v))
        if (skipped.isEmpty) {
          None
        } else {
          Some(
            s"selection of $pkg (${chosenVersions.mkString(", ")}) skips ${skipped.mkString(", ")}: rendered schemas record evolution, including `was` renames, against the immediately preceding version, so the reloaded archive would compare non-adjacent versions and lose that information; select a contiguous range of versions"
          )
        }
    }
    NEList.from(unmatched ++ gaps) match {
      case Some(errors) => Left(errors)
      case None         => Right(NEList.unsafeFrom(selected))
    }
  }

  private def sort(entries: List[SchemeDomainVersion]): List[SchemeDomainVersion] = {
    entries.sortWith {
      (a, b) =>
        val byDomain = a.pkg.toString.compareTo(b.pkg.toString)
        if (byDomain != 0) byDomain < 0 else a.version < b.version
    }
  }

  private def describe(available: List[SchemeDomainVersion]): String = {
    sort(available)
      .groupBy(_.pkg).toList.sortBy(_._1.toString).map {
        case (pkg, versions) => s"$pkg@{${versions.map(_.version).sorted.mkString(", ")}}"
      }.mkString("; ")
  }
}
