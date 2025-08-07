package io.septimalmind.baboon.typer.model

import scala.util.Try

// TODO: move to izumi
case class ParsedVersion(components: Vector[Int], suffix: Option[String] = None) {
  override def toString: String = {
    val versionString = components.mkString(".")
    suffix match {
      case Some(s) => s"$versionString-$s"
      case None    => versionString
    }
  }

  def compare(other: ParsedVersion)(implicit o: Ordering[ParsedVersion]): Int =
    o.compare(this, other)
}

object ParsedVersion {
  def parse(versionString: String): Option[ParsedVersion] = {
    if (versionString.isEmpty) return None

    val dashIndex = versionString.indexOf('-')
    val (numbersPart, suffixPart) = if (dashIndex >= 0) {
      (versionString.substring(0, dashIndex), Some(versionString.substring(dashIndex + 1)))
    } else {
      (versionString, None)
    }

    val componentStrings = numbersPart.split('.')
    if (componentStrings.isEmpty || componentStrings.exists(_.isEmpty)) return None

    val componentsOpt = componentStrings.foldLeft(Option(Vector.empty[Int])) {
      case (Some(acc), str) => Try(str.toInt).toOption.map(acc :+ _)
      case (None, _)        => None
    }

    componentsOpt.map(ParsedVersion(_, suffixPart))
  }

  implicit val versionOrdering: Ordering[ParsedVersion] = new Ordering[ParsedVersion] {
    def compare(v1: ParsedVersion, v2: ParsedVersion): Int = {
      val numericComparison = compareComponents(v1.components, v2.components)

      if (numericComparison != 0) {
        numericComparison
      } else {
        compareSuffix(v1.suffix, v2.suffix)
      }
    }

    private def compareComponents(c1: Vector[Int], c2: Vector[Int]): Int = {
      val minLength = math.min(c1.length, c2.length)

      for (i <- 0 until minLength) {
        val cmp = c1(i).compare(c2(i))
        if (cmp != 0) return cmp
      }

      c1.length.compare(c2.length)
    }

    private def compareSuffix(s1: Option[String], s2: Option[String]): Int = {
      (s1, s2) match {
        case (None, None)                   => 0
        case (None, Some(_))                => 1
        case (Some(_), None)                => -1
        case (Some(suffix1), Some(suffix2)) => compareSuffixStrings(suffix1, suffix2)
      }
    }

    private def compareSuffixStrings(suffix1: String, suffix2: String): Int = {
      val parts1 = suffix1.split("[.-]")
      val parts2 = suffix2.split("[.-]")

      val minLength = math.min(parts1.length, parts2.length)

      for (i <- 0 until minLength) {
        val p1 = parts1(i)
        val p2 = parts2(i)

        (Try(p1.toInt).toOption, Try(p2.toInt).toOption) match {
          case (Some(n1), Some(n2)) =>
            val cmp = n1.compare(n2)
            if (cmp != 0) return cmp
          case (Some(_), None) => return -1 // Numbers come before strings
          case (None, Some(_)) => return 1
          case (None, None) =>
            val cmp = p1.compare(p2)
            if (cmp != 0) return cmp
        }
      }

      parts1.length.compare(parts2.length)
    }
  }
}
