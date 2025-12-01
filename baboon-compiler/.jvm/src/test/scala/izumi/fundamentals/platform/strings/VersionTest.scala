package izumi.fundamentals.platform.strings

import io.septimalmind.baboon.typer.model.Version
import org.scalatest.wordspec.AnyWordSpec

import scala.math.Ordered.orderingToOrdered

class VersionTest extends AnyWordSpec {
  "Version comparison" should {
    "properly handle >, <, >=, <=" in {
      val v1 = Version.parse("3.8.0")
      val v2 = Version.parse("3.10.0")
      assert(v1 < v2)
      assert(v1 <= v2)
      assert(v2 > v1)
      assert(v2 >= v1)
    }

    "support ordering" in {
      val versions = List(
        "1.0.0",
        "2.0.0",
      ).map(Version.parse)

      assert(versions.sorted == versions)
      assert(versions.sorted(Version.ordering.reverse) == versions.reverse)
    }
  }
}

class ParsedVersionTest extends AnyWordSpec {
  "ParsedVersion" should {
    "properly support ordering" in {
      val versions = List(
        "1.0.0-alpha",
        "1.0.0-alpha.1",
        "1.0.0-alpha.2",
        "1.0.0-beta",
        "1.0.0-rc.1",
        "1.0.0",
        "1.0.1",
        "1.2.3.4",
        "1.2.3.4.5-s1-suffix2",
        "1.2.3.4.5",
        "1.2.3.4.5.6.7.8.9",
        "2.0.0",
        "10.0.0",
      )

      versions.foreach {
        v =>
          Version.parse(v).v match {
            case _: izumi.fundamentals.platform.versions.Version.Unknown =>
              fail(s"Failed to parse $v")
            case _ =>

          }
      }

      val parsedVersions = versions.map(Version.parse)
      val sorted         = parsedVersions.sorted
      assert(sorted.map(_.toString) == versions)

      def compareVersions(v1: String, v2: String): Unit = {
        (Version.parse(v1), Version.parse(v2)) match {
          case (pv1, pv2) =>
            val _ = assert(pv1.compare(pv2) > 0, s"$v1 > $v2")
          case _ =>
            fail(s"failed to parse $v1 / $v2 pair")
        }
      }

      compareVersions("1.0.0", "1.0.0-alpha")
      compareVersions("1.2.3.0", "1.2.3")
      compareVersions("1.2.3.4.5", "1.2.3.4.5-s1-suffix2")
      compareVersions("2.0.0-beta", "2.0.0-alpha")

    }
  }
}
