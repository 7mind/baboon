package io.septimalmind.baboon.tests

import io.septimalmind.baboon.scheme.{SchemeSelection, SelectorComponent}
import io.septimalmind.baboon.typer.model.{Pkg, Version}
import io.septimalmind.baboon.{SchemeCLIMode, SchemeCLIOptions}
import izumi.fundamentals.collections.nonempty.NEList
import org.scalatest.wordspec.AnyWordSpec

final class SchemeCLIModeTest extends AnyWordSpec {
  private val none = SchemeCLIOptions(domain = None, version = None, target = None, domains = None, zipOutput = None)

  private def errors(options: SchemeCLIOptions): String = {
    SchemeCLIMode.parse(options).fold(_.toList.mkString("\n"), mode => fail(s"expected a CLI error, got $mode"))
  }

  "SchemeCLIMode.parse" should {
    "keep the single-schema mode" in {
      val pkg = Pkg(NEList("my", "pkg"))
      assert(SchemeCLIMode.parse(none.copy(domain = Some("my.pkg"), version = Some("1.0.0"))) == Right(SchemeCLIMode.Single(pkg, Version.parse("1.0.0"), None)))
      assert(
        SchemeCLIMode.parse(none.copy(domain = Some("my.pkg"), version = Some("1.0.0"), target = Some("out.baboon"))) ==
        Right(SchemeCLIMode.Single(pkg, Version.parse("1.0.0"), Some("out.baboon")))
      )
    }

    "select the archive mode" in {
      SchemeCLIMode.parse(none.copy(domains = Some("*@*"), zipOutput = Some("schemas.zip"))) match {
        case Right(SchemeCLIMode.Archive(selectors, "schemas.zip")) =>
          assert(selectors.toList.map(s => (s.domain, s.version)) == List((SelectorComponent.Wildcard, SelectorComponent.Wildcard)))
        case other => fail(s"unexpected $other")
      }
    }

    "reject mixed modes" in {
      assert(errors(none.copy(domain = Some("a"), version = Some("1.0.0"), domains = Some("*@*"), zipOutput = Some("z.zip"))).contains("cannot be combined"))
      assert(
        errors(none.copy(target = Some("t"), zipOutput = Some("z.zip"), domains = Some("*@*")))
          .contains("--target (single-schema mode) cannot be combined with --domains, --zip-output")
      )
      assert(errors(none.copy(version = Some("1.0.0"), zipOutput = Some("z.zip"))).contains("cannot be combined"))
      assert(errors(none.copy(domain = Some("a"), domains = Some("*@*"))).contains("cannot be combined"))
    }

    "reject incomplete combinations" in {
      assert(errors(none).contains("specify either"))
      assert(errors(none.copy(domain = Some("a"))).contains("--version is required"))
      assert(errors(none.copy(version = Some("1.0.0"))).contains("--domain is required"))
      assert(errors(none.copy(target = Some("t"))).contains("--domain is required"))
      assert(errors(none.copy(domains = Some("*@*"))).contains("--domains requires --zip-output"))
      assert(errors(none.copy(zipOutput = Some("z.zip"))).contains("--zip-output requires --domains"))
    }

    "reject malformed selectors" in {
      assert(errors(none.copy(domains = Some("my.*@1.0.0"), zipOutput = Some("z.zip"))).contains("only a whole domain may be the wildcard"))
    }
  }

  "SchemeSelection.parseSelectors" should {
    "parse every selector form, trimming items" in {
      val parsed = SchemeSelection.parseSelectors(" *@* , my.domain@* ,my.domain@1.0.0,*@2.0.0 ").fold(e => fail(e.toString), _.toList)
      assert(
        parsed.map(s => (s.raw, s.domain, s.version)) == List(
          ("*@*", SelectorComponent.Wildcard, SelectorComponent.Wildcard),
          ("my.domain@*", SelectorComponent.Exact(Pkg(NEList("my", "domain"))), SelectorComponent.Wildcard),
          ("my.domain@1.0.0", SelectorComponent.Exact(Pkg(NEList("my", "domain"))), SelectorComponent.Exact(Version.parse("1.0.0"))),
          ("*@2.0.0", SelectorComponent.Wildcard, SelectorComponent.Exact(Version.parse("2.0.0"))),
        )
      )
    }

    "reject malformed selectors with an explanation for each" in {
      val cases = List(
        ""                 -> "empty selector",
        "a@1.0.0,"         -> "empty selector",
        " , "              -> "empty selector",
        "a"                -> "expected exactly one '@'",
        "a@1.0.0@2.0.0"    -> "expected exactly one '@'",
        "my.*@1.0.0"       -> "only a whole domain may be the wildcard",
        "*@1.*"            -> "only a whole version may be the wildcard",
        "**@*"             -> "only a whole domain may be the wildcard",
        "my domain@1.0.0"  -> "'my domain' is not a valid domain",
        "my.domain @1.0.0" -> "'my.domain ' is not a valid domain",
        "@1.0.0"           -> "'' is not a valid domain",
        "a.@1.0.0"         -> "'a.' is not a valid domain",
        "1a@1.0.0"         -> "'1a' is not a valid domain",
        "a@"               -> "'' is not a valid version",
        "a@one"            -> "'one' is not a valid version",
        "a@01.0.0"         -> "'01.0.0' is not a valid version",
      )
      cases.foreach {
        case (raw, expected) =>
          val message = SchemeSelection.parseSelectors(raw).fold(_.toList.mkString("\n"), s => fail(s"'$raw' parsed as $s"))
          assert(message.contains(expected), s"'$raw': $message")
      }
    }
  }
}
