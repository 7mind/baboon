package io.septimalmind.baboon.tests

import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.scheme.BaboonSchemeRenderer
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.BaboonFamilyManager
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.Error2
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzResources
import izumi.reflect.TagKK

final class SchemeRoundtripTest extends SchemeRoundtripTestBase[Either]

abstract class SchemeRoundtripTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  "BaboonSchemeRenderer" should {
    "roundtrip all versions of testpkg.pkg0" in {
      (loader: BaboonLoader[F], manager: BaboonFamilyManager[F], renderer: BaboonSchemeRenderer) =>
        for {
          family <- loadPkg(loader)
          _      <- checkRoundtrip(renderer, manager, family, Pkg(NEList("testpkg", "pkg0")), Version.parse("1.0.0"))
          _      <- checkRoundtrip(renderer, manager, family, Pkg(NEList("testpkg", "pkg0")), Version.parse("2.0.0"))
          _      <- checkRoundtrip(renderer, manager, family, Pkg(NEList("testpkg", "pkg0")), Version.parse("3.0.0"))
        } yield ()
    }

    "roundtrip all versions of rename.ns" in {
      (loader: BaboonLoader[F], manager: BaboonFamilyManager[F], renderer: BaboonSchemeRenderer) =>
        for {
          family <- loadRenamePkg(loader)
          _      <- checkRoundtrip(renderer, manager, family, Pkg(NEList("rename", "ns")), Version.parse("1.0.0"))
          _      <- checkRoundtrip(renderer, manager, family, Pkg(NEList("rename", "ns")), Version.parse("2.0.0"))
        } yield ()
    }
  }

  private def checkRoundtrip(
    renderer: BaboonSchemeRenderer,
    manager: BaboonFamilyManager[F],
    family: BaboonFamily,
    pkg: Pkg,
    version: Version,
  ): F[NEList[BaboonIssue], Unit] = {
    val rendered1 = renderer
      .render(family, pkg, version).fold(
        e => throw new AssertionError(s"First render failed for $pkg@$version: $e"),
        identity,
      )
    val input = BaboonParser.Input(
      FSPath.parse(NEString.unsafeFrom(s"roundtrip-$version.baboon")),
      rendered1,
    )
    for {
      family2 <- manager.load(List(input))
    } yield {
      val rendered2 = renderer
        .render(family2, pkg, version).fold(
          e => throw new AssertionError(s"Second render failed for $pkg@$version: $e"),
          identity,
        )
      assert(rendered1 == rendered2, s"Roundtrip mismatch for $pkg@$version:\n---FIRST---\n$rendered1\n---SECOND---\n$rendered2")
    }
  }

  private def loadRenamePkg(loader: BaboonLoader[F]): F[NEList[BaboonIssue], BaboonFamily] = {
    val root = IzResources
      .getPath("baboon/rename-ns")
      .get
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons = IzFiles
      .walk(root.toFile)
      .toList
      .filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
    loader.load(baboons)
  }
}
