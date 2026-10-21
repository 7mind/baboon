package io.septimalmind.baboon.tests

import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.explore.RandomJsonGenerator
import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.scheme.BaboonSchemeRenderer
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.{BaboonEnquiries, BaboonFamilyManager, BaboonRuntimeCodec}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
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

    // https://github.com/7mind/baboon/issues/99: a contract's fields sit where its `is` is declared,
    // so rendering every `is` ahead of the fields moved them to the front on reload
    "keep the field order, and so the UEBA layout, of DTOs whose `is` is not their first member" in {
      (manager: BaboonFamilyManager[F], renderer: BaboonSchemeRenderer, codec: BaboonRuntimeCodec[F]) =>
        val pkg     = Pkg(NEList("scheme", "isorder"))
        val version = Version.parse("1.0.0")
        val source =
          """model scheme.isorder
            |
            |version "1.0.0"
            |
            |contract Counted {
            |  count: i32
            |}
            |
            |contract Extra {
            |  is Counted
            |  note: str
            |}
            |
            |contract Keyed {
            |  key: i64
            |}
            |
            |root data After {
            |  a: i32
            |  is Counted
            |}
            |
            |root data Between {
            |  a: i32
            |  is Counted
            |  b: str
            |  is Keyed
            |  c: bit
            |}
            |
            |root data Extended {
            |  x: u08
            |  is Extra
            |  y: u16
            |}
            |
            |root adt Shape {
            |  is Keyed
            |  data Circle {
            |    r: f64
            |    is Counted
            |    n: str
            |  }
            |  data Square {
            |    side: i32
            |  }
            |}
            |""".stripMargin
        for {
          family <- manager.load(List(BaboonParser.Input(FSPath.parse(NEString.unsafeFrom("isorder.baboon")), source)))
          reloaded <- checkRoundtrip(renderer, manager, family, pkg, version)
          samples = {
            val domain    = family.domains.toMap(pkg).versions.toMap(version)
            val generator = new RandomJsonGenerator(domain, new BaboonEnquiries.BaboonEnquiriesImpl)
            domain.defs.meta.nodes.values.collect {
              case member @ DomainMember.User(_, _: Typedef.Dto | _: Typedef.Adt, _, _) =>
                member.id.toString -> generator.generate(member).fold(e => throw new AssertionError(e), identity)
            }.toList
          }
          encodings <- F.traverse(samples) {
            case (id, json) =>
              for {
                fromSource   <- F.leftMap(codec.encode(family, pkg, version, id, json, indexed = false))(NEList(_))
                fromReloaded <- F.leftMap(codec.encode(reloaded, pkg, version, id, json, indexed = false))(NEList(_))
              } yield (id, json, fromSource, fromReloaded)
          }
        } yield {
          assert(samples.size == 6, samples.map(_._1).toString)
          val differing = encodings.collect { case (id, json, a, b) if a != b => s"$id: ${json.noSpaces}" }
          assert(differing.isEmpty, s"encoded differently after a render/reload cycle:\n${differing.mkString("\n")}")
        }
    }

    "preserve a declared type rename through a render/reload cycle" in {
      (loader: BaboonLoader[F], manager: BaboonFamilyManager[F], renderer: BaboonSchemeRenderer) =>
        // A type rename lives in `Domain.renames`, not on the member, and used to be dropped in
        // silence: a rendered scheme turned the rename into a remove+add for anyone who fed it back
        // in. The idempotence check above cannot see that on its own -- both renders omitted it --
        // so the clause is asserted explicitly here, and the reloaded model must carry the rename.
        val pkg     = Pkg(NEList("rename", "ns"))
        val version = Version.parse("2.0.0")
        for {
          family <- loadRenamePkg(loader)
          rendered = renderer
            .render(family, pkg, version).fold(e => throw new AssertionError(s"render failed: $e"), identity)
          _ = assert(rendered.contains("was["), s"rendered scheme lost the type rename:\n$rendered")
          reloaded <- manager.load(
            List(BaboonParser.Input(FSPath.parse(NEString.unsafeFrom("rt.baboon")), rendered))
          )
        } yield {
          val domain2 = reloaded.domains.toMap(pkg).versions.toMap(version)
          assert(domain2.renames.nonEmpty, s"reloaded model has no renames; rendered scheme was:\n$rendered")
        }
    }
  }

  private def checkRoundtrip(
    renderer: BaboonSchemeRenderer,
    manager: BaboonFamilyManager[F],
    family: BaboonFamily,
    pkg: Pkg,
    version: Version,
  ): F[NEList[BaboonIssue], BaboonFamily] = {
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
      val before  = wireShapes(family.domains.toMap(pkg).versions.toMap(version))
      val after   = wireShapes(family2.domains.toMap(pkg).versions.toMap(version))
      val changed = before.collect { case (id, shape) if !after.get(id).contains(shape) => s"$id: $shape -> ${after.get(id)}" }
      assert(changed.isEmpty, s"Wire shapes changed by a render/reload cycle for $pkg@$version:\n${changed.mkString("\n")}\n---RENDERED---\n$rendered1")
      assert(rendered1 == rendered2, s"Roundtrip mismatch for $pkg@$version:\n---FIRST---\n$rendered1\n---SECOND---\n$rendered2")
      family2
    }
  }

  /** What the wire depends on, per user type: field, branch and member order. */
  private def wireShapes(domain: Domain): Map[TypeId, List[String]] = {
    domain.defs.meta.nodes.collect {
      case (id, DomainMember.User(_, d: Typedef.Dto, _, _))  => id -> d.fields.map(f => s"${f.name.name}: ${f.tpe}")
      case (id, DomainMember.User(_, a: Typedef.Adt, _, _))  => id -> a.members.toList.map(_.toString)
      case (id, DomainMember.User(_, e: Typedef.Enum, _, _)) => id -> e.members.toList.map(m => s"${m.name}=${m.const}")
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
