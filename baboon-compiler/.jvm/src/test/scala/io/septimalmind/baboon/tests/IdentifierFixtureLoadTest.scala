package io.septimalmind.baboon.tests

import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.model.{BaboonFamily, DomainMember, Typedef}
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzResources

import java.nio.file.Path

/** PR-56 smoke test: loads the identifier-ok fixture through the full
  * parser → typer → validator pipeline and asserts the typed Domain has the
  * expected `Typedef.Dto.isIdentifier` flags set. This proves end-to-end
  * pipeline acceptance of the fixture; emitter correctness is exercised by
  * `IdentifierReprPropertyTest` (runtime helpers + spec) and the
  * `mdl :test-gen-regular-adt :test-scala-regular` Scala-stub gen pipeline.
  */
final class IdentifierFixtureLoadTest extends IdentifierFixtureLoadTestBase[Either]

abstract class IdentifierFixtureLoadTestBase[F[+_, +_]: Error2: BaboonTestModule: izumi.reflect.TagKK]
    extends BaboonTest[F] {

  private def resolveBaboonFiles(resourcePath: String): List[Path] = {
    val root = IzResources
      .getPath(resourcePath)
      .getOrElse(throw new AssertionError(s"resource not found: $resourcePath"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    if (root.toFile.isDirectory) {
      IzFiles
        .walk(root.toFile)
        .toList
        .filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
    } else {
      List(root)
    }
  }

  private def runPipeline(loader: BaboonLoader[F], paths: List[Path]): F[Nothing, Either[NEList[BaboonIssue], BaboonFamily]] = {
    loader.load(paths).map(Right(_): Either[NEList[BaboonIssue], BaboonFamily]).catchAll {
      errs => F.pure(Left(errs): Either[NEList[BaboonIssue], BaboonFamily])
    }
  }

  "identifier-ok fixture" should {
    "load successfully and mark all `id` types as Typedef.Dto with isIdentifier=true" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon/identifier-ok/identifiers.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          val fam = outcome match {
            case Right(f)   => f
            case Left(errs) => throw new AssertionError(s"expected fixture to load cleanly; got: $errs")
          }
          // pull the (sole) loaded domain
          val (_, lineage) = fam.domains.toSeq.head
          val (_, domain)  = lineage.versions.toSeq.head

          val idDtos = domain.defs.meta.nodes.values.collect {
            case u: DomainMember.User =>
              u.defn match { case d: Typedef.Dto => d; case _ => null }
          }.filter(_ != null).toList

          val expectedIds = Set("PointId", "Mixed", "UInts", "Outer", "Marker", "LongId", "A", "B", "C", "D")
          val actualIds   = idDtos.filter(_.isIdentifier).map(_.id.name.name).toSet

          assert(
            expectedIds.subsetOf(actualIds),
            s"expected all of $expectedIds to be flagged isIdentifier=true; got actual=$actualIds",
          )

          // No `data` DTOs in this fixture — make sure none accidentally got isIdentifier=true.
          val nonIdDtos = idDtos.filterNot(_.isIdentifier)
          assert(nonIdDtos.isEmpty, s"unexpected non-identifier Dto(s) in fixture: ${nonIdDtos.map(_.id.name.name)}")
        }
    }
  }
}
