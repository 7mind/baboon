package io.septimalmind.baboon.tests

import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, VerificationIssue}
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzResources
import izumi.reflect.TagKK

import java.nio.file.Path
import scala.reflect.ClassTag

final class AnyFrontEndTest extends AnyFrontEndTestBase[Either]

abstract class AnyFrontEndTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  // Resolve a `.baboon` resource directory or file and return the list of `.baboon` paths to feed
  // to BaboonLoader. Mirrors `BaboonTest.loadPkg` but parameterised by the resource subpath.
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

  // Drive the full pipeline (parser → typer → comparator → validator) and return Either
  // so positive and negative tests can both branch off the result.
  private def runPipeline(loader: BaboonLoader[F], paths: List[Path]): F[Nothing, Either[NEList[BaboonIssue], BaboonFamily]] = {
    loader.load(paths).map(Right(_): Either[NEList[BaboonIssue], BaboonFamily]).catchAll {
      errs => F.pure(Left(errs): Either[NEList[BaboonIssue], BaboonFamily])
    }
  }

  private def verificationIssues(issues: NEList[BaboonIssue]): List[VerificationIssue] = {
    issues.toList.collect { case BaboonIssue.Verification(vi) => vi }
  }

  private def assertProducesIssue[T <: VerificationIssue: ClassTag](outcome: Either[NEList[BaboonIssue], BaboonFamily]): Unit = {
    val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
    val vi     = verificationIssues(issues)
    val ct     = implicitly[ClassTag[T]]
    assert(
      vi.exists(ct.runtimeClass.isInstance),
      s"expected ${ct.runtimeClass.getSimpleName}, got: $vi",
    )
  }

  "any front-end" should {
    "validate the any-ok fixture cleanly" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon/any-ok")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assert(outcome.isRight, s"expected clean validation, got: $outcome")
        }
    }

    "reject any[i32] (underlying not user type)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon/any-bad/underlying-not-user-type.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesIssue[VerificationIssue.AnyUnderlyingNotUserType](outcome)
        }
    }

    "reject any[Foo] without ueba derivation" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon/any-bad/underlying-lacks-ueba.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesIssue[VerificationIssue.AnyUnderlyingLacksUebaDerivation](outcome)
        }
    }

    "reject map[any, str] (any as map key)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon/any-bad/map-key.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesIssue[VerificationIssue.AnyAsMapKey](outcome)
        }
    }

    "reject set[any] (any as set element)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon/any-bad/set-element.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesIssue[VerificationIssue.AnyAsSetElement](outcome)
        }
    }
  }
}
