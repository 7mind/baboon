package io.septimalmind.baboon.tests

import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TyperIssue}
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzResources
import izumi.reflect.TagKK

import java.nio.file.Path
import scala.reflect.ClassTag

final class ServiceFrontEndTest extends ServiceFrontEndTestBase[Either]

abstract class ServiceFrontEndTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

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

  private def typerIssues(issues: NEList[BaboonIssue]): List[TyperIssue] = {
    issues.toList.collect { case BaboonIssue.Typer(ti) => ti }
  }

  private def assertProducesTyperIssue[T <: TyperIssue: ClassTag](outcome: Either[NEList[BaboonIssue], BaboonFamily]): Unit = {
    val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
    val ti     = typerIssues(issues)
    val ct     = implicitly[ClassTag[T]]
    assert(
      ti.exists(ct.runtimeClass.isInstance),
      s"expected ${ct.runtimeClass.getSimpleName}, got: $ti",
    )
  }

  "service front-end" should {
    // Two paths reject duplicate-input declarations, depending on the surface syntax:
    //   - Inline `data in {}` blocks: ScopeBuilder rejects with NonUniqueScope before convertService runs
    //     (each inline struct registers a ScopeName(in); duplicates collide in the scope tree).
    //   - Ref form `in = SomeType`: scope is per-method, no duplicate registration; convertService's
    //     ServiceMultipleInputs defensive check catches it (mirrors ServiceMultipleOutputs).
    // Both cases are tested below to lock in the rejection coverage discovered during PR-53.

    "reject service method with multiple `data in = X` ref-form inputs (PR-47-D01, ServiceMultipleInputs path)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/service-bad/multiple-inputs-ref.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesTyperIssue[TyperIssue.ServiceMultipleInputs](outcome)
        }
    }

    "reject service method with multiple inline `data in {}` blocks (PR-47-D01, NonUniqueScope path)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/service-bad/multiple-inputs-inline.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesTyperIssue[TyperIssue.NonUniqueScope](outcome)
        }
    }
  }
}
