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

final class IdentifierValidatorTest extends IdentifierValidatorTestBase[Either]

abstract class IdentifierValidatorTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

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

  private def verificationIssues(issues: NEList[BaboonIssue]): List[VerificationIssue] = {
    issues.toList.collect { case BaboonIssue.Verification(vi) => vi }
  }

  private def assertProducesVerificationIssue[T <: VerificationIssue: ClassTag](outcome: Either[NEList[BaboonIssue], BaboonFamily]): Unit = {
    val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
    val vi     = verificationIssues(issues)
    val ct     = implicitly[ClassTag[T]]
    assert(
      vi.exists(ct.runtimeClass.isInstance),
      s"expected ${ct.runtimeClass.getSimpleName}, got: $vi",
    )
  }

  "identifier validator" should {
    "reject identifier with collection field (lst)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/identifier-bad/id-with-collection.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesVerificationIssue[VerificationIssue.IdentifierFieldCollection](outcome)
        }
    }

    "reject identifier with float field (f64)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/identifier-bad/id-with-float.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesVerificationIssue[VerificationIssue.IdentifierFieldFloatType](outcome)
        }
    }

    "reject identifier referencing a data DTO (not an id)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/identifier-bad/id-with-data-ref.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesVerificationIssue[VerificationIssue.IdentifierFieldUserNotIdentifier](outcome)
        }
    }

    "reject identifier referencing an ADT (not an id)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/identifier-bad/id-with-adt-ref.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesVerificationIssue[VerificationIssue.IdentifierFieldUserNotIdentifier](outcome)
        }
    }

    "reject identifier with any field" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/identifier-bad/id-with-any.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesVerificationIssue[VerificationIssue.IdentifierFieldAny](outcome)
        }
    }

    "reject identifier with float field (f32)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/identifier-bad/id-with-f32.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesVerificationIssue[VerificationIssue.IdentifierFieldFloatType](outcome)
        }
    }

    "reject identifier with float field (f128)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/identifier-bad/id-with-f128.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesVerificationIssue[VerificationIssue.IdentifierFieldFloatType](outcome)
        }
    }

    "reject identifier referencing an enum (not an id)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/identifier-bad/id-with-enum-ref.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesVerificationIssue[VerificationIssue.IdentifierFieldUserNotIdentifier](outcome)
        }
    }

    "reject identifier referencing a contract (not an id)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/identifier-bad/id-with-contract-ref.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesVerificationIssue[VerificationIssue.IdentifierFieldUserNotIdentifier](outcome)
        }
    }

    "reject identifier referencing a foreign type (not an id)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/identifier-bad/id-with-foreign-ref.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesVerificationIssue[VerificationIssue.IdentifierFieldUserNotIdentifier](outcome)
        }
    }

    "reject identifier with collection field (opt)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/identifier-bad/id-with-opt.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesVerificationIssue[VerificationIssue.IdentifierFieldCollection](outcome)
        }
    }

    "reject identifier with collection field (set)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/identifier-bad/id-with-set.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesVerificationIssue[VerificationIssue.IdentifierFieldCollection](outcome)
        }
    }

    "reject identifier with collection field (map)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/identifier-bad/id-with-map.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesVerificationIssue[VerificationIssue.IdentifierFieldCollection](outcome)
        }
    }

    // Cycle detection for identifiers reuses the existing checkLoops machinery (plan §2.5).
    // Two mutually-referencing `id` types form a cycle that terminatesLoop cannot break,
    // so ReferentialCyclesFound is emitted — no new cycle-specific issue type is needed.
    "reject mutually-referencing identifiers with ReferentialCyclesFound (existing checkLoops)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/identifier-bad/id-cycle.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesVerificationIssue[VerificationIssue.ReferentialCyclesFound](outcome)
        }
    }
  }
}
