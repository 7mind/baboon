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

final class M19MapKeyEligibilityFrontEndTest extends M19MapKeyEligibilityFrontEndTestBase[Either]

abstract class M19MapKeyEligibilityFrontEndTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

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

  private def assertProducesIssue[T <: VerificationIssue: ClassTag](outcome: Either[NEList[BaboonIssue], BaboonFamily]): Unit = {
    val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
    val vi     = verificationIssues(issues)
    val ct     = implicitly[ClassTag[T]]
    assert(
      vi.exists(ct.runtimeClass.isInstance),
      s"expected ${ct.runtimeClass.getSimpleName}, got: $vi",
    )
  }

  private def assertProducesIneligibleReason[R <: VerificationIssue.IneligibleMapKeyReason: ClassTag](outcome: Either[NEList[BaboonIssue], BaboonFamily]): Unit = {
    val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
    val vi     = verificationIssues(issues)
    val rct    = implicitly[ClassTag[R]]
    val ok = vi.exists {
      case VerificationIssue.IneligibleUserMapKey(_, _, reason, _) => rct.runtimeClass.isInstance(reason)
      case _                                                       => false
    }
    assert(ok, s"expected IneligibleUserMapKey with reason ${rct.runtimeClass.getSimpleName}, got: $vi")
  }

  "M19 map-key validator" should {
    // ---------- negative tests ----------

    "reject multi-field non-id wrapper as map key" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/m19-bad/multi-field-non-id-wrapper.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesIneligibleReason[VerificationIssue.IneligibleMapKeyReason.MultiFieldNonIdWrapper](outcome)
        }
    }

    "reject single-field wrapper of collection (lst) as map key" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/m19-bad/collection-wrapper.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesIneligibleReason[VerificationIssue.IneligibleMapKeyReason.CollectionField](outcome)
        }
    }

    "reject single-field wrapper of opt as map key" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/m19-bad/option-wrapper.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesIneligibleReason[VerificationIssue.IneligibleMapKeyReason.OptionField](outcome)
        }
    }

    "reject ADT as map key" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/m19-bad/adt-key.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesIneligibleReason[VerificationIssue.IneligibleMapKeyReason.IneligibleUserType](outcome)
        }
    }

    // Cyclic wrappers are caught by the existing checkLoops machinery (Q-M19-1: reuse existing
    // adjacency-list cycle detection). The IneligibleMapKeyReason.CyclicWrapper arm in the
    // validator's eligibility recursion is a defensive safeguard for the local recursion;
    // in practice a real cycle is reported as ReferentialCyclesFound by checkLoops, which
    // runs earlier in the validator chain.
    "reject mutually-referencing data wrappers as map key (caught by existing checkLoops)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/m19-bad/cyclic-wrapper.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesIssue[VerificationIssue.ReferentialCyclesFound](outcome)
        }
    }

    "reject single-field wrapper that has contracts as map key" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/m19-bad/wrapper-with-contracts.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesIneligibleReason[VerificationIssue.IneligibleMapKeyReason.WrapperWithContracts](outcome)
        }
    }

    "reject wrapper grounding in float (Q-M19-2 asymmetric override)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/m19-bad/float-wrapper.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesIneligibleReason[VerificationIssue.IneligibleMapKeyReason.FloatWrapper](outcome)
        }
    }

    "reject id used as map key when owner derives json but id does not (Q-FU-1)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/m19-bad/id-as-key-without-json-derived.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesIssue[VerificationIssue.MapKeyMissingDerivation](outcome)
        }
    }

    "reject wrapper used as map key when owner derives ueba but wrapper does not (Q-FU-1)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/m19-bad/wrapper-as-key-without-ueba-derived.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesIssue[VerificationIssue.MapKeyMissingDerivation](outcome)
        }
    }

    // D01: chain-based Q-FU-1 — derivation parity must apply to every user type in the
    // wrapper chain, not just the immediate key type.
    "reject nested wrapper when outer derives json but inner wrapper does not (D01 chain fix)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/m19-bad/nested-wrapper-inner-without-json-derived.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesIssue[VerificationIssue.MapKeyMissingDerivation](outcome)
        }
    }

    "reject nested wrapper when outer derives ueba but inner wrapper does not (D01 chain fix)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/m19-bad/nested-id-inner-without-ueba-derived.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesIssue[VerificationIssue.MapKeyMissingDerivation](outcome)
        }
    }

    // ---------- positive tests ----------

    "accept direct single-field wrapper as map key" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon/m19-ok/direct-wrapper.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assert(outcome.isRight, s"expected clean validation, got: $outcome")
        }
    }

    "accept wrapper-via-alias as map key" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon/m19-ok/wrapper-via-alias.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assert(outcome.isRight, s"expected clean validation, got: $outcome")
        }
    }

    "accept nested wrapper-of-wrapper as map key" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon/m19-ok/nested-wrapper-of-wrapper.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assert(outcome.isRight, s"expected clean validation, got: $outcome")
        }
    }

    "accept wrapper-around-foreign as map key (Q-M19-7)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon/m19-ok/wrapper-around-foreign.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assert(outcome.isRight, s"expected clean validation, got: $outcome")
        }
    }

    "accept single-primitive-field id as map key" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon/m19-ok/single-primitive-id.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assert(outcome.isRight, s"expected clean validation, got: $outcome")
        }
    }

    "accept multi-field id as map key (Q-M19-6)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon/m19-ok/multi-field-id.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assert(outcome.isRight, s"expected clean validation, got: $outcome")
        }
    }

    // D02: isIdentifier branch must take priority over contracts.nonEmpty.
    // An id type that implements a contract (via `is ContractName`) must still be
    // accepted as a valid map key.
    "accept id-with-contracts as map key (D02: isIdentifier before contracts.nonEmpty)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon/m19-ok/id-with-contracts.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assert(outcome.isRight, s"expected clean validation, got: $outcome")
        }
    }
  }
}
