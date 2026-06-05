package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, VerificationIssue}
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.BaboonFamilyManager
import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.reflect.TagKK

final class ContractFieldValidatorTest extends ContractFieldValidatorTestBase[Either]

abstract class ContractFieldValidatorTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def makeInput(name: String, body: String): BaboonParser.Input =
    BaboonParser.Input(FSPath.parse(NEString.unsafeFrom(name)), body)

  private def loadFamily(manager: BaboonFamilyManager[F], name: String, body: String): F[NEList[BaboonIssue], BaboonFamily] = {
    manager.load(List(makeInput(name, body)))
  }

  private def runValidator(manager: BaboonFamilyManager[F], name: String, body: String): F[Nothing, Either[NEList[BaboonIssue], BaboonFamily]] = {
    loadFamily(manager, name, body).map(Right(_): Either[NEList[BaboonIssue], BaboonFamily]).catchAll {
      errs => F.pure(Left(errs): Either[NEList[BaboonIssue], BaboonFamily])
    }
  }

  private def verificationIssues(issues: NEList[BaboonIssue]): List[VerificationIssue] = {
    issues.toList.collect { case BaboonIssue.Verification(vi) => vi }
  }

  "data-type field validator — D2" should {

    "reject a plain field whose resolved user type is a contract" in {
      // D2: `convertTpe` admits a contract id as a plain field type (it extends
      // TypeId.Scalar) and no validator rejected it, so a DTO field referencing a
      // contract reached codegen (MCP emitter would emit a dangling `#/$defs/<C>`).
      (manager: BaboonFamilyManager[F]) =>
        val model =
          """model contract.field.bad.plain
            |
            |version "1.0.0"
            |
            |contract C {
            |  x: i32
            |}
            |
            |root data Holder {
            |  f: C
            |}
            |""".stripMargin
        for {
          outcome <- runValidator(manager, "contract-field-bad-plain.baboon", model)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val vi     = verificationIssues(issues)
          assert(
            vi.exists(_.isInstanceOf[VerificationIssue.DataTypeExpectedField]),
            s"expected DataTypeExpectedField, got: $vi",
          )
        }
    }

    "reject a contract referenced through a collection element" in {
      (manager: BaboonFamilyManager[F]) =>
        val model =
          """model contract.field.bad.coll
            |
            |version "1.0.0"
            |
            |contract C {
            |  x: i32
            |}
            |
            |root data Holder {
            |  f: lst[C]
            |}
            |""".stripMargin
        for {
          outcome <- runValidator(manager, "contract-field-bad-coll.baboon", model)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val vi     = verificationIssues(issues)
          assert(
            vi.exists(_.isInstanceOf[VerificationIssue.DataTypeExpectedField]),
            s"expected DataTypeExpectedField, got: $vi",
          )
        }
    }

    "accept a plain field referencing a DTO (control)" in {
      (manager: BaboonFamilyManager[F]) =>
        val model =
          """model contract.field.ok.dto
            |
            |version "1.0.0"
            |
            |data D {
            |  x: i32
            |}
            |
            |root data Holder {
            |  f: D
            |}
            |""".stripMargin
        for {
          outcome <- runValidator(manager, "contract-field-ok-dto.baboon", model)
        } yield {
          assert(outcome.isRight, s"expected clean validation, got: $outcome")
        }
    }

    "still accept a DTO inheriting a contract via `is` (contract not used as a field type)" in {
      (manager: BaboonFamilyManager[F]) =>
        val model =
          """model contract.field.ok.inherit
            |
            |version "1.0.0"
            |
            |contract C {
            |  x: i32
            |}
            |
            |root data Holder {
            |  is C
            |}
            |""".stripMargin
        for {
          outcome <- runValidator(manager, "contract-field-ok-inherit.baboon", model)
        } yield {
          assert(outcome.isRight, s"expected clean validation, got: $outcome")
        }
    }
  }
}
