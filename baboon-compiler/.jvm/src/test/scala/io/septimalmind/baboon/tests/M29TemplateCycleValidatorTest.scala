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

/** PR-29.6 (M29) negative test: matrix #9 — alias cycle through templates.
  *
  * Verifies that a mutual alias cycle (`type A = X[B]; type B = X[A]`) is
  * caught by `BaboonValidator.checkLoops` as
  * `VerificationIssue.ReferentialCyclesFound`, post-typer, post-monomorphisation.
  *
  * After monomorphisation the template instantiations expand to:
  *   data A { f: B }
  *   data B { f: A }
  * Neither A nor B has a termination path, so `checkLoops` rejects the cycle.
  *
  * M29 reuses the existing `ReferentialCyclesFound` diagnostic rather than
  * introducing a dedicated `TemplateInstantiationCycle` typer-side diagnostic
  * (CLAUDE.md §4 simplicity). After monomorphisation the cycle takes the same
  * form as any non-template referential cycle.
  *
  * See `docs/spec/generics.md` §2.5.9.
  */
final class M29TemplateCycleValidatorTest extends M29TemplateCycleValidatorTestBase[Either]

abstract class M29TemplateCycleValidatorTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def makeInput(name: String, body: String): BaboonParser.Input =
    BaboonParser.Input(FSPath.parse(NEString.unsafeFrom(name)), body)

  private def runValidator(manager: BaboonFamilyManager[F], name: String, body: String): F[Nothing, Either[NEList[BaboonIssue], BaboonFamily]] = {
    manager
      .load(List(makeInput(name, body)))
      .map(Right(_): Either[NEList[BaboonIssue], BaboonFamily])
      .catchAll(errs => F.pure(Left(errs): Either[NEList[BaboonIssue], BaboonFamily]))
  }

  private def verificationIssues(issues: NEList[BaboonIssue]): List[VerificationIssue] =
    issues.toList.collect { case BaboonIssue.Verification(vi) => vi }

  "M29 template cycle validator (matrix #9)" should {

    /** Spec §2.5.9 worked counter-example: `type A = X[B]; type B = X[A]`.
      *
      * After monomorphisation:
      *   data A { f: B }
      *   data B { f: A }
      * Neither has a termination path → `ReferentialCyclesFound`.
      */
    "reject mutual alias cycle through template (type A = X[B]; type B = X[A]) with ReferentialCyclesFound" in {
      (manager: BaboonFamilyManager[F]) =>
        val model =
          """model m29.cycle.matrix9
            |
            |version "1.0.0"
            |
            |data X[T] { f: T }
            |root type A = X[B]
            |root type B = X[A]
            |""".stripMargin
        for {
          outcome <- runValidator(manager, "m29-cycle-matrix9.baboon", model)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val vi     = verificationIssues(issues)
          assert(
            vi.exists(_.isInstanceOf[VerificationIssue.ReferentialCyclesFound]),
            s"expected ReferentialCyclesFound, got: $vi",
          )
        }
    }
  }
}
