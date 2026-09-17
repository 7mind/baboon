package io.septimalmind.baboon.tests

import io.septimalmind.baboon.{BaboonLoader, CompilerProduct}
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.translator.DomainProductTranslator
import io.septimalmind.baboon.typer.model.{BaboonFamily, Domain, DomainMember, TypeId}
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.reflect.TagKK

final class DomainProductTranslatorTest extends DomainProductTranslatorTestBase[Either]

abstract class DomainProductTranslatorTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {
  private def domain(family: BaboonFamily): Domain = family.domains.toMap.values.head.versions.toMap.values.head

  "Domain product translation" should {
    "preserve member iteration order and exclude builtins" in {
      (loader: BaboonLoader[F]) =>
        for {
          family  <- loadPkg(loader)
          selected = domain(family)
          actual <- DomainProductTranslator.translate[F, TypeId.User](
            selected,
            Set(CompilerProduct.Definition),
            CompilerProduct.Definition,
            member => F.pure(List(member.id)),
          )
        } yield {
          val expected = selected.defs.meta.nodes.toList.collect { case (_, member: DomainMember.User) => member.id }
          assert(expected.nonEmpty)
          assert(actual == expected)
        }
    }

    "leave disabled product callbacks unevaluated" in {
      (loader: BaboonLoader[F]) =>
        for {
          family <- loadPkg(loader)
          actual <- DomainProductTranslator.translate[F, TypeId.User](
            domain(family),
            Set(CompilerProduct.Definition),
            CompilerProduct.Fixture,
            _ => throw new AssertionError("disabled product callback evaluated"),
          )
        } yield assert(actual.isEmpty)
    }

    "accumulate every member failure through the existing error channel" in {
      (loader: BaboonLoader[F]) =>
        for {
          family  <- loadPkg(loader)
          selected = domain(family)
          actual <- DomainProductTranslator
            .translate[F, TypeId.User](
              selected,
              Set(CompilerProduct.Test),
              CompilerProduct.Test,
              _ => F.fail(BaboonIssue.of(TranslationIssue.TranslationBug())),
            ).map(Right(_): Either[NEList[BaboonIssue], List[TypeId.User]]).catchAll(errors => F.pure(Left(errors)))
        } yield {
          val expectedCount = selected.defs.meta.nodes.values.count(_.isInstanceOf[DomainMember.User])
          val errors        = actual.left.getOrElse(throw new AssertionError("member failures were lost"))
          assert(expectedCount > 0)
          assert(errors.toList.size == expectedCount)
        }
    }
  }
}
