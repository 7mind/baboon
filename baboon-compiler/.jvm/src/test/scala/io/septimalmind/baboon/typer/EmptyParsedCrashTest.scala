package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model._
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TyperIssue}
import io.septimalmind.baboon.typer.model._
import io.septimalmind.baboon.util.{BLogger, FileContentProvider}
import io.septimalmind.baboon.validator.BaboonValidator
import izumi.functional.bio.Error2
import izumi.functional.bio.ParallelErrorAccumulatingOps2
import izumi.functional.bio.impl.BioEither
import izumi.functional.bio.unsafe.MaybeSuspend2
import izumi.fundamentals.collections.nonempty.{NEList, NEMap}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Regression test for defect D42: loading an empty domain list must yield
  * Left(TyperIssue.EmptyFamily) rather than throw NoSuchElementException from
  * resolveImports (parsed.head.header.meta called on an empty list).
  *
  * RED phase (before the guard): manager.load(Nil) threw
  *   java.util.NoSuchElementException: head of empty list
  *   at scala.collection.immutable.Nil$.head(List.scala:662)
  *   ... BaboonFamilyManager.resolveImports (BaboonFamilyManager.scala:337)
  *
  * GREEN phase (after the guard in buildFamily): manager.load(Nil) returns
  *   Left(NEList(TyperIssue.EmptyFamily(Nil))) via the F error channel.
  */
class EmptyParsedCrashTest extends AnyWordSpec with Matchers {
  private type EitherF[+E, +A] = Either[E, A]
  private implicit val error2: Error2[EitherF]               = BioEither
  private implicit val maybeSuspend2: MaybeSuspend2[EitherF] = new MaybeSuspend2[EitherF]
  private implicit val parallel2: ParallelErrorAccumulatingOps2[EitherF] = new ParallelErrorAccumulatingOps2[EitherF] {
    override def InnerF: Error2[EitherF] = BioEither
    override def parTraverse[E, A, B](l: Iterable[A])(f: A => Either[E, B]): Either[E, List[B]] =
      InnerF.traverse(l)(f)
    override def parTraverseN[E, A, B](maxConcurrent: Int)(l: Iterable[A])(f: A => Either[E, B]): Either[E, List[B]] =
      parTraverse(l)(f)
    override def parTraverseNCore[E, A, B](l: Iterable[A])(f: A => Either[E, B]): Either[E, List[B]] =
      parTraverse(l)(f)
    override def zipWithPar[E, A, B, C](fa: Either[E, A], fb: Either[E, B])(f: (A, B) => C): Either[E, C] =
      InnerF.map2(fa, fb)(f)
  }

  private def makeManager(): BaboonFamilyManager.BaboonFamilyManagerImpl[EitherF] =
    new BaboonFamilyManager.BaboonFamilyManagerImpl[EitherF](
      new NopParser[EitherF],
      new NopTyper[EitherF],
      new NopComparator[EitherF],
      BLogger.Noop,
      new NopValidator[EitherF],
      NopFileContentProvider,
      new AdtDeltaMaterializer[EitherF],
    )

  "BaboonFamilyManager.load" should {
    "return Left(EmptyFamily) when called with an empty domain list" in {
      // D42 fix: before the guard this call threw NoSuchElementException at
      // resolveImports (parsed.head.header.meta on an empty list).
      // After the guard in buildFamily the F error channel carries EmptyFamily.
      val result = makeManager().load(Nil)

      result shouldBe a[Left[_, _]]

      val errors = result.swap.toOption.get
      errors.toList should have length 1
      // BaboonIssue.of wraps TyperIssue into BaboonIssue.Typer
      errors.head shouldBe a[BaboonIssue.Typer]

      val emptyFamily = errors.head.asInstanceOf[BaboonIssue.Typer].issue.asInstanceOf[TyperIssue.EmptyFamily]
      emptyFamily.input shouldBe Nil
    }
  }

  // Minimal stubs — identical in structure to BaboonFamilyManagerIncrementalTest.
  // The stubs for parser/typer/comparator are never invoked when load(Nil) is called
  // because the guard in buildFamily short-circuits before any parsing or typing.

  private object NopFileContentProvider extends FileContentProvider {
    override def read(path: FSPath): Option[String] = None
  }

  private final class NopParser[F[+_, +_]] extends BaboonParser[F] {
    override def parse(input: BaboonParser.Input): F[NEList[BaboonIssue], RawDomain] =
      sys.error("NopParser.parse should not be called")
  }

  private final class NopTyper[F[+_, +_]] extends BaboonTyper[F] {
    override def process(rawDomain: RawDomain): F[NEList[BaboonIssue], Domain] =
      sys.error("NopTyper.process should not be called")
  }

  private final class NopComparator[F[+_, +_]] extends BaboonComparator[F] {
    override def evolve(pkg: Pkg, versions: NEMap[Version, Domain]): F[NEList[BaboonIssue], BaboonEvolution] =
      sys.error("NopComparator.evolve should not be called")
    override def compare(last: Domain, prev: Domain): F[NEList[BaboonIssue], BaboonDiff] =
      sys.error("NopComparator.compare should not be called")
  }

  private final class NopValidator[F[+_, +_]: Error2] extends BaboonValidator[F] {
    override def validate(family: BaboonFamily): F[NEList[BaboonIssue], Unit] =
      implicitly[Error2[F]].pure(())
  }
}
