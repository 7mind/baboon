package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model._
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model._
import io.septimalmind.baboon.util.{BLogger, FileContentProvider}
import io.septimalmind.baboon.validator.BaboonValidator
import izumi.functional.bio.Error2
import izumi.functional.bio.ParallelErrorAccumulatingOps2
import izumi.functional.bio.impl.BioEither
import izumi.functional.bio.unsafe.MaybeSuspend2
import izumi.fundamentals.collections.nonempty.{NEList, NEMap, NEString}
import izumi.fundamentals.graphs.DG
import izumi.fundamentals.graphs.struct.AdjacencyPredList
import izumi.fundamentals.graphs.GraphMeta
import izumi.fundamentals.graphs.tools.cycles.LoopDetector
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BaboonFamilyManagerIncrementalTest extends AnyWordSpec with Matchers {
  private type EitherF[+E, +A] = Either[E, A]
  private implicit val error2: Error2[EitherF] = BioEither
  private implicit val maybeSuspend2: MaybeSuspend2[EitherF] = new MaybeSuspend2[EitherF]
  private implicit val parallel2: ParallelErrorAccumulatingOps2[EitherF] = new ParallelErrorAccumulatingOps2[EitherF] {
    override def InnerF: Error2[EitherF] = BioEither
    override def parTraverse[E, A, B](l: Iterable[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      InnerF.traverse(l)(f)
    }
    override def parTraverseN[E, A, B](maxConcurrent: Int)(l: Iterable[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      parTraverse(l)(f)
    }
    override def parTraverseNCore[E, A, B](l: Iterable[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      parTraverse(l)(f)
    }
    override def zipWithPar[E, A, B, C](fa: Either[E, A], fb: Either[E, B])(f: (A, B) => C): Either[E, C] = {
      InnerF.map2(fa, fb)(f)
    }
  }

  "BaboonFamilyManager.reload" should {
    "retype only affected versions and later versions of the same domain" in {
      val parser = new TestParser[EitherF]
      val typer = new CountingTyper[EitherF]
      val comparator = new TestComparator[EitherF]
      val validator = new NoopValidator[EitherF]
      val manager = new BaboonFamilyManager.BaboonFamilyManagerImpl[EitherF](
        parser,
        typer,
        comparator,
        BLogger.Noop,
        validator,
        FileContentProviderNoop,
      )

      val inputV1 = input("/test/foo-1.baboon", "foo|1.0.0")
      val inputV2 = input("/test/foo-2.baboon", "foo|2.0.0")
      val inputOther = input("/test/bar-1.baboon", "bar|1.0.0")

      val initial = manager.load(List(inputV1, inputV2, inputOther)).toOption.get
      typer.reset()

      val updatedV1 = inputV1.copy(content = "foo|1.0.0|changed")
      val reloadInputs = List(updatedV1, inputV2, inputOther).map {
        in => BaboonParser.ReloadInput.Unparsed(in.path, in.content)
      }

      manager.reload(Some(initial), reloadInputs).toOption.get

      typer.processedKeys shouldBe Set(
        DomainKey("foo", "1.0.0"),
        DomainKey("foo", "2.0.0"),
      )
    }

    "retype only the changed version when no later versions exist" in {
      val parser = new TestParser[EitherF]
      val typer = new CountingTyper[EitherF]
      val comparator = new TestComparator[EitherF]
      val validator = new NoopValidator[EitherF]
      val manager = new BaboonFamilyManager.BaboonFamilyManagerImpl[EitherF](
        parser,
        typer,
        comparator,
        BLogger.Noop,
        validator,
        FileContentProviderNoop,
      )

      val inputV1 = input("/test/foo-1.baboon", "foo|1.0.0")
      val initial = manager.load(List(inputV1)).toOption.get
      typer.reset()

      val updatedV1 = inputV1.copy(content = "foo|1.0.0|changed")
      val reloadInputs = List(updatedV1).map {
        in => BaboonParser.ReloadInput.Unparsed(in.path, in.content)
      }

      manager.reload(Some(initial), reloadInputs).toOption.get

      typer.processedKeys shouldBe Set(
        DomainKey("foo", "1.0.0"),
      )
    }
  }

  private def input(path: String, content: String): BaboonParser.Input = {
    BaboonParser.Input(FSPath.parse(NEString.unsafeFrom(path)), content)
  }

  private object FileContentProviderNoop extends FileContentProvider {
    override def read(path: FSPath): Option[String] = None
  }

  private final class TestParser[F[+_, +_]: Error2] extends BaboonParser[F] {
    override def parse(input: BaboonParser.Input): F[NEList[BaboonIssue], RawDomain] = {
      val F = implicitly[Error2[F]]
      val parts = input.content.split("\\|").toList
      val pkg = parts.headOption.getOrElse("default")
      val version = parts.drop(1).headOption.getOrElse("1.0.0")
      val meta = RawNodeMeta(InputPointer.JustFile(input.path))
      val header = RawHeader(meta, pkg.split("\\.").toSeq)
      val rawVersion = RawVersion(meta, version)
      val domain = RawDomain(header, rawVersion, None, RawContent(Seq.empty, Seq.empty))
      F.pure(domain)
    }
  }

  private final class CountingTyper[F[+_, +_]: Error2] extends BaboonTyper[F] {
    @volatile private var seen: Set[DomainKey] = Set.empty

    def processedKeys: Set[DomainKey] = seen

    def reset(): Unit = {
      seen = Set.empty
    }

    override def process(rawDomain: RawDomain): F[NEList[BaboonIssue], Domain] = {
      val F = implicitly[Error2[F]]
      val key = DomainKey(rawDomain.header.name.mkString("."), rawDomain.version.value)
      seen = seen + key
      F.pure(toDomain(rawDomain))
    }

    private def toDomain(rawDomain: RawDomain): Domain = {
      val pkg = Pkg(NEList.unsafeFrom(rawDomain.header.name.toList))
      val version = Version.parse(rawDomain.version.value)
      Domain(
        pkg,
        version,
        emptyGraph(),
        Set.empty,
        Map.empty,
        Set.empty[LoopDetector.Cycles[TypeId]],
        Map.empty,
        Map.empty,
        Set.empty,
        Map.empty,
      )
    }
  }

  private final class TestComparator[F[+_, +_]: Error2] extends BaboonComparator[F] {
    override def evolve(
      pkg: Pkg,
      versions: NEMap[Version, Domain],
    ): F[NEList[BaboonIssue], BaboonEvolution] = {
      val F = implicitly[Error2[F]]
      val latest = versions.keySet.toList.sorted(Version.ordering).last
      F.pure(BaboonEvolution(pkg, latest, Map.empty, Map.empty, Map.empty))
    }
  }

  private final class NoopValidator[F[+_, +_]: Error2] extends BaboonValidator[F] {
    override def validate(
      family: BaboonFamily
    ): F[NEList[BaboonIssue], Unit] = {
      val F = implicitly[Error2[F]]
      F.pure(())
    }
  }

  private def emptyGraph(): DG[TypeId, DomainMember] = {
    val preds = AdjacencyPredList(Map.empty[TypeId, Set[TypeId]])
    val nodes = GraphMeta(Map.empty[TypeId, DomainMember])
    DG.fromPred(preds, nodes)
  }
}
