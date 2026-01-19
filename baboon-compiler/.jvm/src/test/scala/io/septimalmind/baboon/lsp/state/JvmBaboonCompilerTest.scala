package io.septimalmind.baboon.lsp.state

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, ParserIssue}
import io.septimalmind.baboon.typer.BaboonFamilyManager
import io.septimalmind.baboon.typer.model._
import izumi.functional.bio.Error2
import izumi.functional.bio.impl.BioEither
import izumi.functional.quasi.QuasiIORunner
import izumi.fundamentals.collections.nonempty.{NEList, NEMap, NEString}
import izumi.fundamentals.graphs.DG
import izumi.fundamentals.graphs.struct.AdjacencyPredList
import izumi.fundamentals.graphs.GraphMeta
import izumi.fundamentals.graphs.tools.cycles.LoopDetector
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JvmBaboonCompilerTest extends AnyWordSpec with Matchers {
  private type EitherF[+E, +A] = Either[E, A]

  private implicit val error2: Error2[EitherF] = BioEither

  "JvmBaboonCompiler.reload" should {
    "forward previous family and mark all inputs as unparsed" in {
      val manager = new RecordingManager[EitherF]
      val runner = new EitherRunner
      val compiler = new JvmBaboonCompiler[EitherF](manager, runner)

      val inputA = BaboonParser.Input(path("a.baboon"), "content-a")
      val inputB = BaboonParser.Input(path("b.baboon"), "content-b")
      val inputs = Seq(inputA, inputB)

      compiler.reload(inputs, None)
      manager.lastReloadInputs.map(pathOf) shouldBe inputs.map(_.path).toList
      manager.lastReloadInputs.forall(_.isInstanceOf[BaboonParser.ReloadInput.Unparsed]) shouldBe true

      val emptyFamily = makeFamily()
      compiler.reload(inputs, Some(emptyFamily))
      manager.lastPrevious shouldBe Some(emptyFamily)
    }
  }

  private def path(name: String): FSPath = {
    FSPath.parse(NEString.unsafeFrom(s"/test/$name"))
  }

  private def pathOf(input: BaboonParser.ReloadInput): FSPath = input match {
    case BaboonParser.ReloadInput.Unparsed(path, _) => path
    case BaboonParser.ReloadInput.Parsed(path, _)   => path
  }

  private final class EitherRunner extends QuasiIORunner[Either[Throwable, _]] {
    override def run[A](fa: => Either[Throwable, A]): A = fa match {
      case Right(value) => value
      case Left(error)  => throw error
    }
  }

  private final class RecordingManager[F[+_, +_]: Error2] extends BaboonFamilyManager[F] {
    @volatile private var last: List[BaboonParser.ReloadInput] = List.empty
    @volatile private var prev: Option[BaboonFamily] = None

    def lastReloadInputs: List[BaboonParser.ReloadInput] = last
    def lastPrevious: Option[BaboonFamily] = prev

    override def load(
      definitions: List[BaboonParser.Input]
    ): F[NEList[BaboonIssue], BaboonFamily] = {
      val F = implicitly[Error2[F]]
      F.fail(NEList(BaboonIssue.Parser(ParserIssue.IncludeNotFound(s"unexpected load: ${definitions.size}"))))
    }

    override def reload(
      previous: Option[BaboonFamily],
      definitions: List[BaboonParser.ReloadInput]
    ): F[NEList[BaboonIssue], BaboonFamily] = {
      last = definitions
      prev = previous
      val F = implicitly[Error2[F]]
      F.fail(NEList(BaboonIssue.Parser(ParserIssue.IncludeNotFound("expected failure"))))
    }
  }

  private def makeFamily(): BaboonFamily = {
    val pkg = Pkg(NEList("x"))
    val version = Version.parse("1.0.0")
    val domain = Domain(
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
    val evolution = BaboonEvolution(pkg, version, Map.empty, Map.empty, Map.empty)
    val lineage = BaboonLineage(pkg, NEMap.from(Map(version -> domain)).getOrElse(throw new IllegalStateException("Empty lineage")), evolution)
    BaboonFamily(NEMap.from(Map(pkg -> lineage)).getOrElse(throw new IllegalStateException("Empty family")), BaboonFamilyCache.empty)
  }

  private def emptyGraph(): DG[TypeId, DomainMember] = {
    val preds = AdjacencyPredList(Map.empty[TypeId, Set[TypeId]])
    val nodes = GraphMeta(Map.empty[TypeId, DomainMember])
    DG.fromPred(preds, nodes)
  }
}
