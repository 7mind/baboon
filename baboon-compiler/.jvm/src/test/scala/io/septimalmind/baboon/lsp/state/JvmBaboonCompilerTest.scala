package io.septimalmind.baboon.lsp.state

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.{FSPath, InputPointer, RawContent, RawDomain, RawHeader, RawNodeMeta, RawVersion}
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, ParserIssue}
import io.septimalmind.baboon.typer.BaboonFamilyManager
import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.functional.bio.Error2
import izumi.functional.bio.impl.BioEither
import izumi.functional.quasi.QuasiIORunner
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.util.concurrent.atomic.AtomicInteger

class JvmBaboonCompilerTest extends AnyWordSpec with Matchers {
  private type EitherF[+E, +A] = Either[E, A]

  private implicit val error2: Error2[EitherF] = BioEither

  "JvmBaboonCompiler.reload" should {
    "reuse cached parses and only parse changed inputs" in {
      val parser = new CountingParser[EitherF]
      val manager = new RecordingManager[EitherF]
      val runner = new EitherRunner
      val compiler = new JvmBaboonCompiler[EitherF](parser, manager, runner)

      val inputA = BaboonParser.Input(path("a.baboon"), "content-a")
      val inputB = BaboonParser.Input(path("b.baboon"), "content-b")
      val inputs = Seq(inputA, inputB)

      compiler.reload(inputs)
      parser.parsedCount shouldBe 2
      manager.lastReloadInputs.map(pathOf) shouldBe inputs.map(_.path).toList

      compiler.reload(inputs)
      parser.parsedCount shouldBe 2
      manager.lastReloadInputs.map(pathOf) shouldBe inputs.map(_.path).toList

      val updated = Seq(inputA.copy(content = "content-a-2"), inputB)
      compiler.reload(updated)
      parser.parsedCount shouldBe 3
      manager.lastReloadInputs.map(pathOf) shouldBe updated.map(_.path).toList
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

  private final class CountingParser[F[+_, +_]: Error2] extends BaboonParser[F] {
    private val counter = new AtomicInteger(0)

    def parsedCount: Int = counter.get()

    override def parse(input: BaboonParser.Input): F[NEList[BaboonIssue], RawDomain] = {
      counter.incrementAndGet()
      val F = implicitly[Error2[F]]
      F.pure(makeDomain(input.path))
    }

    private def makeDomain(path: FSPath): RawDomain = {
      val meta = RawNodeMeta(InputPointer.JustFile(path))
      RawDomain(
        RawHeader(meta, Seq("test")),
        RawVersion(meta, "1.0.0"),
        None,
        RawContent(Seq.empty, Seq.empty),
      )
    }
  }

  private final class RecordingManager[F[+_, +_]: Error2] extends BaboonFamilyManager[F] {
    @volatile private var last: List[BaboonParser.ReloadInput] = List.empty

    def lastReloadInputs: List[BaboonParser.ReloadInput] = last

    override def load(
      definitions: List[BaboonParser.Input]
    ): F[NEList[BaboonIssue], BaboonFamily] = {
      val F = implicitly[Error2[F]]
      F.fail(NEList(BaboonIssue.Parser(ParserIssue.IncludeNotFound(s"unexpected load: ${definitions.size}"))))
    }

    override def reload(
      definitions: List[BaboonParser.ReloadInput]
    ): F[NEList[BaboonIssue], BaboonFamily] = {
      last = definitions
      val F = implicitly[Error2[F]]
      F.fail(NEList(BaboonIssue.Parser(ParserIssue.IncludeNotFound("expected failure"))))
    }
  }
}
