package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.{CSTarget, ScTarget}
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.csharp.{CSCodecFixtureTranslator, CSCodecTestsTranslator, CSCodecTranslator, CSDefnTranslator, CSDomainTreeTools, CSFileTools, CSTreeTools, CSTypeTranslator}
import io.septimalmind.baboon.typer.TypeInfo
import io.septimalmind.baboon.typer.model.{BaboonEvolution, Domain, DomainMember}
import izumi.functional.bio.Applicative2
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree

trait ScDefnTranslator[F[+_, +_]] {
  def translate(defn: DomainMember.User): F[NEList[BaboonIssue.TranslationIssue], List[ScDefnTranslator.OutputExt]]
  def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue.TranslationIssue], List[ScDefnTranslator.Output]]
  def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue.TranslationIssue], List[ScDefnTranslator.Output]]
}

object ScDefnTranslator {
  case class OutputExt(output: Output, codecReg: TextTree[ScValue])

  case class Output(path: String, tree: TextTree[ScValue], pkg: ScValue.ScPackageId, product: CompilerProduct, doNotModify: Boolean = false)

  class CSDefnTranslatorImpl[F[+_, +_]: Applicative2 /* This impl has no errors right now */ ](
    target: ScTarget,
    domain: Domain,
    evo: BaboonEvolution,
    types: TypeInfo,
  ) extends ScDefnTranslator[F] {

    override def translate(defn: DomainMember.User): F[NEList[BaboonIssue.TranslationIssue], List[OutputExt]] = ???

    override def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue.TranslationIssue], List[Output]] = ???

    override def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue.TranslationIssue], List[Output]] = ???
  }
}
