package io.septimalmind.baboon.translator

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.fundamentals.collections.nonempty.NEList

case class OutputFile(content: String, product: CompilerProduct)
case class Sources(files: Map[String, OutputFile])

trait BaboonAbstractTranslator[F[+_, +_]] {
  def translate(family: BaboonFamily): F[NEList[BaboonIssue.TranslationIssue], Sources]
}
