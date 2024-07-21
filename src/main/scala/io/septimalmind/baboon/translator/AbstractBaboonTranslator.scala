package io.septimalmind.baboon.translator

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.fundamentals.collections.nonempty.NEList

case class OutputFile(content: String, isTest: Boolean)
case class Sources(files: Map[String, OutputFile])

trait BaboonAbstractTranslator {
  def translate(
    family: BaboonFamily
  ): Either[NEList[BaboonIssue.TranslationIssue], Sources]
}
