package io.septimalmind.baboon.translator

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.{BaboonFamily, Domain}
import izumi.fundamentals.collections.nonempty.NEList

case class Sources(files: Map[String, String])
case class DomainSources(domain: Domain, files: Map[String, String])

trait AbstractBaboonTranslator {
  def translate(
    family: BaboonFamily
  ): Either[NEList[BaboonIssue.TranslationIssue], Sources]
}
