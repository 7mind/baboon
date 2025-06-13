package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.CompilerTarget.ScTarget
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.{BaboonEvolution, Domain, DomainMember}
import izumi.fundamentals.platform.strings.TextTree

trait ScCodecFixtureTranslator {
  def translate(definition: DomainMember.User): Option[TextTree[ScValue]]
}

object ScCodecFixtureTranslator {
  final class ScRandomMethodTranslatorImpl(
    target: ScTarget,
    translator: ScTypeTranslator,
    enquiries: BaboonEnquiries,
    domain: Domain,
    evo: BaboonEvolution,
  ) extends ScCodecFixtureTranslator {
    override def translate(definition: DomainMember.User): Option[TextTree[ScValue]] = ???
  }
}
