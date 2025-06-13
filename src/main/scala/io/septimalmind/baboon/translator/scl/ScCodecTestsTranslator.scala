package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.CompilerTarget.ScTarget
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.{BaboonEvolution, Domain, DomainMember}
import io.septimalmind.baboon.util.BLogger
import izumi.fundamentals.platform.strings.TextTree

trait ScCodecTestsTranslator {
  def translate(
    definition: DomainMember.User,
    csRef: ScValue.ScType,
    srcRef: ScValue.ScType,
  ): Option[TextTree[ScValue]]
}

object ScCodecTestsTranslator {
  final class Impl(
    codecs: Set[ScCodecTranslator],
    typeTranslator: ScTypeTranslator,
    logger: BLogger,
    enquiries: BaboonEnquiries,
    target: ScTarget,
    domain: Domain,
    evo: BaboonEvolution,
  ) extends ScCodecTestsTranslator {
    override def translate(definition: DomainMember.User, csRef: ScValue.ScType, srcRef: ScValue.ScType): Option[TextTree[ScValue]] = None
  }
}
