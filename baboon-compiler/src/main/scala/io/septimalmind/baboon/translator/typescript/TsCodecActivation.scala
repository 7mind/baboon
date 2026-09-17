package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.CompilerTarget.TsTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*

object TsCodecActivation {
  sealed trait Kind
  case object Json extends Kind
  case object Ueba extends Kind

  def isActive(target: TsTarget, domain: Domain, id: TypeId, kind: Kind): Boolean = {
    val (enabled, byDefault, derivation) = kind match {
      case Json => (target.language.generateJsonCodecs, target.language.generateJsonCodecsByDefault, "json")
      case Ueba => (target.language.generateUebaCodecs, target.language.generateUebaCodecsByDefault, "ueba")
    }
    !BaboonEnquiries.isBaboonRefForeign(id, domain, BaboonLang.Typescript) && enabled &&
    (byDefault || domain.derivationRequests.getOrElse(RawMemberMeta.Derived(derivation), Set.empty[TypeId]).contains(id))
  }
}
