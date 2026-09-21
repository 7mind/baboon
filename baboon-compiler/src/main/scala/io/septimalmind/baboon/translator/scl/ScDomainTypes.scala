package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.translator.scl.ScValue.{ScPackageId, ScType}
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree

/** The current domain's view of [[ScTypeTranslator]].
  *
  * Bound inside the per-domain subcontext, so a component that already holds the current `Domain`
  * and `BaboonEvolution` renders references without threading them through every call. The
  * explicit-domain API on [[ScTypeTranslator]] stays for the callers that genuinely mean another
  * domain: a conversion renders the SOURCE version's types alongside the current ones, and the
  * per-family translator walks several lineages.
  */
final class ScDomainTypes(trans: ScTypeTranslator, domain: Domain, evo: BaboonEvolution) {
  def asScRef(tpe: TypeRef): TextTree[ScValue] = trans.asScRef(tpe, domain, evo)

  def asScType(tpe: TypeId): ScType = trans.asScType(tpe, domain, evo)

  def toScTypeRefKeepForeigns(tid: TypeId.User): ScType = trans.toScTypeRefKeepForeigns(tid, domain, evo)

  /** The package of the current domain's own version. */
  def currentPkg: ScPackageId = trans.toScPkg(domain.id, domain.version, evo)
}
