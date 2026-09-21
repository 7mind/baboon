package io.septimalmind.baboon.translator.swift

import io.septimalmind.baboon.translator.swift.SwValue.*
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree

/** The current domain's view of [[SwTypeTranslator]].
  *
  * Bound inside the per-domain subcontext, so a component that already holds the current `Domain`
  * and `BaboonEvolution` renders references without threading them through every call. The
  * explicit-domain API on [[SwTypeTranslator]] stays for the callers that genuinely mean another domain: a
  * conversion renders the SOURCE version's types alongside the current ones.
  */
final class SwDomainTypes(trans: SwTypeTranslator, domain: Domain, evo: BaboonEvolution) {
  def asSwRef(tpe: TypeRef): TextTree[SwValue] = trans.asSwRef(tpe, domain, evo)

  def asSwType(tpe: TypeId): SwType = trans.asSwType(tpe, domain, evo)

  def toSwTypeRefKeepForeigns(tid: TypeId.User): SwType = trans.toSwTypeRefKeepForeigns(tid, domain, evo)

  def fixtureClassName(tid: TypeId.User): String = trans.fixtureClassName(tid, domain, evo)

  def effectiveSwPkg(owner: Owner): SwPackageId = trans.effectiveSwPkg(owner, domain, evo)

  /** The package/crate of the current domain's own version. */
  def currentPkg: SwPackageId = trans.toSwPkg(domain.id, domain.version, evo)
}
