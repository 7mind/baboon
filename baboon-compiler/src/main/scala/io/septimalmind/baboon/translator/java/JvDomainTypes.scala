package io.septimalmind.baboon.translator.java

import io.septimalmind.baboon.translator.java.JvValue.*
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree

/** The current domain's view of [[JvTypeTranslator]].
  *
  * Bound inside the per-domain subcontext, so a component that already holds the current `Domain`
  * and `BaboonEvolution` renders references without threading them through every call. The
  * explicit-domain API on [[JvTypeTranslator]] stays for the callers that genuinely mean another domain: a
  * conversion renders the SOURCE version's types alongside the current ones.
  */
final class JvDomainTypes(trans: JvTypeTranslator, domain: Domain, evo: BaboonEvolution) {
  def asJvRef(tpe: TypeRef): TextTree[JvValue] = trans.asJvRef(tpe, domain, evo)

  def asJvBoxedRef(tpe: TypeRef): TextTree[JvValue] = trans.asJvBoxedRef(tpe, domain, evo)

  def asJvType(tpe: TypeId): JvType = trans.asJvType(tpe, domain, evo)

  def asJvBoxedType(tpe: TypeId): JvType = trans.asJvBoxedType(tpe, domain, evo)

  def toJvTypeRefKeepForeigns(tid: TypeId.User): JvType = trans.toJvTypeRefKeepForeigns(tid, domain, evo)

  def effectiveJvPkg(owner: Owner): JvPackageId = trans.effectiveJvPkg(owner, domain, evo)
}
