package io.septimalmind.baboon.translator.rust

import io.septimalmind.baboon.translator.rust.RsValue.RsType
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree

/** The current domain's view of [[RsTypeTranslator]].
  *
  * Bound inside the per-domain subcontext, so a component that already holds the current `Domain`
  * and `BaboonEvolution` renders references without threading them through every call. The
  * explicit-domain API on [[RsTypeTranslator]] stays for the callers that genuinely mean another
  * domain: a conversion renders the SOURCE version's types alongside the current ones.
  */
final class RsDomainTypes(trans: RsTypeTranslator, domain: Domain, evo: BaboonEvolution) {
  def asRsRef(tpe: TypeRef): TextTree[RsValue] = trans.asRsRef(tpe, domain, evo)

  def asRsType(tpe: TypeId): RsType = trans.asRsType(tpe, domain, evo)

  def toRsTypeRefKeepForeigns(tid: TypeId.User): RsType = trans.toRsTypeRefKeepForeigns(tid, domain, evo)
}
