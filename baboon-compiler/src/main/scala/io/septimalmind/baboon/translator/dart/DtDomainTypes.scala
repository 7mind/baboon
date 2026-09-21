package io.septimalmind.baboon.translator.dart

import io.septimalmind.baboon.translator.dart.DtValue.*
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree

/** The current domain's view of [[DtTypeTranslator]].
  *
  * Bound inside the per-domain subcontext, so a component that already holds the current `Domain`
  * and `BaboonEvolution` renders references without threading them through every call. The
  * explicit-domain API on [[DtTypeTranslator]] stays for the callers that genuinely mean another domain: a
  * conversion renders the SOURCE version's types alongside the current ones.
  */
final class DtDomainTypes(trans: DtTypeTranslator, domain: Domain, evo: BaboonEvolution) {
  def asDtRef(tpe: TypeRef): TextTree[DtValue] = trans.asDtRef(tpe, domain, evo)

  def asDtType(tpe: TypeId): DtType = trans.asDtType(tpe, domain, evo)

  def toDtTypeRefKeepForeigns(tid: TypeId.User): DtType = trans.toDtTypeRefKeepForeigns(tid, domain, evo)

  def effectiveDtPkg(owner: Owner): DtPackageId = trans.effectiveDtPkg(owner, domain, evo)
}
