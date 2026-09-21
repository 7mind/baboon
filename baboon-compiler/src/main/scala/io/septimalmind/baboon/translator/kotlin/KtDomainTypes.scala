package io.septimalmind.baboon.translator.kotlin

import io.septimalmind.baboon.translator.kotlin.KtValue.*
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree

/** The current domain's view of [[KtTypeTranslator]].
  *
  * Bound inside the per-domain subcontext, so a component that already holds the current `Domain`
  * and `BaboonEvolution` renders references without threading them through every call. The
  * explicit-domain API on [[KtTypeTranslator]] stays for the callers that genuinely mean another domain: a
  * conversion renders the SOURCE version's types alongside the current ones.
  */
final class KtDomainTypes(trans: KtTypeTranslator, domain: Domain, evo: BaboonEvolution) {
  def asKtRef(tpe: TypeRef): TextTree[KtValue] = trans.asKtRef(tpe, domain, evo)

  def asKtNullableRef(tpe: TypeRef): TextTree[KtValue] = trans.asKtNullableRef(tpe, domain, evo)

  def asKtType(tpe: TypeId): KtType = trans.asKtType(tpe, domain, evo)

  def toKtTypeRefKeepForeigns(tid: TypeId.User): KtType = trans.toKtTypeRefKeepForeigns(tid, domain, evo)

  /** The package/crate of the current domain's own version. */
  def currentPkg: KtPackageId = trans.toKtPkg(domain.id, domain.version, evo)
}
