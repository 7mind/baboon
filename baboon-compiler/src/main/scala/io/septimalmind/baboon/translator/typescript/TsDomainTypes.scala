package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.translator.typescript.TsValue.*
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree

/** The current domain's view of [[TsTypeTranslator]].
  *
  * Bound inside the per-domain subcontext, so a component that already holds the current `Domain`
  * and `BaboonEvolution` renders references without threading them through every call. The
  * explicit-domain API on [[TsTypeTranslator]] stays for the callers that genuinely mean another domain: a
  * conversion renders the SOURCE version's types alongside the current ones.
  */
final class TsDomainTypes(trans: TsTypeTranslator, domain: Domain, evo: BaboonEvolution) {
  def asTsRef(tpe: TypeRef, pkgBase: List[String] = Nil): TextTree[TsValue] = trans.asTsRef(tpe, domain, evo, pkgBase)

  def asTsType(tpe: TypeId, pkgBase: List[String] = Nil): TsType = trans.asTsType(tpe, domain, evo, pkgBase)

  def asTsTypeDerefForeign(tid: TypeId.User, pkgBase: List[String]): TsType = trans.asTsTypeDerefForeign(tid, domain, evo, pkgBase)

  def asTsTypeKeepForeigns(tid: TypeId.User, pkgBase: List[String]): TsType = trans.asTsTypeKeepForeigns(tid, domain, evo, pkgBase)

  def toTsModule(tid: TypeId.User, pkgBase: List[String], suffix: String = ""): TsModuleId = trans.toTsModule(tid, domain, evo, pkgBase, suffix)

  def renderNsOwnerPath(path: Seq[TypeName]): List[String] = trans.renderNsOwnerPath(path, domain)
}
