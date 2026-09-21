package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.translator.csharp.CSValue.*
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree

/** The current domain's view of [[CSTypeTranslator]].
  *
  * Bound inside the per-domain subcontext, so a component that already holds the current `Domain`
  * and `BaboonEvolution` renders references without threading them through every call. The
  * explicit-domain API on [[CSTypeTranslator]] stays for the callers that genuinely mean another domain: a
  * conversion renders the SOURCE version's types alongside the current ones.
  */
final class CSDomainTypes(trans: CSTypeTranslator, domain: Domain, evo: BaboonEvolution) {
  def asCsRef(tpe: TypeRef, mutableCollections: Boolean = false): TextTree[CSValue] = trans.asCsRef(tpe, domain, evo, mutableCollections)

  def asCsType(tpe: TypeId, mutableCollections: Boolean = false): CSType = trans.asCsType(tpe, domain, evo, mutableCollections)

  def asCsTypeKeepForeigns(tid: TypeId.User): CSType = trans.asCsTypeKeepForeigns(tid, domain, evo)

  def isService(tid: TypeId.User): Boolean = trans.isService(tid, domain)

  def serviceMethodContainers(tid: TypeId.User): Option[(Seq[String], Seq[String])] = trans.serviceMethodContainers(tid, domain, evo)

  def serviceMethodFixtureNs(tid: TypeId.User): Option[Seq[String]] = trans.serviceMethodFixtureNs(tid, domain, evo)

  def csFixtureRef(tid: TypeId.User): CSType = trans.csFixtureRef(tid, domain, evo)

  def deNull(tpe: TypeRef, ref: TextTree[CSValue]): TextTree[CSValue] = trans.deNull(tpe, domain, ref)

  /** The package/crate of the current domain's own version. */
  def currentPkg: CSPackageId = trans.toCsPkg(domain.id, domain.version, evo)
}
