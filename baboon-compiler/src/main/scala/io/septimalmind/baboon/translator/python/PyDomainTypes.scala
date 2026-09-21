package io.septimalmind.baboon.translator.python

import io.septimalmind.baboon.translator.python.PyValue.*
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree

/** The current domain's view of [[PyTypeTranslator]].
  *
  * Bound inside the per-domain subcontext, so a component that already holds the current `Domain`
  * and `BaboonEvolution` renders references without threading them through every call. The
  * explicit-domain API on [[PyTypeTranslator]] stays for the callers that genuinely mean another domain: a
  * conversion renders the SOURCE version's types alongside the current ones.
  */
final class PyDomainTypes(trans: PyTypeTranslator, domain: Domain, evo: BaboonEvolution) {
  def asPyRef(tpe: TypeRef, pkgBase: List[String] = Nil): TextTree[PyValue] = trans.asPyRef(tpe, domain, evo, pkgBase)

  def asPyType(tpe: TypeId, pkgBase: List[String] = Nil): PyType = trans.asPyType(tpe, domain, evo, pkgBase)

  def asPyTypeDerefForeign(tid: TypeId.User, pkgBase: List[String] = Nil): PyType = trans.asPyTypeDerefForeign(tid, domain, evo, pkgBase)

  def asPyTypeKeepForeigns(tid: TypeId.User, pkgBase: List[String] = Nil): PyType = trans.asPyTypeKeepForeigns(tid, domain, evo, pkgBase)

  def asPyTypeVersioned(tid: TypeId.User, pkgBase: List[String]): PyType = trans.asPyTypeVersioned(tid, domain, evo, pkgBase)

  /** The module of `tid` at the current domain's version. */
  def toPyModule(tid: TypeId.User, pkgBase: List[String]): PyModuleId =
    trans.toPyModule(tid, domain.version, evo, pkgBase)
}
