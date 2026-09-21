package io.septimalmind.baboon.translator

import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*

/** The current domain's view of the domain-taking half of [[BaboonEnquiries]].
  *
  * `BaboonEnquiries` is shared by the typer, the validator and every backend, so it stays bound
  * globally with an explicit-domain API. This facade is bound inside each per-domain subcontext, so
  * a code generator that already holds the current `Domain` asks its questions without re-supplying
  * it. Language-agnostic, hence one class bound in all nine submodules rather than one per backend.
  */
final class DomainEnquiries(enquiries: BaboonEnquiries, domain: Domain) {
  def hasForeignType(definition: DomainMember.User): Boolean = enquiries.hasForeignType(definition, domain)

  def hasForeignType(definition: DomainMember.User, lang: BaboonLang): Boolean =
    enquiries.hasForeignType(definition, domain, lang)

  def isRecursiveTypedef(definition: DomainMember.User): Boolean = enquiries.isRecursiveTypedef(definition, domain)

  def isEnum(tpe: TypeRef): Boolean = enquiries.isEnum(tpe, domain)

  def unfold(contracts: List[TypeId.User]): List[Field] = enquiries.unfold(domain, contracts)

  def collectParents(definitions: List[DomainMember.User]): List[TypeId.User] =
    enquiries.collectParents(domain, definitions)
}
