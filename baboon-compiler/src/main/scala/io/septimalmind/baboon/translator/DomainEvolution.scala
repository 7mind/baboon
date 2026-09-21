package io.septimalmind.baboon.translator

import io.septimalmind.baboon.typer.model.*

/** The current domain's position in its lineage.
  *
  * `BaboonEvolution` is keyed by version because it describes a whole lineage; inside a per-domain
  * subcontext exactly one of those keys is meaningful. This facade fixes it, so a code generator
  * asks what is unchanged, forward-readable, or readable by which readers without re-deriving
  * `domain.version` at every call. Language-agnostic, hence one class bound in all nine submodules.
  */
final class DomainEvolution(evo: BaboonEvolution, domain: Domain) {
  def typesUnchangedSince: Map[TypeId, UnmodifiedSince] = evo.typesUnchangedSince(domain.version)

  def typesForwardReadable: Map[TypeId, ForwardReadable] = evo.typesForwardReadable(domain.version)

  def minReaders(id: TypeId): Map[ForwardCompatTier, Version] = evo.minReaders(domain.version, id)
}
