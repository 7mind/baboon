package io.septimalmind.baboon.typer.model

import io.septimalmind.baboon.parser.model.RawMemberMeta
import izumi.fundamentals.graphs.DG
import izumi.fundamentals.graphs.tools.cycles.LoopDetector

case class Domain(
  id: Pkg,
  version: Version,
  defs: DG[TypeId, DomainMember],
  excludedIds: Set[TypeId],
  typeMeta: Map[TypeId, TypeMeta],
  loops: Set[LoopDetector.Cycles[TypeId]],
  refMeta: Map[TypeRef, RefMeta],
  derivationRequests: Map[RawMemberMeta, Set[TypeId]],
  roots: Set[TypeId],
  renameCandidates: Map[TypeId.User, List[TypeId.User]],
) {

  import izumi.fundamentals.platform.strings.IzString.*

  override def toString: String =
    s"""$id $version
       |  deps: ${defs.predecessors.links.toList.niceList().shift(4)}
       |  excluded: ${excludedIds.niceList().shift(4)}
       |  defns: ${defs.meta.nodes.values
        .map(member => s"${typeMeta(member.id).shallowId}, ${typeMeta(member.id).deepId} = $member")
        .niceList()
        .shift(4)}""".stripMargin
}
