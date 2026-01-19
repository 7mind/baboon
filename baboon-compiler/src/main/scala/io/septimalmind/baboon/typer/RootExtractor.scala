package io.septimalmind.baboon.typer

import io.septimalmind.baboon.typer.model.{DomainMember, TypeId}

trait RootExtractor {
  def roots(indexedDefs: Map[TypeId, DomainMember]): Map[TypeId, DomainMember.User]
}

object RootExtractor {
  final class DeclaredRootExtractor extends RootExtractor {
    override def roots(indexedDefs: Map[TypeId, DomainMember]): Map[TypeId, DomainMember.User] = {
      indexedDefs.collect {
        case (k, v: DomainMember.User) if v.root =>
          (k, v)
      }
    }
  }

  final class AllRootsExtractor extends RootExtractor {
    override def roots(indexedDefs: Map[TypeId, DomainMember]): Map[TypeId, DomainMember.User] = {
      indexedDefs.collect {
        case (k, v: DomainMember.User) =>
          (k, v)
      }
    }
  }
}
