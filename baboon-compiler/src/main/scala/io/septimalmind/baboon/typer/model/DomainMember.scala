package io.septimalmind.baboon.typer.model

import io.septimalmind.baboon.parser.model.{RawMemberMeta, RawNodeMeta}

sealed trait DomainMember {
  def id: TypeId
}

object DomainMember {
  case class Builtin(id: TypeId.Builtin) extends DomainMember

  case class User(root: Boolean, defn: Typedef.User, derivations: Set[RawMemberMeta], meta: RawNodeMeta) extends DomainMember {
    def id: TypeId.User = defn.id
    def ownedByAdt: Boolean = {
      defn.id.owner match {
        case _: Owner.Adt => true
        case _            => false
      }
    }
    def isAdt: Boolean = {
      defn match {
        case _: Typedef.Adt => true
        case _              => false
      }
    }
  }
}
