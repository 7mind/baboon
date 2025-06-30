package io.septimalmind.baboon.typer.model

import io.septimalmind.baboon.parser.model.{DerivationDecl, RawNodeMeta}

sealed trait DomainMember {
  def id: TypeId
}

object DomainMember {
  case class Builtin(id: TypeId.Builtin) extends DomainMember

  case class User(root: Boolean, defn: Typedef.User, derivations: Set[DerivationDecl], meta: RawNodeMeta) extends DomainMember {
    def id: TypeId.User = defn.id
  }
}
