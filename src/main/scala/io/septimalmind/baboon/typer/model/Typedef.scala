package io.septimalmind.baboon.typer.model

import izumi.fundamentals.collections.nonempty.NonEmptyList

case class Domain(defs: List[Typedef])

sealed trait Typedef {
  def id: TypeId
}

object Typedef {
  case class Dto(id: TypeId, fields: List[Field]) extends Typedef
  case class Enum(id: TypeId, members: NonEmptyList[EnumMember]) extends Typedef
  case class Adt(id: TypeId, members: NonEmptyList[TypeId]) extends Typedef
}

sealed trait TypeRef
object TypeRef {
  case class Scalar(id: TypeId) extends TypeRef
  case class Constructor(id: TypeId, args: NonEmptyList[TypeRef])
      extends TypeRef
}

case class TypeId(pkg: Pkg, owner: Owner, name: TypeName)

sealed trait Owner
object Owner {
  case object Toplevel extends Owner
  case class Adt(id: TypeId) extends Owner
}

case class Pkg(path: NonEmptyList[String])
case class TypeName(name: String)

case class EnumMember(name: String)

case class FieldName(name: String)
case class Field(name: FieldName, tpe: TypeRef)
