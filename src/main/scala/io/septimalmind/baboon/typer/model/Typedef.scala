package io.septimalmind.baboon.typer.model

import izumi.fundamentals.collections.nonempty.NonEmptyList
import izumi.fundamentals.graphs.DG

case class Domain(id: Pkg,
                  version: Version,
                  defs: DG[TypeId, DomainMember],
                  excludedIds: Set[TypeId]) {
  import izumi.fundamentals.platform.strings.IzString.*
  override def toString: String =
    s"""${id} ${version}
       |  deps: ${defs.predecessors.links.toList.niceList().shift(4)}
       |  excluded: ${excludedIds.niceList().shift(4)}
       |  defns: ${defs.meta.nodes.values.niceList().shift(4)}""".stripMargin
}

sealed trait DomainMember {
  def id: TypeId
}
object DomainMember {
  case class Builtin(id: TypeId.Builtin) extends DomainMember
  case class User(root: Boolean, defn: Typedef.User) extends DomainMember {
    def id: TypeId.User = defn.id
  }
}

sealed trait Typedef {
  def id: TypeId
}

object Typedef {
  sealed trait User extends Typedef {
    def id: TypeId.User
  }

  case class Dto(id: TypeId.User, fields: List[Field]) extends User
  case class Enum(id: TypeId.User, members: NonEmptyList[EnumMember])
      extends User
  case class Adt(id: TypeId.User, members: NonEmptyList[TypeId]) extends User
}

sealed trait TypeRef
object TypeRef {
  case class Scalar(id: TypeId) extends TypeRef
  case class Constructor(id: TypeId, args: NonEmptyList[TypeRef])
      extends TypeRef
}

sealed trait TypeId {
  def name: TypeName
}
object TypeId {
  case class Builtin(name: TypeName) extends TypeId {
    override def toString: String = s"#${name.name}"
  }
  case class User(pkg: Pkg, owner: Owner, name: TypeName) extends TypeId {
    override def toString: String = s"$pkg#${name.name}"
  }

  object Builtins {
    final val i08 = Builtin(TypeName("i08"))
    final val i32 = Builtin(TypeName("i32"))
    final val i64 = Builtin(TypeName("i64"))
    final val str = Builtin(TypeName("str"))
    final val tsu = Builtin(TypeName("tsu"))
    final val tso = Builtin(TypeName("tso"))

    final val map = Builtin(TypeName("map"))
    final val opt = Builtin(TypeName("opt"))
    final val lst = Builtin(TypeName("lst"))
    final val set = Builtin(TypeName("set"))

    final val integers = Set(i08, i32, i64)
    final val timestamps = Set(tsu, tso)
    final val data = Set(str)
    final val collections = Set(map, opt, lst, set)

    final val scalars = integers ++ data ++ timestamps
    final val all = scalars ++ collections
  }
}

sealed trait Owner
object Owner {
  case object Toplevel extends Owner
  case class Adt(id: TypeId) extends Owner
}

case class Pkg(path: NonEmptyList[String]) {
  override def toString: String = path.mkString(".")
}
case class TypeName(name: String)

case class EnumMember(name: String)

case class FieldName(name: String) {
  override def toString: String = s"$name"
}
case class Field(name: FieldName, tpe: TypeRef) {
  override def toString: String = s"$name: $tpe"
}

case class Version(version: String) {
  override def toString: String = s"{${version}}"
}
