package io.septimalmind.baboon.typer.model

import izumi.fundamentals.collections.nonempty.NonEmptyList
import izumi.fundamentals.graphs.DG

case class Domain(id: Pkg,
                  version: Version,
                  defs: DG[TypeId, DomainMember],
                  excludedIds: Set[TypeId],
                  shallowSchema: Map[TypeId, ShallowSchemaId],
                  deepSchema: Map[TypeId, DeepSchemaId],
) {
  import izumi.fundamentals.platform.strings.IzString.*
  override def toString: String =
    s"""${id} ${version}
       |  deps: ${defs.predecessors.links.toList.niceList().shift(4)}
       |  excluded: ${excludedIds.niceList().shift(4)}
       |  defns: ${defs.meta.nodes.values
         .map(
           member =>
             s"${shallowSchema(member.id)}, ${deepSchema(member.id)} = $member"
         )
         .niceList()
         .shift(4)}""".stripMargin
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
  case class Adt(id: TypeId.User, members: NonEmptyList[TypeId.User])
      extends User
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
    final val bit = Builtin(TypeName("bit"))

    final val i08 = Builtin(TypeName("i08"))
    final val i16 = Builtin(TypeName("i16"))
    final val i32 = Builtin(TypeName("i32"))
    final val i64 = Builtin(TypeName("i64"))

    final val u08 = Builtin(TypeName("u08"))
    final val u16 = Builtin(TypeName("u16"))
    final val u32 = Builtin(TypeName("u32"))
    final val u64 = Builtin(TypeName("u64"))

    final val f32 = Builtin(TypeName("f32"))
    final val f64 = Builtin(TypeName("f64"))
    final val f128 = Builtin(TypeName("f128"))

    final val str = Builtin(TypeName("str"))
    final val tsu = Builtin(TypeName("tsu"))
    final val tso = Builtin(TypeName("tso"))

    final val map = Builtin(TypeName("map"))
    final val opt = Builtin(TypeName("opt"))
    final val lst = Builtin(TypeName("lst"))
    final val set = Builtin(TypeName("set"))

    final val integers = Set(i08, i16, i32, i64, u08, u16, u32, u64)
    final val floats = Set(f32, f64, f128)
    final val timestamps = Set(tsu, tso)
    final val varlens = Set(str)

    final val seqCollections = Set(lst, set)
    final val collections = Set(map, opt) ++ seqCollections

    final val scalars = integers ++ floats ++ varlens ++ timestamps ++ Set(bit)
    final val all = scalars ++ collections

    private final val collIds = TypeId.Builtins.collections.toSet[TypeId]
    private final val seqColls = TypeId.Builtins.seqCollections.toSet[TypeId]
    private final val safeSources = seqColls ++ Set(TypeId.Builtins.opt)

    def hasDefaultValue(id: TypeRef.Constructor): Boolean =
      collIds.contains(id.id)

    def canBeWrappedIntoCollection(o: TypeRef.Scalar,
                                   n: TypeRef.Constructor): Boolean = {
      safeSources.contains(n.id) && n.args == NonEmptyList(o)
    }

    def canChangeCollectionType(o: TypeRef.Constructor,
                                n: TypeRef.Constructor): Boolean = {
      // we can safely change collection types between list <-> set, opt -> (list | set)
      o.args == n.args && safeSources.contains(o.id) && seqColls
        .contains(n.id)
    }
  }

  sealed trait ComparatorType
  object ComparatorType {
    case object Direct extends ComparatorType
    case object ObjectEquals extends ComparatorType
    case object OptionEquals extends ComparatorType
  }

  def comparator(ref: TypeRef): ComparatorType = {
    ref match {
      case TypeRef.Scalar(id) =>
        if (TypeId.Builtins.scalars.toSet[TypeId].contains(id)) {
          ComparatorType.Direct
        } else {
          ComparatorType.ObjectEquals
        }
      case c: TypeRef.Constructor =>
        if (c.id == TypeId.Builtins.opt) {
          comparator(c.args.head) match {
            case ComparatorType.Direct => ComparatorType.Direct
            case _                     => ComparatorType.OptionEquals
          }
        } else {
          ComparatorType.ObjectEquals
        }
    }
  }
}

sealed trait Owner {
  def asPseudoPkg: Seq[String]
}

object Owner {
  case object Toplevel extends Owner {
    override def asPseudoPkg: Seq[String] = Seq.empty
  }
  case class Adt(id: TypeId.User) extends Owner {
    override def asPseudoPkg: Seq[String] =
      id.owner.asPseudoPkg ++ Seq(id.name.name)
  }
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
