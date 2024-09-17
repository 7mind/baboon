package io.septimalmind.baboon.typer.model

import io.septimalmind.baboon.parser.model.RawNodeMeta
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.graphs.DG
import izumi.fundamentals.graphs.tools.cycles.LoopDetector

case class Domain(id: Pkg,
                  version: Version,
                  defs: DG[TypeId, DomainMember],
                  excludedIds: Set[TypeId],
                  shallowSchema: Map[TypeId, ShallowSchemaId],
                  deepSchema: Map[TypeId, DeepSchemaId],
                  loops: Set[LoopDetector.Cycles[TypeId]],
) {
  import izumi.fundamentals.platform.strings.IzString.*
  override def toString: String =
    s"""$id $version
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
  case class User(root: Boolean, defn: Typedef.User, meta: RawNodeMeta)
      extends DomainMember {
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

  sealed trait NonDataTypedef {
    this: Typedef =>
  }

  case class Dto(id: TypeId.User,
                 fields: List[Field],
                 contracts: List[TypeId.User])
      extends User
  case class Contract(id: TypeId.User,
                      fields: List[Field],
                      contracts: List[TypeId.User])
      extends User
      with NonDataTypedef
  case class Enum(id: TypeId.User, members: NEList[EnumMember]) extends User
  case class Adt(id: TypeId.User,
                 members: NEList[TypeId.User],
                 contracts: List[TypeId.User],
                 fields: List[Field],
  ) extends User
  case class Foreign(id: TypeId.User, bindings: Map[String, String])
      extends User

  object Adt {
    implicit class AdtSyntax(val adt: Adt) extends AnyVal {
      def dataMembers(domain: Domain): Seq[TypeId.User] =
        adt.members.toList.filterNot { id =>
          val a: DomainMember = domain.defs.meta.nodes(id)
          a match {
            case u: DomainMember.User =>
              u.defn.isInstanceOf[Typedef.NonDataTypedef]
            case _ => false
          }

        }
    }
  }
}

sealed trait TypeRef {
  def id: TypeId
}
object TypeRef {
  case class Scalar(id: TypeId.Scalar) extends TypeRef
  case class Constructor(id: TypeId.BuiltinCollection, args: NEList[TypeRef])
      extends TypeRef
}

sealed trait TypeId {
  def name: TypeName
}
object TypeId {
  sealed trait Builtin extends TypeId

  sealed trait Scalar extends TypeId

  case class BuiltinScalar(name: TypeName) extends Builtin with Scalar {
    override def toString: String = s"#${name.name}"
  }

  case class BuiltinCollection(name: TypeName) extends Builtin {
    override def toString: String = s"#${name.name}"
  }

  case class User(pkg: Pkg, owner: Owner, name: TypeName)
      extends TypeId
      with Scalar {
    override def toString: String = {
      s"$pkg/$owner#${name.name}"
    }
  }

  object Builtins {

    final val bit = BuiltinScalar(TypeName("bit"))

    final val i08 = BuiltinScalar(TypeName("i08"))
    final val i16 = BuiltinScalar(TypeName("i16"))
    final val i32 = BuiltinScalar(TypeName("i32"))
    final val i64 = BuiltinScalar(TypeName("i64"))

    final val u08 = BuiltinScalar(TypeName("u08"))
    final val u16 = BuiltinScalar(TypeName("u16"))
    final val u32 = BuiltinScalar(TypeName("u32"))
    final val u64 = BuiltinScalar(TypeName("u64"))

    final val f32 = BuiltinScalar(TypeName("f32"))
    final val f64 = BuiltinScalar(TypeName("f64"))
    final val f128 = BuiltinScalar(TypeName("f128"))

    final val str = BuiltinScalar(TypeName("str"))
    final val uid = BuiltinScalar(TypeName("uid"))
    final val tsu = BuiltinScalar(TypeName("tsu"))
    final val tso = BuiltinScalar(TypeName("tso"))

    final val map = BuiltinCollection(TypeName("map"))
    final val opt = BuiltinCollection(TypeName("opt"))
    final val lst = BuiltinCollection(TypeName("lst"))
    final val set = BuiltinCollection(TypeName("set"))

    final val integers = Set(i08, i16, i32, i64, u08, u16, u32, u64)
    final val floats = Set(f32, f64, f128)
    final val timestamps = Set(tsu, tso)
    final val stringy = Set(uid)
    final val varlens = Set(str)

    final val seqCollections = Set(lst, set)
    final val iterableCollections = Set(map) ++ seqCollections
    final val collections = Set(opt) ++ iterableCollections

    final val scalars = integers ++ floats ++ varlens ++ stringy ++ timestamps ++ Set(
      bit
    )
    final val all = scalars ++ collections

    private final val collIds = TypeId.Builtins.collections.toSet[TypeId]
    private final val seqColls = TypeId.Builtins.seqCollections.toSet[TypeId]
    private final val safeSources = seqColls ++ Set(TypeId.Builtins.opt)

    def hasDefaultValue(id: TypeRef.Constructor): Boolean =
      collIds.contains(id.id)

    def canBeWrappedIntoCollection(o: TypeRef.Scalar,
                                   n: TypeRef.Constructor): Boolean = {
      safeSources.contains(n.id) && n.args == NEList(o)
    }

    def canChangeCollectionType(o: TypeRef.Constructor,
                                n: TypeRef.Constructor): Boolean = {
      // we can safely change collection types between list <-> set, opt -> (list | set)
      val isSwap = safeSources.contains(o.id) && seqColls.contains(n.id)

      val isSimpleSwap = (o.args == n.args && isSwap)

      val isPrecex = o.args.length == n.args.length && o.args.toSeq
        .zip(n.args.toSeq)
        .forall {
          case (o, n) => isPrecisionExpansion(o.id, n.id)
        }
      val isSimplePrecex = (o.id == n.id) && isPrecex

      val isSwapPrecex = (isSwap && isPrecex)

      isSimpleSwap || isSimplePrecex || isSwapPrecex
    }

    def isPrecisionExpansion(o: TypeId, n: TypeId): Boolean = {
      (TypeId.Builtins.unpack(o), TypeId.Builtins.unpack(n)) match {
        case (Some(oldScalar), Some(newScalar)) =>
          oldScalar._1 == newScalar._1 && oldScalar._2 < newScalar._2
        case _ =>
          false
      }
    }

    def unpack(typeId: TypeId): Option[(String, Int)] = {
      typeId match {
        case Builtins.i08  => Some(("int", 8))
        case Builtins.i16  => Some(("int", 16))
        case Builtins.i32  => Some(("int", 32))
        case Builtins.i64  => Some(("int", 64))
        case Builtins.u08  => Some(("uint", 8))
        case Builtins.u16  => Some(("uint", 16))
        case Builtins.u32  => Some(("uint", 32))
        case Builtins.u64  => Some(("uint", 64))
        case Builtins.f32  => Some(("float", 32))
        case Builtins.f64  => Some(("float", 64))
        case Builtins.f128 => Some(("float", 128))
        case _             => None
      }
    }
  }

  sealed trait ComparatorType
  object ComparatorType {
    sealed trait Basic extends ComparatorType
    case object Direct extends Basic
    case object ObjectEquals extends Basic

    sealed trait Complex extends ComparatorType
    case class OptionEquals(subComparator: ComparatorType) extends Complex
    case class SeqEquals(subComparator: ComparatorType) extends Complex
    case class SetEquals(subComparator: ComparatorType) extends Complex
    case class MapEquals(keyComparator: ComparatorType,
                         valComparator: ComparatorType)
        extends Complex
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
        val arg1 = c.args.head

        c.id match {
          case TypeId.Builtins.opt =>
            comparator(arg1) match {
              case ComparatorType.Direct => ComparatorType.Direct
              case out                   => ComparatorType.OptionEquals(out)
            }
          case TypeId.Builtins.set =>
            ComparatorType.SetEquals(comparator(arg1))

          case TypeId.Builtins.map =>
            ComparatorType.MapEquals(comparator(arg1), comparator(c.args.last))
          case TypeId.Builtins.lst =>
            ComparatorType.SeqEquals(comparator(arg1))
          case _ =>
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

    override def toString: String = ":"
  }

  case class Ns(path: Seq[TypeName]) extends Owner {
    override def asPseudoPkg: Seq[String] = path.map(_.name)
    override def toString: String = path.map(_.name).mkString("/")
  }

  case class Adt(id: TypeId.User) extends Owner {
    override def asPseudoPkg: Seq[String] =
      id.owner.asPseudoPkg ++ Seq(id.name.name)
    override def toString: String = s"[$id]"
  }
}

case class Pkg(path: NEList[String]) {
  override def toString: String = path.mkString(".")
}
case class TypeName(name: String)

case class EnumMember(name: String, const: Option[Long])

case class FieldName(name: String) {
  override def toString: String = s"$name"
}
case class Field(name: FieldName, tpe: TypeRef) {
  override def toString: String = s"$name: $tpe"
}

case class Version(version: String) {
  override def toString: String = s"{${version}}"
}
