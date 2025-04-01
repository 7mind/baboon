package io.septimalmind.baboon.typer.model

import io.septimalmind.baboon.parser.model.RawNodeMeta
import izumi.fundamentals.collections.nonempty.{NEList, NESet}
import izumi.fundamentals.graphs.DG
import izumi.fundamentals.graphs.tools.cycles.LoopDetector

sealed trait BinReprLen {
  def isVariable: Boolean
  def prefixed(len: Int): BinReprLen
}
object BinReprLen {
  case class Fixed(bytes: Int) extends BinReprLen {

    override def isVariable: Boolean = false

    override def prefixed(len: Int): BinReprLen = Fixed(bytes + len)
  }
  sealed trait Variable extends BinReprLen {
    override def isVariable: Boolean = true
  }

  case class Unknown() extends Variable {
    override def prefixed(len: Int): BinReprLen = this
  }

  case class Alternatives(variants: NESet[Int]) extends Variable {
    override def prefixed(len: Int): BinReprLen =
      Alternatives(NESet(len) ++ variants.map(_ + len))
  }

  case class Range(min: Int, max: Option[Int]) extends Variable {
    override def prefixed(len: Int): BinReprLen =
      Range(min + len, max.map(_ + len))
  }
}

case class TypeMeta(shallowId: ShallowSchemaId, deepId: DeepSchemaId)
case class RefMeta(len: BinReprLen)

case class Domain(
  id: Pkg,
  version: Version,
  defs: DG[TypeId, DomainMember],
  excludedIds: Set[TypeId],
  typeMeta: Map[TypeId, TypeMeta],
  loops: Set[LoopDetector.Cycles[TypeId]],
  refMeta: Map[TypeRef, RefMeta],
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

sealed trait DomainMember {
  def id: TypeId
}

object DomainMember {
  case class Builtin(id: TypeId.Builtin) extends DomainMember

  case class User(root: Boolean, defn: Typedef.User, meta: RawNodeMeta) extends DomainMember {
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

  case class Dto(id: TypeId.User, fields: List[Field], contracts: List[TypeId.User]) extends User

  case class Enum(id: TypeId.User, members: NEList[EnumMember]) extends User

  case class Adt(id: TypeId.User, members: NEList[TypeId.User], contracts: List[TypeId.User], fields: List[Field]) extends User

  case class ForeignEntryAttr(name: String, value: String)
  case class ForeignEntryAttrs(attrs: List[ForeignEntryAttr])
  case class ForeignEntry(lang: String, decl: String, attrs: ForeignEntryAttrs)

  case class Foreign(id: TypeId.User, bindings: Map[String, ForeignEntry]) extends User

  sealed trait NonDataTypedef {
    this: Typedef =>
  }

//  case class FuncArgName(name: String) extends AnyVal
//  case class FuncArgDef(name: FuncArgName, ref: TypeRef)
  case class MethodName(name: String) extends AnyVal
  case class MethodDef(name: MethodName, sig: TypeRef, out: Option[TypeRef], err: Option[TypeRef])

  case class Service(id: TypeId.User, methods: List[MethodDef]) extends User with NonDataTypedef

  case class Contract(id: TypeId.User, fields: List[Field], contracts: List[TypeId.User]) extends User with NonDataTypedef

  object Adt {
    implicit class AdtSyntax(val adt: Adt) extends AnyVal {
      def dataMembers(domain: Domain): Seq[TypeId.User] =
        adt.members.toList.filterNot {
          id =>
            domain.defs.meta.nodes(id) match {
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
  case class Scalar(id: TypeId.Scalar) extends TypeRef {
    override def toString: String = id.toString
  }

  case class Constructor(id: TypeId.BuiltinCollection, args: NEList[TypeRef]) extends TypeRef {
    override def toString: String = s"""${id.toString}${args.mkString("[", ",", "]")}"""
  }

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

  case class User(pkg: Pkg, owner: Owner, name: TypeName) extends TypeId with Scalar {
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

    final val f32  = BuiltinScalar(TypeName("f32"))
    final val f64  = BuiltinScalar(TypeName("f64"))
    final val f128 = BuiltinScalar(TypeName("f128"))

    final val str = BuiltinScalar(TypeName("str"))
    final val uid = BuiltinScalar(TypeName("uid"))
    final val tsu = BuiltinScalar(TypeName("tsu"))
    final val tso = BuiltinScalar(TypeName("tso"))

    final val map = BuiltinCollection(TypeName("map"))
    final val opt = BuiltinCollection(TypeName("opt"))
    final val lst = BuiltinCollection(TypeName("lst"))
    final val set = BuiltinCollection(TypeName("set"))

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

    case class MapEquals(keyComparator: ComparatorType, valComparator: ComparatorType) extends Complex
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
  override def toString: String = s"{$version}"
}
