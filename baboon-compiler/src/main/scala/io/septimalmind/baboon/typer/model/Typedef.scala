package io.septimalmind.baboon.typer.model

import izumi.fundamentals.collections.nonempty.NEList

case class UnmodifiedSince(typeId: TypeId, in: Version, sameIn: NEList[Version]) {
  def higherTwins(version: Version): List[Version] = {
    sameIn.toList.filter(_ > version)
  }
}

case class TypeMeta(shallowId: ShallowSchemaId, deepId: DeepSchemaId)
case class RefMeta(len: BinReprLen)

sealed trait BaboonLang {
  def asString: String
}

object BaboonLang {
  case object Scala extends BaboonLang { val asString = "scala" }
  case object Cs extends BaboonLang { val asString = "cs" }
  case object Py extends BaboonLang { val asString = "py" }
  case object Rust extends BaboonLang { val asString = "rust" }
  case object Typescript extends BaboonLang { val asString = "typescript" }
  case object Kotlin extends BaboonLang { val asString = "kotlin" }
  case object Java extends BaboonLang { val asString = "java" }
  case object Dart extends BaboonLang { val asString = "dart" }
  case object Swift extends BaboonLang { val asString = "swift" }

  val all: List[BaboonLang] = List(Scala, Cs, Py, Rust, Typescript, Kotlin, Java, Dart, Swift)

  def fromString(s: String): Option[BaboonLang] = all.find(_.asString == s)
}

sealed trait Typedef {
  def id: TypeId
}

object Typedef {
  sealed trait User extends Typedef {
    def id: TypeId.User
  }

  case class Dto(id: TypeId.User, fields: List[Field], contracts: List[TypeId.User], isIdentifier: Boolean = false) extends User

  case class Enum(id: TypeId.User, members: NEList[EnumMember]) extends User

  case class Adt(id: TypeId.User, members: NEList[TypeId.User], contracts: List[TypeId.User], fields: List[Field]) extends User

  case class ForeignEntryAttr(name: String, value: String)
  case class ForeignEntryAttrs(attrs: List[ForeignEntryAttr])

  sealed trait ForeignMapping
  object ForeignMapping {
    case class Custom(decl: String, attrs: ForeignEntryAttrs) extends ForeignMapping
    case class BaboonRef(typeRef: TypeRef) extends ForeignMapping
  }

  case class ForeignEntry(lang: BaboonLang, mapping: ForeignMapping)

  case class Foreign(id: TypeId.User, bindings: Map[BaboonLang, ForeignEntry], runtimeMapping: Option[TypeRef]) extends User

  sealed trait NonDataTypedef {
    this: Typedef =>
  }

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

  // `any` opaque field. `variant` encodes which meta components appear on the wire;
  // `underlying.isDefined` distinguishes the D1/D2/D3 typed variants from A/B/C.
  case class Any(variant: AnyVariant, underlying: Option[TypeRef]) extends TypeRef {
    override def id: TypeId = TypeId.AnyType
    override def toString: String = variant match {
      case AnyVariant.Global  => underlying.fold("#any")(u => s"#any[$u]")
      case AnyVariant.ThisDom => underlying.fold("#any[domain:this]")(u => s"#any[domain:this,$u]")
      case AnyVariant.Current => underlying.fold("#any[domain:current]")(u => s"#any[domain:current,$u]")
    }
  }

  sealed trait AnyVariant
  object AnyVariant {

    /** Variant A (`any`) / D1 (`any[T]`) — domain + version + typeid on the wire. */
    case object Global extends AnyVariant

    /** Variant B (`any[domain:this]`) / D2 (`any[domain:this, T]`) — version + typeid on the wire. */
    case object ThisDom extends AnyVariant

    /** Variant C (`any[domain:current]`) / D3 (`any[domain:current, T]`) — typeid only on the wire. */
    case object Current extends AnyVariant

    /** Meta-kind byte per the design spec §"Meta-kind byte table".
      * Bit 0 = typeid present, bit 1 = version present, bit 2 = domain present.
      * `underlying` present ⇒ typeid is known statically ⇒ typeid bit is 0.
      */
    def metaKindByte(v: AnyVariant, hasUnderlying: Boolean): Byte = {
      val typeidBit = if (hasUnderlying) 0 else 1
      val (domainBit, versionBit) = v match {
        case Global  => (1, 1)
        case ThisDom => (0, 1)
        case Current => (0, 0)
      }
      ((domainBit << 2) | (versionBit << 1) | typeidBit).toByte
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

case class EnumMember(name: String, const: Option[Long], prevName: Option[String])

case class FieldName(name: String) {
  override def toString: String = s"$name"
}

case class Field(name: FieldName, tpe: TypeRef, prevName: Option[FieldName]) {
  override def toString: String = prevName match {
    case Some(prev) => s"$name: $tpe was $prev"
    case None       => s"$name: $tpe"
  }
}
