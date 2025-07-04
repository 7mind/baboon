package io.septimalmind.baboon.typer.model

import izumi.fundamentals.collections.nonempty.NEList

case class UnmodifiedSince(typeId: TypeId, in: Version, sameIn: NEList[Version]) {
  def higherTwins(version: Version): List[Version] = {
    sameIn.toList.filter(_.version > version.version)
  }
}

case class TypeMeta(shallowId: ShallowSchemaId, deepId: DeepSchemaId)
case class RefMeta(len: BinReprLen)

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
