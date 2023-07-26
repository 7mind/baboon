package io.septimalmind.baboon.typer

import io.septimalmind.baboon.typer.model.{DomainMember, Owner, ShallowSchemaId, TypeId, TypeRef, Typedef}
import izumi.fundamentals.platform.crypto.IzSha256Hash

trait BaboonEnquiries {
  def directDepsOf(defn: DomainMember): Set[TypeId]
  def wrap(id: TypeId): String
}

object BaboonEnquiries {

  class BaboonEnquiriesImpl() extends BaboonEnquiries {
    def directDepsOf(defn: DomainMember): Set[TypeId] = defn match {
      case _: DomainMember.Builtin => Set.empty
      case u: DomainMember.User =>
        u.defn match {
          case t: Typedef.Dto  => t.fields.flatMap(f => explode(f.tpe)).toSet
          case _: Typedef.Enum => Set.empty
          case t: Typedef.Adt  => t.members.toSet
        }
    }

    private def explode(tpe: TypeRef): Set[TypeId] = tpe match {
      case TypeRef.Scalar(id) => Set(id)
      case TypeRef.Constructor(id, args) =>
        Set(id) ++ args.toList.flatMap(a => explode(a))
    }

    def shallowId(defn: DomainMember): ShallowSchemaId = {
      val normalizedRepr = defn match {
        case id: DomainMember.Builtin => s"[builtin:${id.id.name.name}]"
        case u: DomainMember.User =>
          u.defn match {
            case d: Typedef.Dto =>
              val members = d.fields.map { f =>
                s"${f.name}:${wrap(f.tpe)}"
              }.sorted

              s"[dto;${wrap(d.id)};$members]"

            case c: Typedef.Enum =>
              val members = c.members.toList.map(_.name).sorted.mkString(",")
              s"[enum;${wrap(c.id)};$members]"
            case a: Typedef.Adt =>
              val members =
                a.members.toList.map(id => wrap(id)).sorted.mkString(",")
              s"[adt;${wrap(a.id)};$members]"
          }
      }

      ShallowSchemaId(IzSha256Hash.hash(normalizedRepr))
    }

    def wrap(id: TypeId): String = {
      id match {
        case b: TypeId.Builtin =>
          s"#${b.name.name}"
        case u: TypeId.User =>
          val owner = u.owner match {
            case Owner.Toplevel => "//"
            case Owner.Adt(id)  => s"/${wrap(u)}/"
          }
          s"${u.pkg.path.mkString(".")}#${owner}#${u.name.name}"

      }

    }

    private def wrap(tpe: TypeRef): String = tpe match {
      case s: TypeRef.Scalar =>
        s"{${wrap(s.id)}}"
      case c: TypeRef.Constructor =>
        s"{${wrap(c.id)}${c.args.toList.map(wrap).mkString("[", ",", "]")}"
    }

  }

}


