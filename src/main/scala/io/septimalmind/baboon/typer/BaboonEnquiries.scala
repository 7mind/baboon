package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.{
  RawAdt,
  RawDefn,
  RawDtoMember,
  RawDtoid,
  ScopedRef
}
import io.septimalmind.baboon.typer.model.{
  DomainMember,
  Field,
  Owner,
  ShallowSchemaId,
  TypeId,
  TypeRef,
  Typedef
}
import izumi.fundamentals.platform.crypto.IzSha256Hash

import scala.collection.mutable

trait BaboonEnquiries {
  def fullDepsOfDefn(defn: DomainMember): Set[TypeId]
  def strongDepsOfDefn(defn: DomainMember): Set[TypeId]
  def wrap(id: TypeId): String
  def explode(tpe: TypeRef): Set[TypeId]
  def shallowId(defn: DomainMember): ShallowSchemaId
  def hardDepsOfRawDefn(dd: RawDefn): Set[ScopedRef]
}

object BaboonEnquiries {

  class BaboonEnquiriesImpl() extends BaboonEnquiries {
    def hardDepsOfRawDefn(dd: RawDefn): Set[ScopedRef] = {
      dd match {
        case d: RawDtoid =>
          d.members
            .collect {
              case d: RawDtoMember.ParentDef =>
                Seq(d.parent)
              case d: RawDtoMember.UnparentDef =>
                Seq(d.parent)
              case d: RawDtoMember.IntersectionDef =>
                Seq(d.parent)
              case d: RawDtoMember.ContractRef =>
                Seq(d.contract.tpe)
              case _ =>
                Seq.empty
            }
            .flatten
            .toSet
        case a: RawAdt =>
          a.contracts.map(_.contract.tpe).toSet
        case _ =>
          Set.empty
      }
    }

    def fullDepsOfDefn(defn: DomainMember): Set[TypeId] = {
      depsOfDefn(defn, explode)
    }

    def strongDepsOfDefn(defn: DomainMember): Set[TypeId] = {
      depsOfDefn(defn, explodeStrong)
    }

    private def depsOfDefn(defn: DomainMember,
                           explodeField: TypeRef => Set[TypeId],
    ): Set[TypeId] = {
      def explodeFields(f: List[Field]) = {
        f.flatMap(f => explodeField(f.tpe)).toSet
      }

      // TODO: do we REALLY need to consider field types as dependencies?
      defn match {
        case _: DomainMember.Builtin =>
          Set.empty
        case u: DomainMember.User =>
          u.defn match {
            case t: Typedef.Dto =>
              explodeFields(t.fields) ++ t.contracts
            case t: Typedef.Contract =>
              explodeFields(t.fields) ++ t.contracts
            case _: Typedef.Enum    => Set.empty
            case t: Typedef.Adt     => t.members.toSet ++ t.contracts
            case _: Typedef.Foreign => Set.empty
          }
      }
    }

    def explode(tpe: TypeRef): Set[TypeId] = {
      val seen = mutable.HashSet.empty[TypeId]
      def doExplode(tpe: TypeRef): Set[TypeId] = tpe match {
        case TypeRef.Scalar(id) =>
          seen += id
          Set(id)
        case TypeRef.Constructor(id, args) if seen.contains(id) =>
          args.toList.flatMap(a => explode(a)).toSet
        case TypeRef.Constructor(id, args) =>
          seen += id
          Set(id) ++ args.toList.flatMap(a => explode(a))

      }

      doExplode(tpe)
    }

    def explodeStrong(tpe: TypeRef): Set[TypeId] = {
      def doExplode(tpe: TypeRef): Set[TypeId] = tpe match {
        case TypeRef.Scalar(id) =>
          Set(id)
        case TypeRef.Constructor(id, _) =>
          // we ignore arguments for generics, that IS the point, current BIGs terminate referential loops
          // this routing might be extended in the future if we add support for some other type constructors
          Set(id)
      }

      doExplode(tpe)
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
            case d: Typedef.Contract =>
              val members = d.fields.map { f =>
                s"${f.name}:${wrap(f.tpe)}"
              }.sorted

              s"[contract;${wrap(d.id)};$members]"

            case c: Typedef.Enum =>
              val members = c.members.toList.map(_.name).sorted.mkString(",")
              s"[enum;${wrap(c.id)};$members]"
            case a: Typedef.Adt =>
              val members =
                a.members.toList.map(id => wrap(id)).sorted.mkString(",")
              s"[adt;${wrap(a.id)};$members]"
            case f: Typedef.Foreign =>
              val members = f.bindings.toSeq.sorted.mkString(":")
              s"[foreign;${wrap(f.id)};$members]"
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
            case Owner.Adt(id)  => s"/${wrap(id)}/"
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
