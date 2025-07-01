package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.CompilerTarget.CSTarget
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*

class CSTypeInfo(target: CSTarget, enquiries: BaboonEnquiries) {
  def adtNsName(id: TypeId.User): String = {
    id.name.name
  }

  def canBeUpgraded(id: TypeId.User, dom: Domain): Boolean = {
    (id.owner match {
      case Owner.Toplevel => true
      case _: Owner.Ns    => true
      case _: Owner.Adt   => false
    }) && (dom.defs.meta.nodes(id) match {
      case DomainMember.Builtin(id) => false
      case DomainMember.User(root, defn, derivations, meta) =>
        defn match {
          case Typedef.Dto(id, fields, contracts)          => true
          case Typedef.Enum(id, members)                   => true
          case Typedef.Adt(id, members, contracts, fields) => false
          case Typedef.Foreign(id, bindings)               => false
          case Typedef.Service(id, methods)                => false
          case Typedef.Contract(id, fields, contracts)     => false
        }
    })
  }

  def isCSValueType(tpe: TypeRef, domain: Domain): Boolean = {
    // TODO: c# rules are complex, probably we have some issues here
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            s match {
              case TypeId.Builtins.bit  => true
              case TypeId.Builtins.i08  => true
              case TypeId.Builtins.i16  => true
              case TypeId.Builtins.i32  => true
              case TypeId.Builtins.i64  => true
              case TypeId.Builtins.u08  => true
              case TypeId.Builtins.u16  => true
              case TypeId.Builtins.u32  => true
              case TypeId.Builtins.u64  => true
              case TypeId.Builtins.f32  => true
              case TypeId.Builtins.f64  => true
              case TypeId.Builtins.f128 => true
              case TypeId.Builtins.uid  => true
              case TypeId.Builtins.tsu  => true
              case TypeId.Builtins.tso  => true
              case _                    => false
            }
          case _ =>
            enquiries.isEnum(tpe, domain) || foreignTypeIsValueType(id, domain)
        }
      case _ =>
        foreignTypeIsValueType(tpe.id, domain)
    }
  }

  private def foreignTypeIsValueType(id: TypeId, domain: Domain): Boolean = {
    domain.defs.meta.nodes(id) match {
      case DomainMember.User(_, defn: Typedef.Foreign, _, _) =>
        defn
          .bindings("cs").attrs.attrs
          .find(_.name == "value-type")
          .exists(a => a.value.toLowerCase == "yes" || a.value.toLowerCase == "true")
      case _ =>
        false
    }
  }
}
