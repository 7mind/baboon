package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.CompilerTarget.CSTarget
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*

class CSTypeInfo(target: CSTarget, enquiries: BaboonEnquiries) {
  def adtNsName(id: TypeId.User): String = {
    id.name.name
  }

  def eliminated(id: TypeId.User, version: Version, lineage: BaboonLineage): Boolean = {
    val thisCanBeUpgraded = canBeUpgradedTo(id, version, lineage).nonEmpty

    val dom           = lineage.versions(version)
    val dependsOnThis = dom.defs.successors.links(id)

    // see BaboonComparator#compare and BaboonComparator#evolve,
    // a type can be upgraded ONLY when it's schema hash is completely unchanged,
    // so this should be safe
    val allDependantsCanBeUpgraded = dependsOnThis.forall(d => canBeUpgradedTo(d.asInstanceOf[TypeId.User], version, lineage).nonEmpty)

    thisCanBeUpgraded || (allDependantsCanBeUpgraded && !dom.roots.contains(id))
  }

  def canBeUpgradedTo(id: TypeId.User, version: Version, lineage: BaboonLineage): Option[Version] = {
    if (target.language.deduplicate) {
      val dom = lineage.versions(version)
      val upgradeAllowed = dom.defs.meta.nodes(id) match {
        case _: DomainMember.Builtin =>
          false
        case u: DomainMember.User =>
          u.defn match {
            case _: Typedef.Dto      => true
            case _: Typedef.Enum     => true
            case _: Typedef.Adt      => true
            case _: Typedef.Contract => true
            case _: Typedef.Foreign  => true
            case _: Typedef.Service  => false
          }
      }
      if (upgradeAllowed) {
        possibleUpgrades(id, version, lineage).lastOption
      } else {
        None
      }
    } else {
      None
    }
  }

  def possibleUpgrades(id: TypeId.User, version: Version, lineage: BaboonLineage, checkOwner: Boolean = true): List[Version] = {
    val evo = lineage.evolution
    val dom = lineage.versions(version)

    evo
      .typesUnchangedSince(version).get(id)
      .toList.flatMap {
        u =>
          val higher = u.higherTwins(version)
          if (higher.isEmpty) {
            None
          } else {
            val defn = dom.defs.meta.nodes(id)

            defn match {
              case u: DomainMember.User =>
                def regularUpgrade(td: Typedef.User): Seq[Version] = td match {
                  case a: Typedef.Adt =>
                    possibleAdtUpgrades(a, version, lineage, higher)
                  case _ =>
                    higher
                }

                u.id.owner match {
                  case o: Owner.Adt =>
                    if (checkOwner) {
                      val ownerAdt = dom.defs.meta.nodes(o.id).asInstanceOf[DomainMember.User].defn.asInstanceOf[Typedef.Adt]
                      val allowedUpgrades =
                        possibleAdtUpgrades(ownerAdt, version, lineage, higher)

                      allowedUpgrades.filter {
                        higherTwinVersion =>
                          val upgradeVersions = possibleUpgrades(o.id, version, lineage)
                          val hardCriterion   = upgradeVersions.contains(higherTwinVersion)
                          hardCriterion
                      }
                    } else {
                      regularUpgrade(u.defn)
                    }
                  case _ =>
                    regularUpgrade(u.defn)
                }

              case _: DomainMember.Builtin =>
                higher
            }
          }
      }

  }

  private def possibleAdtUpgrades(adt: Typedef.Adt, version: Version, lineage: BaboonLineage, higher: List[Version]): List[Version] = {
    // maximum version to which EVERY branch can be upgraded
    val maxUpgrade = higher.filter(mh => adt.members.map(m => possibleUpgrades(m, version, lineage, checkOwner = false)).forall(_.contains(mh)))
    maxUpgrade
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
      case DomainMember.User(_, defn: Typedef.Foreign, _, _, _) =>
        defn
          .bindings("cs").attrs.attrs
          .find(_.name == "value-type")
          .exists(a => a.value.toLowerCase == "yes" || a.value.toLowerCase == "true")
      case _ =>
        false
    }
  }

}
