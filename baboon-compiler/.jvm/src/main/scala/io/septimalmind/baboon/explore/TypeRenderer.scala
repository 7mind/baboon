package io.septimalmind.baboon.explore

import io.septimalmind.baboon.parser.model.{InputPointer, RawMemberMeta}
import io.septimalmind.baboon.typer.model.*

class TypeRenderer(domain: Domain) {
  import Colors.*

  def render(member: DomainMember.User): String = {
    val sb = new StringBuilder

    if (member.root) {
      sb.append(s"$MAGENTA@root$RESET ")
    }

    member.derivations.foreach {
      case RawMemberMeta.Derived(id) => sb.append(s"$CYAN: derived[$id]$RESET ")
      case _                         =>
    }

    member.defn match {
      case dto: Typedef.Dto =>
        sb.append(s"${BLUE}data$RESET $GREEN${dto.id.name.name}$RESET")
        if (dto.contracts.nonEmpty) {
          sb.append(s" $CYAN:$RESET ${dto.contracts.map(_.name.name).mkString(", ")}")
        }
        sb.append(" {\n")
        dto.fields.foreach {
          f =>
            sb.append(s"    $YELLOW${f.name.name}$RESET: ${renderTypeRef(f.tpe)}\n")
        }
        sb.append("}")

      case adt: Typedef.Adt =>
        sb.append(s"${BLUE}adt$RESET $GREEN${adt.id.name.name}$RESET")
        if (adt.contracts.nonEmpty) {
          sb.append(s" $CYAN:$RESET ${adt.contracts.map(_.name.name).mkString(", ")}")
        }
        sb.append(" {\n")

        if (adt.fields.nonEmpty) {
          sb.append(s"    $DIM// Shared fields:$RESET\n")
          adt.fields.foreach {
            f =>
              sb.append(s"    $YELLOW${f.name.name}$RESET: ${renderTypeRef(f.tpe)}\n")
          }
          sb.append("\n")
        }

        adt.members.toList.foreach {
          memberId =>
            domain.defs.meta.nodes.get(memberId).foreach {
              case u: DomainMember.User =>
                sb.append("    ")
                val memberRendered = renderBrief(u)
                sb.append(memberRendered.split("\n").mkString("\n    "))
                sb.append("\n")
              case _ =>
            }
        }
        sb.append("}")

      case enum: Typedef.Enum =>
        sb.append(s"${BLUE}enum$RESET $GREEN${enum.id.name.name}$RESET {\n")
        enum.members.toList.foreach {
          m =>
            val constPart = m.const.map(c => s" $CYAN=$RESET $c").getOrElse("")
            sb.append(s"    $YELLOW${m.name}$RESET$constPart\n")
        }
        sb.append("}")

      case foreign: Typedef.Foreign =>
        sb.append(s"${BLUE}foreign$RESET $GREEN${foreign.id.name.name}$RESET {\n")
        foreign.bindings.toSeq.sortBy(_._1.asString).foreach {
          case (lang, entry) =>
            entry.mapping match {
              case Typedef.ForeignMapping.Custom(decl, entryAttrs) =>
                sb.append(s"    $CYAN${lang.asString}$RESET = $WHITE\"$decl\"$RESET")
                if (entryAttrs.attrs.nonEmpty) {
                  val attrs = entryAttrs.attrs.map(a => s"\"${a.name}\" = \"${a.value}\"").mkString(", ")
                  sb.append(s" with { $attrs }")
                }
                sb.append("\n")
              case Typedef.ForeignMapping.BaboonRef(typeRef) =>
                sb.append(s"    $CYAN${lang.asString}$RESET = ${renderTypeRef(typeRef)}\n")
            }
        }
        sb.append("}")

      case contract: Typedef.Contract =>
        sb.append(s"${BLUE}mixin$RESET $GREEN${contract.id.name.name}$RESET")
        if (contract.contracts.nonEmpty) {
          sb.append(s" $CYAN:$RESET ${contract.contracts.map(_.name.name).mkString(", ")}")
        }
        sb.append(" {\n")
        contract.fields.foreach {
          f =>
            sb.append(s"    $YELLOW${f.name.name}$RESET: ${renderTypeRef(f.tpe)}\n")
        }
        sb.append("}")

      case service: Typedef.Service =>
        sb.append(s"${BLUE}service$RESET $GREEN${service.id.name.name}$RESET {\n")
        service.methods.foreach {
          m =>
            sb.append(s"    $YELLOW${m.name.name}$RESET(${renderTypeRef(m.sig)})")
            m.out.foreach(o => sb.append(s" $CYAN->$RESET ${renderTypeRef(o)}"))
            m.err.foreach(e => sb.append(s" $RED!>$RESET ${renderTypeRef(e)}"))
            sb.append("\n")
        }
        sb.append("}")
    }

    sb.append("\n\n")
    val location = InputPointer.format(member.meta.pos)
    if (location.nonEmpty) {
      sb.append(s"$DIM${location.mkString("\n")}$RESET")
    }

    sb.toString()
  }

  def renderBrief(member: DomainMember.User): String = {
    val sb = new StringBuilder

    member.defn match {
      case dto: Typedef.Dto =>
        sb.append(s"${BLUE}data$RESET $GREEN${dto.id.name.name}$RESET")
        if (dto.fields.nonEmpty) {
          sb.append(" { ")
          sb.append(dto.fields.map(f => s"${f.name.name}: ${renderTypeRef(f.tpe)}").mkString(", "))
          sb.append(" }")
        } else {
          sb.append(" {}")
        }

      case adt: Typedef.Adt =>
        sb.append(s"${BLUE}adt$RESET $GREEN${adt.id.name.name}$RESET")
        sb.append(s" $DIM(${adt.members.size} branches)$RESET")

      case enum: Typedef.Enum =>
        sb.append(s"${BLUE}enum$RESET $GREEN${enum.id.name.name}$RESET")
        sb.append(s" $DIM(${enum.members.size} values)$RESET")

      case foreign: Typedef.Foreign =>
        sb.append(s"${BLUE}foreign$RESET $GREEN${foreign.id.name.name}$RESET")

      case contract: Typedef.Contract =>
        sb.append(s"${BLUE}mixin$RESET $GREEN${contract.id.name.name}$RESET")

      case service: Typedef.Service =>
        sb.append(s"${BLUE}service$RESET $GREEN${service.id.name.name}$RESET")
    }

    sb.toString()
  }

  def renderTypeRef(ref: TypeRef): String = ref match {
    case TypeRef.Scalar(id) => renderTypeId(id)
    case TypeRef.Constructor(id, args) =>
      s"$CYAN${id.name.name}$RESET[${args.toList.map(renderTypeRef).mkString(", ")}]"
  }

  private def renderTypeId(id: TypeId): String = id match {
    case b: TypeId.Builtin => s"$CYAN${b.name.name}$RESET"
    case u: TypeId.User    => s"$GREEN${u.name.name}$RESET"
  }

  def renderTypeName(member: DomainMember.User): String = {
    val kind = member.defn match {
      case _: Typedef.Dto      => s"${BLUE}data$RESET"
      case _: Typedef.Adt      => s"${BLUE}adt$RESET"
      case _: Typedef.Enum     => s"${BLUE}enum$RESET"
      case _: Typedef.Foreign  => s"${BLUE}foreign$RESET"
      case _: Typedef.Contract => s"${BLUE}mixin$RESET"
      case _: Typedef.Service  => s"${BLUE}service$RESET"
    }
    val rootMarker = if (member.root) s"$MAGENTA@root$RESET " else ""
    s"$rootMarker$kind $GREEN${member.id.name.name}$RESET"
  }
}
