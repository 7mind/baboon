package io.septimalmind.baboon.scheme

import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.graphs.struct.AdjacencyList
import izumi.fundamentals.graphs.tools.{Toposort, ToposortLoopBreaker}

trait BaboonSchemeRenderer {
  def render(family: BaboonFamily, pkg: Pkg, version: Version): Either[String, String]
}

object BaboonSchemeRenderer {

  class BaboonSchemeRendererImpl() extends BaboonSchemeRenderer {

    override def render(family: BaboonFamily, pkg: Pkg, version: Version): Either[String, String] = {
      for {
        lineage <- family.domains.toMap.get(pkg).toRight(s"Domain not found: $pkg")
        domain  <- lineage.versions.toMap.get(version).toRight(s"Version not found: $version in domain $pkg")
        result  <- renderDomain(domain)
      } yield result
    }

    private def renderDomain(domain: Domain): Either[String, String] = {
      val userNodes = domain.defs.meta.nodes.collect {
        case (id: TypeId.User, m: DomainMember.User) => (id, m)
      }

      val reverseDeps = buildReverseDeps(domain)

      val predMatrix = domain.defs.predecessors.links.collect {
        case (id: TypeId.User, deps) if userNodes.contains(id) =>
          id -> deps.collect { case uid: TypeId.User if userNodes.contains(uid) => uid }
      }

      val sorted = Toposort.cycleBreaking(
        AdjacencyList(predMatrix),
        ToposortLoopBreaker.dontBreak,
      ) match {
        case Left(_)     => userNodes.keys.toList
        case Right(list) => list.toList
      }

      val rootsFirst = sorted.reverse

      val adtOwnedIds = userNodes.keys.filter {
        id =>
          id.owner match {
            case _: Owner.Adt => true
            case _            => false
          }
      }.toSet

      val inlineServiceTypeIds = collectInlineServiceTypeIds(userNodes)

      val topLevelIds = rootsFirst.filterNot(id => adtOwnedIds.contains(id) || inlineServiceTypeIds.contains(id))

      val grouped = groupByNamespace(topLevelIds)

      val sb = new StringBuilder
      sb.append(s"model ${domain.id}\n\n")
      sb.append(s"""version "${domain.version}"\n""")

      renderPragmas(sb, domain)
      sb.append("\n")

      renderGroup(sb, grouped, userNodes, reverseDeps, domain, indent = "")

      Right(sb.toString().trim + "\n")
    }

    private def buildReverseDeps(domain: Domain): Map[TypeId, Set[TypeId.User]] = {
      val result = scala.collection.mutable.Map.empty[TypeId, Set[TypeId.User]]
      domain.defs.predecessors.links.foreach {
        case (dependent, dependencies) =>
          dependent match {
            case uid: TypeId.User =>
              dependencies.foreach {
                dep =>
                  result.updateWith(dep) {
                    case Some(existing) => Some(existing + uid)
                    case None           => Some(Set(uid))
                  }
              }
            case _ =>
          }
      }
      result.toMap
    }

    private sealed trait NamespaceGroup
    private case class TopLevelDef(id: TypeId.User) extends NamespaceGroup
    private case class NamespaceBlock(name: String, children: List[NamespaceGroup]) extends NamespaceGroup

    private def groupByNamespace(ids: List[TypeId.User]): List[NamespaceGroup] = {
      val (toplevel, namespaced) = ids.partition {
        id =>
          id.owner match {
            case Owner.Toplevel => true
            case _              => false
          }
      }

      val nsGroups = namespaced.groupBy {
        id =>
          id.owner match {
            case Owner.Ns(path) => path.map(_.name).toList
            case _              => List.empty
          }
      }

      val nsBlocks = buildNamespaceTree(nsGroups)

      val interleaved = interleaveByOriginalOrder(ids, toplevel.map(TopLevelDef(_)), nsBlocks)
      interleaved
    }

    private def interleaveByOriginalOrder(
      originalOrder: List[TypeId.User],
      toplevelDefs: List[TopLevelDef],
      nsBlocks: List[NamespaceBlock],
    ): List[NamespaceGroup] = {
      val orderMap = originalOrder.zipWithIndex.toMap

      val toplevelItems: List[(Int, NamespaceGroup)] = toplevelDefs.map {
        d => (orderMap.getOrElse(d.id, Int.MaxValue), d)
      }
      val nsItems: List[(Int, NamespaceGroup)] = nsBlocks.map {
        block =>
          val minIdx = firstIdInBlock(block).flatMap(orderMap.get).minOption.getOrElse(Int.MaxValue)
          (minIdx, block)
      }

      (toplevelItems ++ nsItems).sortBy(_._1).map(_._2)
    }

    private def firstIdInBlock(block: NamespaceBlock): List[TypeId.User] = {
      block.children.flatMap {
        case TopLevelDef(id)    => List(id)
        case nb: NamespaceBlock => firstIdInBlock(nb)
      }
    }

    private def buildNamespaceTree(groups: Map[List[String], List[TypeId.User]]): List[NamespaceBlock] = {
      val byPrefix = groups.toList.groupBy(_._1.head)
      byPrefix.toList.sortBy(_._1).map {
        case (nsName, entries) =>
          val (direct, nested) = entries.partition(_._1.size == 1)
          val directDefs       = direct.flatMap(_._2).map(TopLevelDef(_))
          val nestedGroups = if (nested.nonEmpty) {
            val subGroups = nested.map { case (path, ids) => (path.tail, ids) }.toMap
            buildNamespaceTree(subGroups)
          } else List.empty
          NamespaceBlock(nsName, directDefs ++ nestedGroups)
      }
    }

    private def collectInlineServiceTypeIds(userNodes: Map[TypeId.User, DomainMember.User]): Set[TypeId.User] = {
      val result = scala.collection.mutable.Set.empty[TypeId.User]
      userNodes.values.foreach {
        member =>
          member.defn match {
            case service: Typedef.Service =>
              val serviceOwnerPath = service.id.owner match {
                case Owner.Toplevel => Seq.empty[TypeName]
                case Owner.Ns(path) => path.toSeq
                case _              => Seq.empty[TypeName]
              }
              service.methods.foreach {
                method =>
                  val expectedPath = serviceOwnerPath ++ Seq(TypeName(service.id.name.name), TypeName(method.name.name))
                  collectMethodTypeRef(method.sig, expectedPath, userNodes).foreach(result.add)
                  method.out.flatMap(collectMethodTypeRef(_, expectedPath, userNodes)).foreach(result.add)
                  method.err.flatMap(collectMethodTypeRef(_, expectedPath, userNodes)).foreach(result.add)
              }
            case _ =>
          }
      }
      result.toSet
    }

    private def collectMethodTypeRef(ref: TypeRef, expectedNsPath: Seq[TypeName], userNodes: Map[TypeId.User, DomainMember.User]): Option[TypeId.User] = {
      ref match {
        case TypeRef.Scalar(id: TypeId.User) if userNodes.contains(id) =>
          id.owner match {
            case Owner.Ns(path) if path.toSeq == expectedNsPath => Some(id)
            case _                                              => None
          }
        case _ => None
      }
    }

    private def renderPragmas(sb: StringBuilder, domain: Domain): Unit = {
      domain.pragmas.foreach {
        case (key, value) =>
          sb.append(s"\npragma $key = $value")
      }
    }

    private def renderGroup(
      sb: StringBuilder,
      groups: List[NamespaceGroup],
      userNodes: Map[TypeId.User, DomainMember.User],
      reverseDeps: Map[TypeId, Set[TypeId.User]],
      domain: Domain,
      indent: String,
    ): Unit = {
      groups.foreach {
        case TopLevelDef(id) =>
          userNodes.get(id).foreach {
            member =>
              renderDefinition(sb, member, userNodes, reverseDeps, domain, indent)
          }
        case NamespaceBlock(name, children) =>
          sb.append(s"\n${indent}ns $name {\n")
          renderGroup(sb, children, userNodes, reverseDeps, domain, indent + "  ")
          sb.append(s"$indent}\n")
      }
    }

    private def renderDefinition(
      sb: StringBuilder,
      member: DomainMember.User,
      userNodes: Map[TypeId.User, DomainMember.User],
      reverseDeps: Map[TypeId, Set[TypeId.User]],
      domain: Domain,
      indent: String,
    ): Unit = {
      val dependents = reverseDeps
        .getOrElse(member.id, Set.empty)
        .filter(id => domain.defs.meta.nodes.contains(id))
        .toList
        .map(_.name.name)
        .sorted

      if (dependents.nonEmpty) {
        sb.append(s"\n$indent// depended upon by: ${dependents.mkString(", ")}\n")
      } else {
        sb.append(s"\n")
      }

      val rootPrefix = if (member.root) "root " else ""

      member.defn match {
        case dto: Typedef.Dto           => renderDto(sb, dto, member, rootPrefix, indent)
        case adt: Typedef.Adt           => renderAdt(sb, adt, member, rootPrefix, userNodes, reverseDeps, domain, indent)
        case e: Typedef.Enum            => renderEnum(sb, e, member, rootPrefix, indent)
        case foreign: Typedef.Foreign   => renderForeign(sb, foreign, rootPrefix, indent)
        case contract: Typedef.Contract => renderContract(sb, contract, member, rootPrefix, indent)
        case service: Typedef.Service   => renderService(sb, service, member, rootPrefix, userNodes, indent)
      }
    }

    private def renderDerivations(member: DomainMember.User): String = {
      val derivations = member.derivations.toList.collect {
        case io.septimalmind.baboon.parser.model.RawMemberMeta.Derived(id) => s"derived[$id]"
      }.sorted
      if (derivations.nonEmpty) s" : ${derivations.mkString(", ")}" else ""
    }

    private def renderDto(
      sb: StringBuilder,
      dto: Typedef.Dto,
      member: DomainMember.User,
      rootPrefix: String,
      indent: String,
    ): Unit = {
      val derivations = renderDerivations(member)
      sb.append(s"$indent${rootPrefix}data ${dto.id.name.name}$derivations {\n")
      dto.contracts.foreach {
        contractId =>
          sb.append(s"$indent  is ${contractId.name.name}\n")
      }
      dto.fields.foreach {
        field =>
          val wasClause = field.prevName.map(pn => s" was ${pn.name}").getOrElse("")
          sb.append(s"$indent  ${field.name.name}: ${renderTypeRef(field.tpe)}$wasClause\n")
      }
      sb.append(s"$indent}\n")
    }

    private def renderAdt(
      sb: StringBuilder,
      adt: Typedef.Adt,
      member: DomainMember.User,
      rootPrefix: String,
      userNodes: Map[TypeId.User, DomainMember.User],
      reverseDeps: Map[TypeId, Set[TypeId.User]],
      domain: Domain,
      indent: String,
    ): Unit = {
      val derivations = renderDerivations(member)
      sb.append(s"$indent${rootPrefix}adt ${adt.id.name.name}$derivations {\n")
      adt.contracts.foreach {
        contractId =>
          sb.append(s"$indent  is ${contractId.name.name}\n")
      }
      if (adt.contracts.nonEmpty) {
        sb.append(s"\n")
      }
      adt.members.toList.foreach {
        branchId =>
          userNodes.get(branchId).foreach {
            branchMember =>
              renderDefinition(sb, branchMember, userNodes, reverseDeps, domain, indent + "  ")
          }
      }
      sb.append(s"$indent}\n")
    }

    private def renderEnum(
      sb: StringBuilder,
      e: Typedef.Enum,
      member: DomainMember.User,
      rootPrefix: String,
      indent: String,
    ): Unit = {
      val derivations = renderDerivations(member)
      sb.append(s"$indent${rootPrefix}enum ${e.id.name.name}$derivations {\n")
      e.members.toList.foreach {
        m =>
          val constPart = m.const.map(c => s" = $c").getOrElse("")
          val wasPart   = m.prevName.map(p => s" was $p").getOrElse("")
          sb.append(s"$indent  ${m.name}$constPart$wasPart\n")
      }
      sb.append(s"$indent}\n")
    }

    private def renderForeign(
      sb: StringBuilder,
      foreign: Typedef.Foreign,
      rootPrefix: String,
      indent: String,
    ): Unit = {
      sb.append(s"$indent${rootPrefix}foreign ${foreign.id.name.name} {\n")
      foreign.runtimeMapping.foreach { rtRef =>
        sb.append(s"$indent  rt = ${renderTypeRef(rtRef)}\n")
      }
      foreign.bindings.toList.sortBy(_._1.asString).foreach {
        case (lang, entry) =>
          entry.mapping match {
            case Typedef.ForeignMapping.Custom(decl, entryAttrs) =>
              val attrs = if (entryAttrs.attrs.nonEmpty) {
                val attrStr = entryAttrs.attrs.map(a => s""""${a.name}" = "${a.value}"""").mkString(", ")
                s""" with { $attrStr }"""
              } else ""
              sb.append(s"""$indent  ${lang.asString} = "$decl"$attrs\n""")
            case Typedef.ForeignMapping.BaboonRef(typeRef) =>
              sb.append(s"$indent  ${lang.asString} = ${renderTypeRef(typeRef)}\n")
          }
      }
      sb.append(s"$indent}\n")
    }

    private def renderContract(
      sb: StringBuilder,
      contract: Typedef.Contract,
      member: DomainMember.User,
      rootPrefix: String,
      indent: String,
    ): Unit = {
      val derivations = renderDerivations(member)
      sb.append(s"$indent${rootPrefix}contract ${contract.id.name.name}$derivations {\n")
      contract.contracts.foreach {
        contractId =>
          sb.append(s"$indent  is ${contractId.name.name}\n")
      }
      contract.fields.foreach {
        field =>
          val wasClause = field.prevName.map(pn => s" was ${pn.name}").getOrElse("")
          sb.append(s"$indent  ${field.name.name}: ${renderTypeRef(field.tpe)}$wasClause\n")
      }
      sb.append(s"$indent}\n")
    }

    private def renderService(
      sb: StringBuilder,
      service: Typedef.Service,
      member: DomainMember.User,
      rootPrefix: String,
      userNodes: Map[TypeId.User, DomainMember.User],
      indent: String,
    ): Unit = {
      val serviceOwnerPath = service.id.owner match {
        case Owner.Toplevel => Seq.empty[TypeName]
        case Owner.Ns(path) => path.toSeq
        case _              => Seq.empty[TypeName]
      }
      val derivations = renderDerivations(member)
      sb.append(s"$indent${rootPrefix}service ${service.id.name.name}$derivations {\n")
      service.methods.foreach {
        method =>
          val expectedPath = serviceOwnerPath ++ Seq(TypeName(service.id.name.name), TypeName(method.name.name))
          sb.append(s"$indent  def ${method.name.name} (\n")
          renderMethodTypeRef(sb, "in", method.sig, expectedPath, userNodes, indent)
          method.out.foreach(ref => renderMethodTypeRef(sb, "out", ref, expectedPath, userNodes, indent))
          method.err.foreach(ref => renderMethodTypeRef(sb, "err", ref, expectedPath, userNodes, indent))
          sb.append(s"$indent  )\n")
      }
      sb.append(s"$indent}\n")
    }

    private def renderMethodTypeRef(
      sb: StringBuilder,
      role: String,
      ref: TypeRef,
      expectedNsPath: Seq[TypeName],
      userNodes: Map[TypeId.User, DomainMember.User],
      indent: String,
    ): Unit = {
      ref match {
        case TypeRef.Scalar(id: TypeId.User) if isInlineServiceType(id, expectedNsPath, userNodes) =>
          userNodes.get(id).foreach {
            member =>
              member.defn match {
                case dto: Typedef.Dto =>
                  sb.append(s"$indent      data $role {\n")
                  dto.fields.foreach {
                    field =>
                      val wasClause = field.prevName.map(pn => s" was ${pn.name}").getOrElse("")
                      sb.append(s"$indent          ${field.name.name}: ${renderTypeRef(field.tpe)}$wasClause\n")
                  }
                  sb.append(s"$indent      }\n")
                case _ =>
                  sb.append(s"$indent      $role = ${renderTypeRef(ref)}\n")
              }
          }
        case _ =>
          sb.append(s"$indent      $role = ${renderTypeRef(ref)}\n")
      }
    }

    private def isInlineServiceType(id: TypeId.User, expectedNsPath: Seq[TypeName], userNodes: Map[TypeId.User, DomainMember.User]): Boolean = {
      userNodes.contains(id) && (id.owner match {
        case Owner.Ns(path) => path.toSeq == expectedNsPath
        case _              => false
      })
    }

    private def renderTypeRef(ref: TypeRef): String = ref match {
      case TypeRef.Scalar(id: TypeId.User) =>
        id.owner match {
          case Owner.Ns(path) => (path.map(_.name) :+ id.name.name).mkString(".")
          case _              => id.name.name
        }
      case TypeRef.Scalar(id) => id.name.name
      case TypeRef.Constructor(id, args) =>
        s"${id.name.name}[${args.toList.map(renderTypeRef).mkString(", ")}]"
    }
  }
}
