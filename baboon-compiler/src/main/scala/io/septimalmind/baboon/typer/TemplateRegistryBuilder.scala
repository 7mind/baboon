package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.*
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TyperIssue}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList

/** PR-29.4 typer-early pass: scans the raw member list for template declarations
  * (`RawDto` / `RawAdt` / `RawContract` / `RawService` with `typeParams.nonEmpty`),
  * validates them (duplicate type-param names → `DuplicateTypeParam`), registers them
  * in a `TemplateRegistry`, and **removes** them from the member list so that no
  * downstream pass ever tries to produce a `DomainMember` for a template.
  *
  * After this pass, `members` contains only non-template declarations and aliases.
  * The returned `TemplateRegistry` is carried through `BaboonTyper.runTyper` so that
  * PR-29.5's `TemplateInstantiator` can read it when rewriting alias RHSes.
  *
  * The pass mirrors the class/trait shape of `AdtInheritanceExpander` and is wired via
  * the same distage `make[TemplateRegistryBuilder[F]]` binding.
  */
trait TemplateRegistryBuilder[F[+_, +_]] {
  def build(
    pkg: Pkg,
    members: Seq[RawTLDef],
  ): F[NEList[BaboonIssue], (Seq[RawTLDef], TemplateRegistry)]
}

object TemplateRegistryBuilder {

  class Impl[F[+_, +_]: Error2] extends TemplateRegistryBuilder[F] {

    override def build(
      pkg: Pkg,
      members: Seq[RawTLDef],
    ): F[NEList[BaboonIssue], (Seq[RawTLDef], TemplateRegistry)] = {
      buildRecursive(pkg, members, ownerForCurrent = Owner.Toplevel)
    }

    private def buildRecursive(
      pkg: Pkg,
      members: Seq[RawTLDef],
      ownerForCurrent: Owner,
    ): F[NEList[BaboonIssue], (Seq[RawTLDef], TemplateRegistry)] = {

      F.traverseAccumErrors(members)(m => processMember(pkg, m, ownerForCurrent)).flatMap {
        results =>
          val filteredMembers = results.flatMap(_._1)
          val flat            = results.flatMap(_._2.templates).toList
          val grouped         = flat.groupBy(_._1)
          // Detect duplicate template names at the same namespace level.
          val dupes: List[BaboonIssue] = grouped.collect {
            case (key, entries) if entries.size > 1 =>
              // brief: emit at the meta of the second definition (the one being dropped from the registry)
              val secondMeta = entries.tail.head._2.rawDefn match {
                case RawTemplateDefn.Dto(raw)      => raw.meta
                case RawTemplateDefn.Adt(raw)      => raw.meta
                case RawTemplateDefn.Contract(raw) => raw.meta
                case RawTemplateDefn.Service(raw)  => raw.meta
              }
              val ownerStr = key._2 match {
                case Owner.Toplevel => "<toplevel>"
                case Owner.Ns(path) => path.iterator.map(_.name).mkString(".")
                case adt: Owner.Adt => throw new IllegalStateException(s"Templates cannot be declared inside an ADT scope; got $adt")
              }
              BaboonIssue.Typer(TyperIssue.DuplicateTemplateName(key._3.name, ownerStr, secondMeta))
          }.toList
          NEList.from(dupes) match {
            case Some(errors) => F.fail(errors)
            case None         => F.pure((filteredMembers, TemplateRegistry(flat.toMap)))
          }
      }
    }

    /** Process a single `RawTLDef`, returning `(List[RawTLDef], TemplateRegistry)`.
      * - Non-template nodes: returned as-is with empty registry.
      * - Template nodes: validated, registered, and dropped from the output list.
      * - `RawNamespace` nodes: recursed into with an updated owner.
      */
    private def processMember(
      pkg: Pkg,
      member: RawTLDef,
      ownerForCurrent: Owner,
    ): F[NEList[BaboonIssue], (List[RawTLDef], TemplateRegistry)] = {
      member match {

        case RawTLDef.DTO(_, raw) if raw.typeParams.nonEmpty =>
          for {
            _ <- validateTypeParams(raw.typeParams, raw.name, raw.meta)
            _ <- validateNoDerived(raw.name, raw.derived, raw.meta)
          } yield {
            val body = TemplateBody(raw.typeParams, RawTemplateDefn.Dto(raw))
            val key  = (pkg, ownerForCurrent, TypeName(raw.name.name))
            (List.empty, TemplateRegistry(Map(key -> body)))
          }

        case RawTLDef.ADT(_, raw) if raw.typeParams.nonEmpty =>
          for {
            _ <- validateTypeParams(raw.typeParams, raw.name, raw.meta)
            _ <- validateNoDerived(raw.name, raw.derived, raw.meta)
          } yield {
            val body = TemplateBody(raw.typeParams, RawTemplateDefn.Adt(raw))
            val key  = (pkg, ownerForCurrent, TypeName(raw.name.name))
            (List.empty, TemplateRegistry(Map(key -> body)))
          }

        case RawTLDef.Contract(_, raw) if raw.typeParams.nonEmpty =>
          validateTypeParams(raw.typeParams, raw.name, raw.meta).map {
            _ =>
              val body = TemplateBody(raw.typeParams, RawTemplateDefn.Contract(raw))
              val key  = (pkg, ownerForCurrent, TypeName(raw.name.name))
              (List.empty, TemplateRegistry(Map(key -> body)))
          }

        case RawTLDef.Service(_, raw) if raw.typeParams.nonEmpty =>
          validateTypeParams(raw.typeParams, raw.name, raw.meta).map {
            _ =>
              val body = TemplateBody(raw.typeParams, RawTemplateDefn.Service(raw))
              val key  = (pkg, ownerForCurrent, TypeName(raw.name.name))
              (List.empty, TemplateRegistry(Map(key -> body)))
          }

        case nsTL @ RawTLDef.Namespace(ns) =>
          // Derive the next owner directly from the current namespace name; no separate
          // nsPath threading needed since Owner.Ns already encodes the full path.
          val nextOwner = ownerForCurrent match {
            case Owner.Toplevel => Owner.Ns(List(TypeName(ns.name.name)))
            case Owner.Ns(path) => Owner.Ns(path.toList :+ TypeName(ns.name.name))
            case adt: Owner.Adt => throw new IllegalStateException(s"Namespace declared inside an ADT scope is structurally impossible; got $adt")
          }
          buildRecursive(pkg, ns.defns, nextOwner).map {
            case (rewrittenChildren, registry) =>
              // If all children were templates and were removed, drop the namespace entirely.
              // Keeping an empty namespace would cause ScopeCannotBeEmpty in the initial scope-build phase.
              if (rewrittenChildren.isEmpty) (List.empty, registry)
              else {
                val rewritten = nsTL.copy(value = ns.copy(defns = rewrittenChildren))
                (List(rewritten), registry)
              }
          }

        case other =>
          F.pure((List(other), TemplateRegistry.empty))
      }
    }

    /** Validate a template's type-param list, emitting one `DuplicateTypeParam` issue per
      * repeated name (negative-test matrix #4). All duplicates are reported at once so the
      * user does not have to fix and re-run iteratively.
      */
    private def validateTypeParams(
      typeParams: List[RawTypeName],
      ownerName: RawTypeName,
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], Unit] = {
      val seen  = scala.collection.mutable.HashSet.empty[String]
      val dupes = typeParams.filter(p => !seen.add(p.name)).distinctBy(_.name)

      val issues: List[BaboonIssue] = dupes.map(p => BaboonIssue.Typer(TyperIssue.DuplicateTypeParam(p.name, ownerName.name, meta)))
      NEList.from(issues) match {
        case Some(errors) => F.fail(errors)
        case None         => F.unit
      }
    }

    /** Reject a template body that carries a `: derived[…]` annotation (spec §5.3,
      * locked decision #6). Derivation must be written on the alias, not the template.
      */
    private def validateNoDerived(
      templateName: RawTypeName,
      derived: Set[RawMemberMeta],
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], Unit] = {
      if (derived.nonEmpty) {
        F.fail(BaboonIssue.of(TyperIssue.TemplateBodyCarriesDerived(templateName.name, meta)))
      } else {
        F.unit
      }
    }
  }
}
