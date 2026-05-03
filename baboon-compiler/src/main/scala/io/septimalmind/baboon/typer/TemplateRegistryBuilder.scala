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
      buildRecursive(pkg, members, ownerForCurrent = Owner.Toplevel, nsPath = List.empty)
    }

    private def buildRecursive(
      pkg: Pkg,
      members: Seq[RawTLDef],
      ownerForCurrent: Owner,
      nsPath: List[TypeName],
    ): F[NEList[BaboonIssue], (Seq[RawTLDef], TemplateRegistry)] = {

      F.traverseAccumErrors(members)(m => processMember(pkg, m, ownerForCurrent, nsPath)).map {
        results =>
          val filteredMembers = results.flatMap(_._1)
          val mergedTemplates = results.flatMap(_._2.templates).toMap
          (filteredMembers, TemplateRegistry(mergedTemplates))
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
      nsPath: List[TypeName],
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
          val nextNsPath = nsPath :+ TypeName(ns.name.name)
          val nextOwner  = Owner.Ns(nextNsPath)
          buildRecursive(pkg, ns.defns, nextOwner, nextNsPath).map {
            case (rewrittenChildren, registry) =>
              val rewritten = nsTL.copy(value = ns.copy(defns = rewrittenChildren))
              (List(rewritten), registry)
          }

        case other =>
          F.pure((List(other), TemplateRegistry.empty))
      }
    }

    /** Validate a template's type-param list, emitting `DuplicateTypeParam` for repeated names
      * (negative-test matrix #4).
      */
    private def validateTypeParams(
      typeParams: List[RawTypeName],
      ownerName: RawTypeName,
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], Unit] = {
      val seen  = scala.collection.mutable.HashSet.empty[String]
      val dupes = typeParams.filter(p => !seen.add(p.name))

      dupes match {
        case Nil =>
          F.unit
        case first :: _ =>
          F.fail(BaboonIssue.of(TyperIssue.DuplicateTypeParam(first.name, ownerName.name, meta)))
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
