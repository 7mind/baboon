package io.septimalmind.baboon.validator

import io.septimalmind.baboon.parser.model.{RawMemberMeta, RawNodeMeta}
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.parser.model.issues.VerificationIssue
import io.septimalmind.baboon.parser.model.issues.VerificationIssue.ConversionIssue
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Conversion.FieldOp
import izumi.functional.bio.{Error2, F, ParallelErrorAccumulatingOps2}
import izumi.fundamentals.collections.nonempty.{NEList, NEMap}

trait BaboonValidator[F[+_, +_]] {
  def validate(
    family: BaboonFamily
  ): F[NEList[BaboonIssue], Unit]
}

object BaboonValidator {
  class BaboonValidatorImpl[F[+_, +_]: Error2: ParallelErrorAccumulatingOps2](
    enquiries: BaboonEnquiries
  ) extends BaboonValidator[F] {

    override def validate(
      family: BaboonFamily
    ): F[NEList[BaboonIssue], Unit] = {
      F.parTraverseAccumErrors_(family.domains.toSeq) {
        case (pkg, lineage) =>
          validateLineage(pkg, lineage)
      }
    }

    private def validateLineage(
      pkg: Pkg,
      lineage: BaboonLineage,
    ): F[NEList[BaboonIssue], Unit] = {
      assert(lineage.pkg == pkg)

      for {
        _ <- F.parTraverseAccumErrors_(lineage.versions.toSeq) {
          case (v, domain) =>
            validateDomain(v, domain)
        }
        _ <- validateEvolution(lineage.evolution, lineage.versions)
      } yield {}
    }

    private def validateDomain(
      version: Version,
      domain: Domain,
    ): F[NEList[BaboonIssue], Unit] = {
      assert(domain.version == version)

      for {
        _ <- checkMissingTypes(domain)
        _ <- checkConventions(domain)
        _ <- checkLoops(domain)
        _ <- checkUniqueness(domain)
        _ <- checkShape(domain)
        _ <- checkPathologicGenerics(domain)
        _ <- checkAnyFields(domain)
        _ <- checkIdentifierFields(domain)
        _ <- checkUserMapKeysEligibility(domain)
        _ <- checkRoots(domain)
      } yield {}

    }

    private def checkLoops(
      domain: Domain
    ): F[NEList[BaboonIssue], Unit] = {
      val filtered = domain.loops.map {
        l =>
          val filtered =
            l.loops.filterNot(
              _.loop.exists(terminatesLoop(_, domain, List.empty))
            )

          l.copy(loops = filtered)

      }
        .filterNot(_.loops.isEmpty)

      F.when(filtered.nonEmpty)(
        F.fail(BaboonIssue.of(VerificationIssue.ReferentialCyclesFound(domain, filtered)))
      )
    }

    private def terminatesLoop(id: TypeId, domain: Domain, path: List[TypeId]): Boolean = {
      val npath = path :+ id
      if (path.contains(id)) {
        false
      } else {
        domain.defs.meta.nodes(id) match {
          case _: DomainMember.Builtin => true
          case u: DomainMember.User =>
            u.defn match {
              case d: Typedef.Dto =>
                allFieldsTerminal(domain, npath, d.fields)
              case d: Typedef.Contract =>
                allFieldsTerminal(domain, npath, d.fields)
              case d: Typedef.Adt =>
                d.members.exists(terminatesLoop(_, domain, npath))
              case _: Typedef.Service =>
                false // TODO
              case _: Typedef.Enum    => true
              case _: Typedef.Foreign => true
            }
        }
      }
    }

    private def allFieldsTerminal(domain: Domain, npath: List[TypeId], f: List[Field]): Boolean = {
      f.map(_.tpe)
        .map(ref => ref.id) // we ignore generic options, BIGs termninate loops, this might change
        .forall(terminatesLoop(_, domain, npath))
    }

    // Enforces the PR-55 (M18.2) field-type rules for `id` (isIdentifier == true) DTOs.
    // Rules per plan §2.2:
    //   (1) Collections (any TypeRef.Constructor) are rejected → IdentifierFieldCollection.
    //   (2) Float scalars (f32/f64/f128) are rejected → IdentifierFieldFloatType.
    //   (3) TypeRef.Any is rejected → IdentifierFieldAny.
    //   (4) User-defined types that are not themselves `id` DTOs are rejected
    //       → IdentifierFieldUserNotIdentifier.
    //   All other scalars (bit, i08…u64, str, uid, tsu, tso, bytes, nested `id`) are accepted.
    private def checkIdentifierFields(
      domain: Domain
    ): F[NEList[BaboonIssue], Unit] = {
      val floatTypes: Set[TypeId] = Set(TypeId.Builtins.f32, TypeId.Builtins.f64, TypeId.Builtins.f128)

      def isIdentifierDto(id: TypeId.User): Boolean = {
        domain.defs.meta.nodes.get(id).exists {
          case DomainMember.User(_, d: Typedef.Dto, _, _) => d.isIdentifier
          case _                                          => false
        }
      }

      F.traverseAccumErrors_(domain.defs.meta.nodes.values) {
        case _: DomainMember.Builtin =>
          F.unit
        case u: DomainMember.User =>
          u.defn match {
            case d: Typedef.Dto if d.isIdentifier =>
              val collectionFields = d.fields.filter(f => f.tpe.isInstanceOf[TypeRef.Constructor])
              val floatFields = d.fields.filter {
                f =>
                  f.tpe match {
                    case TypeRef.Scalar(b) => floatTypes.contains(b)
                    case _                 => false
                  }
              }
              val anyFields = d.fields.filter(f => f.tpe.isInstanceOf[TypeRef.Any])
              val userNotIdFields: List[(Field, TypeId)] = d.fields.collect {
                case f @ Field(_, TypeRef.Scalar(uid: TypeId.User), _) if !isIdentifierDto(uid) =>
                  (f, uid: TypeId)
              }

              for {
                _ <- F.when(collectionFields.nonEmpty)(
                  F.fail(BaboonIssue.of(VerificationIssue.IdentifierFieldCollection(d, collectionFields, u.meta)))
                )
                _ <- F.when(floatFields.nonEmpty)(
                  F.fail(BaboonIssue.of(VerificationIssue.IdentifierFieldFloatType(d, floatFields, u.meta)))
                )
                _ <- F.when(anyFields.nonEmpty)(
                  F.fail(BaboonIssue.of(VerificationIssue.IdentifierFieldAny(d, anyFields, u.meta)))
                )
                _ <- F.when(userNotIdFields.nonEmpty)(
                  F.fail(BaboonIssue.of(VerificationIssue.IdentifierFieldUserNotIdentifier(d, userNotIdFields, u.meta)))
                )
              } yield {}

            case _ =>
              F.unit
          }
      }
    }

    // M19/BAB-A02 (PR-59): rejects user-defined map-key types that are not eligible per the
    // wrapper-eligibility procedure (plan §2.1, decisions Q-M19-1..7, Q-FU-1).
    //
    // Eligibility (recursive, with cycle protection via `seen`):
    //   - TypeRef.Scalar(BuiltinScalar) — accepted (floats included; per Q-M19-2 only WRAPPER
    //     floats are rejected, builtin floats remain accepted as direct map keys).
    //   - TypeRef.Scalar(User) where User is:
    //       Enum                                                         → accepted
    //       Foreign                                                       → accepted (Q-M19-7:
    //                                                                       any single-foreign-field
    //                                                                       data is eligible; the
    //                                                                       user supplies the codec)
    //       Dto with contracts                                            → rejected (WrapperWithContracts)
    //       Dto with isIdentifier=true                                    → accepted (Q-M19-6: ANY id
    //                                                                       is eligible regardless of
    //                                                                       field count; M18 already
    //                                                                       guarantees no float fields)
    //       Dto with exactly one field whose tpe is:
    //         TypeRef.Constructor                                         → rejected (CollectionField,
    //                                                                       also covers `opt[_]` since
    //                                                                       opt is a Constructor)
    //         TypeRef.Scalar(float builtin)                                → rejected (FloatWrapper, the
    //                                                                       Q-M19-2 wrapper-asymmetry override)
    //         otherwise                                                    → recurse
    //       Dto with N≠1 fields and not isIdentifier                       → rejected (MultiFieldNonIdWrapper)
    //       Adt | Contract | Service                                       → rejected (IneligibleUserType)
    //
    // After eligibility, Q-FU-1 derivation parity: when the owning type derives `json` (or `ueba`),
    // any user-typed map key must derive the same. Builtins are exempt (always wire-supported).
    //
    // Cyclic wrappers are normally caught earlier by `checkLoops`; the `seen.contains(u)` arm here
    // is a defensive safeguard for the recursion itself.
    private def checkUserMapKeysEligibility(
      domain: Domain
    ): F[NEList[BaboonIssue], Unit] = {
      import VerificationIssue.IneligibleMapKeyReason
      import VerificationIssue.IneligibleMapKeyReason.*

      val floatScalars: Set[TypeId] = Set(TypeId.Builtins.f32, TypeId.Builtins.f64, TypeId.Builtins.f128)

      // Returns Right(chain) where chain is the ordered list of all user TypeIds visited
      // (including the root key type and every intermediate wrapper), so the caller can
      // apply the Q-FU-1 derivation-parity check to every node in the chain, not just
      // the immediate key type.
      def isEligibleKey(t: TypeRef, seen: Set[TypeId]): Either[IneligibleMapKeyReason, List[TypeId.User]] = t match {
        case TypeRef.Scalar(_: TypeId.BuiltinScalar) =>
          // Builtin scalars are always eligible as direct map keys, including floats
          // (Q-M19-2: only WRAPPER floats are rejected; builtin floats stay accepted).
          Right(Nil)
        case TypeRef.Scalar(u: TypeId.User) if seen.contains(u) =>
          Left(CyclicWrapper(u))
        case TypeRef.Scalar(u: TypeId.User) =>
          domain.defs.meta.nodes.get(u) match {
            case Some(DomainMember.User(_, defn, _, _)) =>
              defn match {
                case _: Typedef.Enum =>
                  Right(List(u))
                case _: Typedef.Foreign =>
                  // Q-M19-7: single-foreign-field data is map-key eligible at face value;
                  // the user supplies the toString/parse codec and takes responsibility.
                  Right(List(u))
                case d: Typedef.Dto if d.isIdentifier =>
                  // Q-M19-6: ANY id is eligible regardless of field count. M18's validator
                  // (checkIdentifierFields) already guarantees: no floats, no collections,
                  // no `any`, no non-id user refs. Multi-field ids reach the JSON map-key
                  // codec via M18's parseRepr/toString machinery (PR-60 emits the wiring).
                  Right(List(u))
                case d: Typedef.Dto if d.contracts.nonEmpty =>
                  Left(WrapperWithContracts(u))
                case d: Typedef.Dto if d.fields.size == 1 =>
                  val f = d.fields.head
                  f.tpe match {
                    case c: TypeRef.Constructor =>
                      // opt[_] / lst[_] / set[_] / map[_,_] are all `Constructor`. Distinguish
                      // opt for a more precise message; everything else is "collection".
                      if (c.id == TypeId.Builtins.opt) Left(OptionField(u))
                      else Left(CollectionField(u))
                    case TypeRef.Scalar(b: TypeId.BuiltinScalar) if floatScalars.contains(b) =>
                      // Q-M19-2 asymmetric override: a wrapper that grounds in float is rejected
                      // even though a direct builtin float key is permitted.
                      Left(FloatWrapper(u))
                    case _ =>
                      // Recurse into the single field, prepending `u` so every node in the
                      // chain is returned to the caller for derivation-parity checking.
                      isEligibleKey(f.tpe, seen + u).map(u :: _)
                  }
                case _: Typedef.Dto =>
                  // N != 1 fields, not isIdentifier — multi-field non-id wrapper.
                  Left(MultiFieldNonIdWrapper(u))
                case _: Typedef.Adt | _: Typedef.Contract | _: Typedef.Service =>
                  Left(IneligibleUserType(u))
              }
            case _ =>
              // Unknown / builtin user-id slot — should not happen; treat as ineligible defensively.
              Left(IneligibleUserType(u))
          }
        case _: TypeRef.Constructor =>
          // Should be unreachable — `checkComplexMapKeys` already rejects nested-collection keys.
          Left(IneligibleUserType(t.id))
        case _: TypeRef.Any =>
          // Should be unreachable — `checkAnyAsMapKey` already rejects `any` keys.
          Left(IneligibleUserType(t.id))
      }

      // Per Q-FU-1: if the owning type EXPLICITLY derives `json` (resp. `ueba`), and the
      // user-typed key does not EXPLICITLY derive the same, fail MapKeyMissingDerivation.
      // Builtin keys skip this check.
      //
      // We use the per-user `DomainMember.User.derivations` set, NOT
      // `domain.derivationRequests`: the latter is the *transitive* closure (every type
      // reachable from a derived root inherits the derivation request) and would silently
      // mask the missing derivation we are trying to detect.
      val explicitlyDerived: Map[String, Set[TypeId]] = {
        val pairs: Iterable[(String, TypeId)] = domain.defs.meta.nodes.values.collect {
          case u: DomainMember.User =>
            u.derivations.collect { case RawMemberMeta.Derived(k) => (k, u.id: TypeId) }
        }.flatten
        pairs.groupBy((p: (String, TypeId)) => p._1).view.mapValues(vs => vs.map((p: (String, TypeId)) => p._2).toSet).toMap
      }
      def derives(id: TypeId, kind: String): Boolean =
        explicitlyDerived.getOrElse(kind, Set.empty[TypeId]).contains(id)

      def collectMapKeyFields(t: TypeRef): List[TypeRef] = t match {
        case TypeRef.Constructor(TypeId.Builtins.map, args) =>
          val key  = args.head
          val rest = args.toList.flatMap(collectMapKeyFields)
          key :: rest
        case TypeRef.Constructor(_, args) =>
          args.toList.flatMap(collectMapKeyFields)
        case _ => Nil
      }

      def checkOnFields(owner: Typedef.User, fields: List[Field], meta: RawNodeMeta, ownerId: TypeId): F[NEList[BaboonIssue], Unit] = {
        val ownerJson = derives(ownerId, "json")
        val ownerUeba = derives(ownerId, "ueba")

        F.traverseAccumErrors_(fields) {
          field =>
            val keyTypes = collectMapKeyFields(field.tpe)
            F.traverseAccumErrors_(keyTypes) {
              keyTpe =>
                isEligibleKey(keyTpe, Set.empty) match {
                  case Left(reason) =>
                    F.fail(BaboonIssue.of(VerificationIssue.IneligibleUserMapKey(owner, field, reason, meta)))
                  case Right(chain) =>
                    // Q-FU-1: apply derivation parity to EVERY user type visited during
                    // eligibility (outer wrapper → innermost). Enums and foreign types are
                    // wire-supported intrinsically (like builtins); skip them.
                    F.traverseAccumErrors_(chain) {
                      u =>
                        val skipDerivationCheck = domain.defs.meta.nodes.get(u) match {
                          case Some(DomainMember.User(_, _: Typedef.Foreign, _, _)) => true
                          case Some(DomainMember.User(_, _: Typedef.Enum, _, _))    => true
                          case _                                                    => false
                        }
                        if (skipDerivationCheck) F.unit
                        else
                          for {
                            _ <- F.when(ownerJson && !derives(u, "json"))(
                              F.fail(BaboonIssue.of(VerificationIssue.MapKeyMissingDerivation(owner, field, u, "json", meta)))
                            )
                            _ <- F.when(ownerUeba && !derives(u, "ueba"))(
                              F.fail(BaboonIssue.of(VerificationIssue.MapKeyMissingDerivation(owner, field, u, "ueba", meta)))
                            )
                          } yield {}
                    }
                }
            }
        }
      }

      F.traverseAccumErrors_(domain.defs.meta.nodes.values) {
        case _: DomainMember.Builtin =>
          F.unit
        case u: DomainMember.User =>
          u.defn match {
            case d: Typedef.Dto =>
              checkOnFields(d, d.fields, u.meta, d.id)
            case c: Typedef.Contract =>
              checkOnFields(c, c.fields, u.meta, c.id)
            case a: Typedef.Adt =>
              checkOnFields(a, a.fields, u.meta, a.id)
            case s: Typedef.Service =>
              val pseudo: List[Field] = s.methods.flatMap {
                m =>
                  val sigF = Field(FieldName(s"${m.name.name}.sig"), m.sig, None)
                  val outF = m.out.map(t => Field(FieldName(s"${m.name.name}.out"), t, None))
                  val errF = m.err.map(t => Field(FieldName(s"${m.name.name}.err"), t, None))
                  List(sigF) ++ outF.toList ++ errF.toList
              }
              checkOnFields(s, pseudo, u.meta, s.id)
            case _ =>
              F.unit
          }
      }
    }

    private def checkRoots(
      domain: Domain
    ): F[NEList[BaboonIssue], Unit] = {
      val roots = domain.defs.meta.nodes.values.collect {
        case v: DomainMember.User if v.root => v.defn
      }.toSeq

      val badRoots = roots.collect { case d: Typedef.Enum => d.id }

      F.when(badRoots.nonEmpty)(
        F.fail(BaboonIssue.of(VerificationIssue.IncorrectRootFound(domain, badRoots)))
      )
    }

    private def checkMissingTypes(
      domain: Domain
    ): F[NEList[BaboonIssue], Unit] = {
      val allDeps = domain.defs.meta.nodes.values.flatMap {
        defn =>
          enquiries.fullDepsOfDefn(defn)
      }.toSet

      val allDefs = domain.defs.meta.nodes.keySet
      val missing = allDeps.diff(allDefs)
      F.when(missing.nonEmpty)(
        F.fail(BaboonIssue.of(VerificationIssue.MissingTypeDef(domain, missing)))
      )
    }

    private def checkUniqueness(
      domain: Domain
    ): F[NEList[BaboonIssue], Unit] = {
      for {
        _ <- {
          val dupes =
            domain.defs.meta.nodes.values.groupBy {
              case DomainMember.Builtin(id) => id.name.name
              case DomainMember.User(_, defn, _, _) =>
                defn.id.owner match {
                  case Owner.Toplevel =>
                    defn.id.name.name
                  case Owner.Adt(id) =>
                    s"${id.name.name}#${defn.id.name.name}"
                  case Owner.Ns(path) =>
                    s"//${path.map(_.name).mkString("/")}#${defn.id.name.name}"

                }
            }
              .filter(_._2.size > 1)
          F.when(dupes.nonEmpty)(
            F.fail(BaboonIssue.of(VerificationIssue.ConflictingTypeIds(domain, dupes)))
          )
        }
        _ <- F.traverseAccumErrors_(domain.defs.meta.nodes.values) {
          case _: DomainMember.Builtin =>
            F.unit
          case u: DomainMember.User =>
            u.defn match {
              case d: Typedef.Dto =>
                validateFields(u, d.fields)
              case e: Typedef.Enum =>
                val dupes =
                  e.members.groupBy(_.name.toLowerCase).filter(_._2.size > 1)
                F.when(dupes.nonEmpty)(
                  F.fail(BaboonIssue.of(VerificationIssue.ConflictingEnumBranches(e, dupes, u.meta)))
                )
              case a: Typedef.Adt =>
                val dupes =
                  a.members
                    .groupBy(_.name.name.toLowerCase)
                    .filter(_._2.size > 1)
                F.when(dupes.nonEmpty)(
                  F.fail(BaboonIssue.of(VerificationIssue.ConflictingAdtBranches(a, dupes, u.meta)))
                )
              case _: Typedef.Foreign =>
                F.unit
              case c: Typedef.Contract =>
                validateFields(u, c.fields)
              case _: Typedef.Service =>
                F.unit // TODO:
            }
        }
      } yield {}
    }

    private def validateFields(u: DomainMember.User, fields: List[Field]): F[NEList[BaboonIssue], Unit] = {
      // TODO: check that no contract fields were removed!!!

      val dupes =
        fields
          .groupBy(_.name.name.toLowerCase)
          .filter(_._2.size > 1)

      F.when(dupes.nonEmpty)(
        F.fail(BaboonIssue.of(VerificationIssue.ConflictingDtoFields(u.id, dupes, u.meta)))
      )
    }

    private def checkPathologicGenerics(
      domain: Domain
    ): F[NEList[BaboonIssue], Unit] = {
      for {
        _ <- F.traverseAccumErrors_(domain.defs.meta.nodes.values) {
          case _: DomainMember.Builtin =>
            F.unit
          case u: DomainMember.User =>
            u.defn match {
              case d: Typedef.Dto =>
                for {
                  _ <- checkDoubleOptions(d, u.meta)
                  _ <- checkComplexSetElements(d, u.meta)
                  _ <- checkComplexMapKeys(d, u.meta)
                } yield {}

              case _ =>
                F.unit
            }
        }
      } yield {}
    }

    private def checkDoubleOptions(
      dto: Typedef.Dto,
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], Unit] = {
      def isDoubleOption(t: TypeRef, path: Seq[TypeId]): Boolean = {

        t match {
          case _: TypeRef.Scalar =>
            false
          case TypeRef.Constructor(id, args) =>
            if (id == TypeId.Builtins.opt && path.lastOption.contains(id)) {
              true
            } else {
              args.exists(a => isDoubleOption(a, path :+ id))
            }
          case _: TypeRef.Any =>
            // `any` carries no generics, so it cannot form a double-option chain. The PR 1.3
            // validator will handle `any`-specific rules (map key, derived[ueba]); PR 1.2 only
            // needs a safe pass-through here.
            false
        }
      }

      val badFields = dto.fields.filter(f => isDoubleOption(f.tpe, Seq.empty))

      F.when(badFields.nonEmpty)(
        F.fail(BaboonIssue.of(VerificationIssue.PathologicGenerics(dto, badFields, meta)))
      )
    }

    private def checkComplexSetElements(
      dto: Typedef.Dto,
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], Unit] = {
      def isDoubleOption(t: TypeRef): Boolean = {
        t match {
          case TypeRef.Constructor(TypeId.Builtins.set, args) =>
            args.head match {
              case _: TypeRef.Constructor => true
              case _                      => false
            }
          case _ =>
            false
        }
      }

      val badFields = dto.fields.filter(f => isDoubleOption(f.tpe))

      F.when(badFields.nonEmpty)(
        F.fail(BaboonIssue.of(VerificationIssue.SetsCantContainGenerics(dto, badFields, meta)))
      )
    }

    private def checkComplexMapKeys(
      dto: Typedef.Dto,
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], Unit] = {
      def isDoubleOption(t: TypeRef): Boolean = {
        t match {
          case TypeRef.Constructor(TypeId.Builtins.map, args) =>
            args.head match {
              case _: TypeRef.Constructor => true
              case _                      => false
            }
          case _ =>
            false
        }
      }

      val badFields = dto.fields.filter(f => isDoubleOption(f.tpe))

      F.when(badFields.nonEmpty)(
        F.fail(BaboonIssue.of(VerificationIssue.MapKeysShouldNotBeGeneric(dto, badFields, meta)))
      )
    }

    // Enforces the PR 1.3 validator rules for `any` / `AnyOpaque` fields.
    // See docs/drafts/20260424-1738-any-opaque-fields.md §"Validator rules".
    //
    // Rules:
    //   (1) Underlying T of `any[T]` (variants D1/D2/D3) must be a user-defined type
    //       (DTO/ADT/Enum) — not a builtin scalar, not a collection, not a nested `any`,
    //       not a foreign type. Foreign types have no intrinsic ueba codec the validator
    //       can reason about; this restriction may be relaxed in a future milestone if
    //       Foreign types gain a ueba-derivation mechanism.
    //   (2) Underlying T must carry `derived[ueba]`.
    //       TODO (M2+): also require `derived[json]` when JSON codegen is enabled for the
    //       containing type. PR 1.3 scope is ueba only.
    //   (3) `any` cannot appear as a map key (any nesting depth).
    //   (4) `any` cannot appear as a set element (any nesting depth).
    //   (5) Other positions (`opt[any]`, `lst[any]`, map value) are allowed.
    //
    // Coverage extends across all `Typedef.User` shapes that carry TypeRefs: Dto fields,
    // Contract fields, Adt own fields (flattened contracts), and Service method
    // sig/out/err. A Contract's fields are also visible on every inheriting DTO; both
    // sites report independently — duplication is acceptable and matches the style of
    // other validators in this file (e.g. `validateFields`).
    private def checkAnyFields(
      domain: Domain
    ): F[NEList[BaboonIssue], Unit] = {
      F.traverseAccumErrors_(domain.defs.meta.nodes.values) {
        case _: DomainMember.Builtin =>
          F.unit
        case u: DomainMember.User =>
          u.defn match {
            case d: Typedef.Dto =>
              checkAnyOnFields(domain, d, d.fields, u.meta)
            case c: Typedef.Contract =>
              checkAnyOnFields(domain, c, c.fields, u.meta)
            case a: Typedef.Adt =>
              checkAnyOnFields(domain, a, a.fields, u.meta)
            case s: Typedef.Service =>
              // Synthesise pseudo-fields whose names point at the offending method slot
              // (`<methodName>.sig`, `<methodName>.out`, `<methodName>.err`). The Field
              // type is the ergonomic carrier the issue printers expect; nothing here
              // is round-tripped through codegen.
              val pseudo: List[Field] = s.methods.flatMap {
                m =>
                  val sigF = Field(FieldName(s"${m.name.name}.sig"), m.sig, None)
                  val outF = m.out.map(t => Field(FieldName(s"${m.name.name}.out"), t, None))
                  val errF = m.err.map(t => Field(FieldName(s"${m.name.name}.err"), t, None))
                  List(sigF) ++ outF.toList ++ errF.toList
              }
              checkAnyOnFields(domain, s, pseudo, u.meta)
            case _ =>
              F.unit
          }
      }
    }

    private def checkAnyOnFields(
      domain: Domain,
      owner: Typedef.User,
      fields: List[Field],
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], Unit] = {
      for {
        _ <- checkAnyUnderlying(domain, owner, fields, meta)
        _ <- checkAnyAsMapKey(owner, fields, meta)
        _ <- checkAnyAsSetElement(owner, fields, meta)
      } yield {}
    }

    private def checkAnyUnderlying(
      domain: Domain,
      owner: Typedef.User,
      fields: List[Field],
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], Unit] = {
      // Collect every `TypeRef.Any(_, Some(u))` reachable through the field type.
      // We only inspect the shape of `u` — the typer has already ensured `u` is well-formed.
      def collectAnyUnderlyings(t: TypeRef): List[TypeRef] = t match {
        case _: TypeRef.Scalar => Nil
        case c: TypeRef.Constructor =>
          c.args.toList.flatMap(collectAnyUnderlyings)
        case TypeRef.Any(_, None) =>
          Nil
        case TypeRef.Any(_, Some(u)) =>
          u :: collectAnyUnderlyings(u)
      }

      // Underlying shape check: only `TypeRef.Scalar(user)` where the user type is a DTO,
      // ADT, or Enum passes. Builtins, collections, nested `any`, services, contracts, and
      // foreign types are rejected per the spec. Rationale: foreign types have no intrinsic
      // ueba codec the validator can reason about; this restriction may be relaxed in a
      // future milestone if Foreign types gain a ueba-derivation mechanism.
      def isUserDataType(t: TypeRef): Boolean = t match {
        case TypeRef.Scalar(id: TypeId.User) =>
          domain.defs.meta.nodes.get(id).exists {
            case DomainMember.User(_, defn, _, _) =>
              defn match {
                case _: Typedef.Dto  => true
                case _: Typedef.Adt  => true
                case _: Typedef.Enum => true
                case _               => false
              }
            case _ => false
          }
        case _ => false
      }

      val uebaDerivation = domain.derivationRequests
        .getOrElse(RawMemberMeta.Derived("ueba"), Set.empty[TypeId])

      def hasUeba(t: TypeRef): Boolean = t match {
        case TypeRef.Scalar(id: TypeId.User) => uebaDerivation.contains(id)
        case _                               => false
      }

      val notUserTypeFields = fields.filter {
        f => collectAnyUnderlyings(f.tpe).exists(u => !isUserDataType(u))
      }

      // Only check the ueba derivation for fields where the underlying IS a valid user type —
      // otherwise the "not user type" issue already fires and dominates.
      val lacksUebaFields = fields.filter {
        f =>
          val underlyings = collectAnyUnderlyings(f.tpe)
          underlyings.nonEmpty && underlyings.forall(isUserDataType) && underlyings.exists(u => !hasUeba(u))
      }

      for {
        _ <- F.when(notUserTypeFields.nonEmpty)(
          F.fail(BaboonIssue.of(VerificationIssue.AnyUnderlyingNotUserType(owner = owner, badFields = notUserTypeFields, meta = meta)))
        )
        _ <- F.when(lacksUebaFields.nonEmpty)(
          F.fail(BaboonIssue.of(VerificationIssue.AnyUnderlyingLacksUebaDerivation(owner = owner, badFields = lacksUebaFields, meta = meta)))
        )
      } yield {}
    }

    // Recursive — walks every nested map position and reports the field if any of them has
    // `any` in the key slot. Does not rely on `checkComplexMapKeys`, which is non-recursive
    // and only rejects builtin-collection keys at the top level.
    private def checkAnyAsMapKey(
      owner: Typedef.User,
      fields: List[Field],
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], Unit] = {
      def anyAsMapKey(t: TypeRef): Boolean = t match {
        case TypeRef.Constructor(TypeId.Builtins.map, args) =>
          args.head.isInstanceOf[TypeRef.Any] || args.toList.exists(anyAsMapKey)
        case TypeRef.Constructor(_, args) =>
          args.toList.exists(anyAsMapKey)
        case _ => false
      }

      val badFields = fields.filter(f => anyAsMapKey(f.tpe))

      F.when(badFields.nonEmpty)(
        F.fail(BaboonIssue.of(VerificationIssue.AnyAsMapKey(owner = owner, badFields = badFields, meta = meta)))
      )
    }

    // Recursive — walks every nested set position and reports the field if any of them has
    // `any` as the element type.
    private def checkAnyAsSetElement(
      owner: Typedef.User,
      fields: List[Field],
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], Unit] = {
      def anyAsSetElement(t: TypeRef): Boolean = t match {
        case TypeRef.Constructor(TypeId.Builtins.set, args) =>
          args.head.isInstanceOf[TypeRef.Any] || args.toList.exists(anyAsSetElement)
        case TypeRef.Constructor(_, args) =>
          args.toList.exists(anyAsSetElement)
        case _ => false
      }

      val badFields = fields.filter(f => anyAsSetElement(f.tpe))

      F.when(badFields.nonEmpty)(
        F.fail(BaboonIssue.of(VerificationIssue.AnyAsSetElement(owner = owner, badFields = badFields, meta = meta)))
      )
    }

    private def checkShape(
      domain: Domain
    ): F[NEList[BaboonIssue], Unit] = {
      F.traverseAccumErrors_(domain.defs.meta.nodes.values) {
        case _: DomainMember.Builtin =>
          F.unit
        case u: DomainMember.User =>
          u.defn match {
            case d: Typedef.Dto =>
              validateFieldShape(u, d.fields)
            case c: Typedef.Contract =>
              validateFieldShape(u, c.fields)
            case e: Typedef.Enum =>
              for {
                _ <- F.when(e.members.isEmpty)(
                  F.fail(BaboonIssue.of(VerificationIssue.EmptyEnumDef(e, u.meta)))
                )
                consts = e.members.toList.flatMap(_.const.toSeq).toSet
                _ <- F.when(
                  consts.nonEmpty && e.members.size != consts.size
                )(
                  F.fail(
                    NEList(
                      VerificationIssue.EitherAllOrNoneEnumMembersMustHaveConstants(e, u.meta): BaboonIssue
                    )
                  )
                )
                _ <- F.when(
                  consts.exists(i => i < Int.MinValue || i > Int.MaxValue)
                )(
                  F.fail(
                    NEList(
                      VerificationIssue.WrongEnumConstant(e, u.meta): BaboonIssue
                    )
                  )
                )
              } yield {}

            case a: Typedef.Adt =>
              F.when(a.members.isEmpty)(
                F.fail(BaboonIssue.of(VerificationIssue.EmptyAdtDef(a, u.meta)))
              )

            case _: Typedef.Foreign =>
              F.unit
            case _: Typedef.Service =>
              F.unit // TODO
          }
      }
    }

    private def validateFieldShape(u: DomainMember.User, f: List[Field]): F[NEList[BaboonIssue], Unit] = {
      for {
        badFields <- F.pure(
          f.map(_.name.name.toLowerCase)
            .filter(_ == u.id.name.name.toLowerCase)
        )
        _ <- F.when(badFields.nonEmpty)(
          F.fail(BaboonIssue.of(VerificationIssue.BadFieldNames(u.id, badFields, u.meta)))
        )
      } yield {}
    }

    private def checkConventions(
      domain: Domain
    ): F[NEList[BaboonIssue], Unit] = {
      F.traverseAccumErrors_(domain.defs.meta.nodes.values) {
        case _: DomainMember.Builtin =>
          F.unit
        case u: DomainMember.User =>
          F.when(u.id.name.name.head == '_')(
            F.fail(BaboonIssue.of(VerificationIssue.UnderscoredDefinitionRetained(u, u.meta)))
          )
      }
    }

    private def validateEvolution(
      evolution: BaboonEvolution,
      versions: NEMap[Version, Domain],
    ): F[NEList[BaboonIssue], Unit] = {

      // val latest = versions(evolution.latest)

      assert(evolution.diffs.keySet == evolution.rules.keySet)

      F.traverseAccumErrors_(evolution.diffs.toSeq) {
        case (v, diff) =>
          validateEvo(
            versions(v.to),
            versions(v.from),
            diff,
            evolution.rules(v),
          )
      }
    }

    private def validateEvo(
      next: Domain,
      prev: Domain,
      diff: BaboonDiff,
      ruleset: BaboonRuleset,
    ): F[NEList[BaboonIssue], Unit] = {
      val nextIds = next.defs.meta.nodes.collect {
        case (id: TypeId.User, _) => id: TypeId
      }.toSet
      val diffIds = diff.diffs.keySet
      val prevIds = prev.defs.meta.nodes.collect {
        case (id: TypeId.User, _) => id: TypeId
      }.toSet
      // Diffs for changed types are keyed by the shared type ID (present in nextIds).
      // Diffs for renamed types are keyed by the OLD type ID (present in prevIds, not nextIds).
      // A truly orphan diff entry is one that belongs to neither version.
      val orphanDiffs        = diffIds.diff(nextIds).diff(prevIds)
      val conversionIds      = ruleset.conversions.map(_.sourceTpe).toSet[TypeId]
      val missingConversions = prevIds.diff(conversionIds)
      val extraConversions   = conversionIds.diff(prevIds)

      for {
        _ <- F.when(orphanDiffs.nonEmpty)(
          F.fail(BaboonIssue.of(VerificationIssue.MissingEvoDiff(prev, next, orphanDiffs)))
        )
        _ <- F.when(
          missingConversions.nonEmpty || extraConversions.nonEmpty
        )(
          F.fail(
            BaboonIssue.of(
              VerificationIssue.MissingEvoConversion(
                prev,
                next,
                missingConversions,
                extraConversions,
              )
            )
          )
        )
        _ <- F.traverseAccumErrors_(ruleset.conversions) {
          case _: Conversion.CustomConversionRequired =>
            F.unit
          case _: Conversion.NonDataTypeTypeNoConversion =>
            F.unit
          case c: Conversion.RemovedTypeNoConversion =>
            F.when(!diff.changes.removed.contains(c.sourceTpe))(
              F.fail(BaboonIssue.of(VerificationIssue.BrokenConversion(c)))
            )
          case c: Conversion.CopyEnumByName =>
            val o = prev.defs.meta.nodes(c.sourceTpe)
            val n = next.defs.meta.nodes(c.targetTpe)
            (o, n) match {
              case (
                    DomainMember.User(_, oe: Typedef.Enum, _, _),
                    DomainMember.User(_, ne: Typedef.Enum, _, _),
                  ) =>
                val oldNames       = oe.members.map(_.name).toSet
                val newNames       = ne.members.map(_.name).toSet
                val mappedOldNames = oldNames.map(name => c.memberMapping.getOrElse(name, name))
                F.when(
                  mappedOldNames.diff(newNames).nonEmpty
                )(
                  F.fail(
                    BaboonIssue.of(
                      VerificationIssue.IncorrectConversionApplication(
                        c,
                        o,
                        n,
                        ConversionIssue.RemovedEnumBranches,
                      )
                    )
                  )
                )
              case _ =>
                F.fail(
                  BaboonIssue.of(
                    VerificationIssue.IncorrectConversionApplication(
                      c,
                      o,
                      n,
                      ConversionIssue.TypeMismatch,
                    )
                  )
                )
            }

          case c: Conversion.DtoConversion =>
            val o = prev.defs.meta.nodes(c.sourceTpe)
            val n = next.defs.meta.nodes(c.targetTpe)
            (o, n) match {
              case (
                    DomainMember.User(_, od: Typedef.Dto, _, _),
                    DomainMember.User(_, nd: Typedef.Dto, _, _),
                  ) =>
                for {
                  newFieldNames <- F.pure(nd.fields.map(_.name).toSet)
                  oldFieldNames <- F.pure(od.fields.map(_.name).toSet)
                  removedFields <- F.pure(c.removed.map(_.name))
                  renamedSources = c.ops.collect {
                    case f: FieldOp.Rename => f.sourceFieldName
                    case f: FieldOp.Redef  => f.sourceFieldName
                  }.toSet
                  removals = oldFieldNames.diff(newFieldNames).diff(renamedSources)
                  _ <- F.when(removals != removedFields)(
                    F.fail(
                      BaboonIssue.of(
                        VerificationIssue.IncorrectConversionApplication(
                          c,
                          o,
                          n,
                          ConversionIssue.RemovedDtoFields,
                        )
                      )
                    )
                  )

                  transfer = c.ops.collect {
                    case f: FieldOp.Transfer => f.targetField.name
                  }.toSet
                  defaults = c.ops.collect {
                    case f: FieldOp.InitializeWithDefault => f.targetField.name
                  }.toSet
                  swap = c.ops.collect {
                    case f: FieldOp.SwapCollectionType => f.fieldName
                  }.toSet
                  precex = c.ops.collect {
                    case f: FieldOp.ExpandPrecision => f.fieldName
                  }.toSet
                  wrap = c.ops.collect {
                    case f: FieldOp.WrapIntoCollection => f.fieldName
                  }.toSet
                  renamed = c.ops.collect {
                    case f: FieldOp.Rename => f.targetField.name
                  }.toSet
                  redef = c.ops.collect {
                    case f: FieldOp.Redef => f.targetField.name
                  }.toSet
                  all = transfer ++ defaults ++ swap ++ wrap ++ precex ++ renamed ++ redef
                  _ <- F.when(newFieldNames != all) {
                    F.fail(
                      BaboonIssue.of(
                        VerificationIssue.IncorrectConversionApplication(
                          c,
                          o,
                          n,
                          ConversionIssue.IncorrectFields,
                        )
                      )
                    )
                  }
                  conflicts = Seq(
                    transfer.intersect(all.diff(transfer)),
                    defaults.intersect(all.diff(defaults)),
                    swap.intersect(all.diff(swap)),
                    wrap.intersect(all.diff(wrap)),
                    precex.intersect(all.diff(precex)),
                  )
                  _ <- F.when(conflicts.exists(_.nonEmpty))(
                    F.fail(
                      BaboonIssue.of(
                        VerificationIssue.IncorrectConversionApplication(
                          c,
                          o,
                          n,
                          ConversionIssue.ConflictingFields,
                        )
                      )
                    )
                  )
                } yield {}

              case _ =>
                F.fail(
                  BaboonIssue.of(
                    VerificationIssue.IncorrectConversionApplication(
                      c,
                      o,
                      n,
                      ConversionIssue.TypeMismatch,
                    )
                  )
                )
            }
          case c: Conversion.CopyAdtBranchByName =>
            val o = prev.defs.meta.nodes(c.sourceTpe)
            val n = next.defs.meta.nodes(c.targetTpe)
            (o, n) match {
              case (
                    DomainMember.User(_, oa: Typedef.Adt, _, _),
                    DomainMember.User(_, na: Typedef.Adt, _, _),
                  ) =>
                val oldNames = oa.members.map(_.name.name).toSet
                val newNames = na.members.map(_.name.name).toSet
                val mappedOldNames = oldNames.map {
                  name =>
                    c.branchMapping.get(name).map(_.name.name).getOrElse(name)
                }
                F.when(
                  mappedOldNames.diff(newNames).nonEmpty
                )(
                  F.fail(
                    BaboonIssue.of(
                      VerificationIssue.IncorrectConversionApplication(
                        c,
                        o,
                        n,
                        ConversionIssue.RemovedAdtBranches,
                      )
                    )
                  )
                )
              case _ =>
                F.fail(
                  BaboonIssue.of(
                    VerificationIssue.IncorrectConversionApplication(
                      c,
                      o,
                      n,
                      ConversionIssue.TypeMismatch,
                    )
                  )
                )
            }
        }
      } yield {}
    }

  }
}
