package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.*
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.parser.model.issues.BaboonIssue.{MissingContractFields, ScopedRefToNamespacedGeneric}
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Scope.NestedScope
import io.septimalmind.baboon.typer.model.Typedef.{ForeignEntry, ForeignEntryAttr, ForeignEntryAttrs, MethodDef, MethodName}
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.cli.model.schema.ParserDef.ArgDef

object BaboonTranslator {
  type Factory[F[+_, +_]] = (
    Pkg,
    NestedScope[ExtendedRawDefn],
    Map[TypeId, DomainMember],
  ) => BaboonTranslator[F]
}

class BaboonTranslator[F[+_, +_]: Error2](
  pkg: Pkg,
  defn: NestedScope[ExtendedRawDefn],
  defined: Map[TypeId, DomainMember],
  scopeSupport: ScopeSupport[F],
) {

  def translate(): F[NEList[BaboonIssue.TyperIssue], List[DomainMember]] = {
    val rawDefn = defn.defn
    for {
      id <- scopeSupport.resolveUserTypeId(
        rawDefn.defn.name,
        defn,
        pkg,
        rawDefn.defn.meta,
      )
      members <- convertMember(id, rawDefn, defn)
    } yield {
      members
    }
  }

  private def convertMember(
    id: TypeId.User,
    defn: ExtendedRawDefn,
    thisScope: NestedScope[ExtendedRawDefn],
  ): F[NEList[BaboonIssue.TyperIssue], List[DomainMember.User]] = {
    val root = defn.gcRoot
    defn.defn match {
      case d: RawDto      => convertDto(id, root, d) { case (id, finalFields, contractRefs) => Typedef.Dto(id, finalFields, contractRefs) }.map(d => List(d))
      case e: RawEnum     => converEnum(id, root, e).map(e => List(e))
      case a: RawAdt      => convertAdt(id, root, a, thisScope).map(_.toList)
      case f: RawForeign  => convertForeign(id, root, f).map(_.toList)
      case c: RawContract => convertDto(id, root, c) { case (id, finalFields, contractRefs) => Typedef.Contract(id, finalFields, contractRefs) }.map(d => List(d))

      // namespace itself is not a typedef :3
      case _: RawNamespace => F.pure(List.empty)
      case s: RawService =>
        convertService(id, root, s, thisScope).map(_.toList)
    }
  }

  private def convertForeign(id: TypeId.User, isRoot: Boolean, f: RawForeign): F[NEList[BaboonIssue.TyperIssue], NEList[DomainMember.User]] = {
    for {
      entries <- F.fromEither {
        f.defns
          .map(
            e =>
              (
                e.lang,
                ForeignEntry(
                  e.lang,
                  e.decl,
                  ForeignEntryAttrs(
                    e.attrs.attrs.map(a => ForeignEntryAttr(a.name, a.value))
                  ),
                ),
              )
          )
          .toUniqueMap(e => NEList(BaboonIssue.NonUniqueForeignEntries(e, id, f.meta)))
      }
    } yield {
      NEList(DomainMember.User(isRoot, Typedef.Foreign(id, entries), f.meta))
    }
  }

  private def converEnum(
    id: TypeId.User,
    isRoot: Boolean,
    choice: RawEnum,
  ): F[NEList[BaboonIssue.TyperIssue], DomainMember.User] = {
    for {
      converted <- F.traverseAccumErrors(choice.members) {
        raw =>
          val name = raw.value
          for {
            _ <- SymbolNames.validEnumMemberName(name, raw.meta)
          } yield {
            EnumMember(
              name,
              raw.associated.map {
                case RawEnumConst.RawInt(int) => int
              },
            )
          }
      }
      _ <- F.fromEither {
        converted
          .map(m => (m.name.toLowerCase, m))
          .toUniqueMap(e => NEList(BaboonIssue.NonUniqueEnumBranches(e, id, choice.meta)))
      }
      nel <- F.fromOption(NEList(BaboonIssue.EmptyEnum(id, choice.meta))) {
        NEList.from(converted)
      }
    } yield {
      DomainMember.User(isRoot, Typedef.Enum(id, nel), choice.meta)
    }
  }

  private def dtoFieldToDefs(
    field: RawField,
    meta: RawNodeMeta,
  ): F[NEList[BaboonIssue.TyperIssue], Seq[Field]] = {
    for {
      name <- F.pure(FieldName(field.name.name))
      _    <- SymbolNames.validFieldName(name, meta)
      tpe  <- convertTpe(field.tpe, meta)
    } yield {
      Seq(Field(name, tpe))
    }
  }

  private def dtoParentToDefs(
    parent: ScopedRef,
    meta: RawNodeMeta,
    refMeta: RawNodeMeta,
  ): F[NEList[BaboonIssue.TyperIssue], Seq[Field]] = {
    for {
      id       <- scopeSupport.resolveScopedRef(parent, defn, pkg, refMeta)
      parentDef = defined(id)
      out <- parentDef match {
        case DomainMember.User(_, defn: Typedef.Dto, _) =>
          F.pure(defn.fields)
        case o =>
          F.fail(NEList(BaboonIssue.WrongParent(id, o.id, meta)))
      }
    } yield {
      out
    }
  }

  def readContractContent(id: TypeId.User, meta: RawNodeMeta): F[NEList[BaboonIssue.WrongParent], List[ContractContent]] = {
    for {
      parentDef <- F.pure(defined(id))
      fields <- parentDef match {
        case DomainMember.User(_, defn: Typedef.Contract, _) =>
          for {
            parents <- F.flatTraverseAccumErrors(defn.contracts) {
              c =>
                readContractContent(c, meta)
            }

          } yield {
            parents ++ Set(ContractContent(defn.fields, Set(id)))
          }
        case o =>
          F.fail(NEList(BaboonIssue.WrongParent(id, o.id, meta)))
      }
    } yield {
      fields
    }
  }

  def contractContent(c: RawDtoMember.ContractRef, meta: RawNodeMeta, refMeta: RawNodeMeta): F[NEList[BaboonIssue.TyperIssue], List[ContractContent]] = {
    for {
      id      <- scopeSupport.resolveScopedRef(c.contract.tpe, defn, pkg, refMeta)
      content <- readContractContent(id, meta)
    } yield {
      content
    }
  }

  case class ContractContent(fields: Seq[Field], refs: Set[TypeId.User])

  private def convertDto(
    id: TypeId.User,
    isRoot: Boolean,
    dto: RawDtoid,
  )(produce: (TypeId.User, List[Field], List[TypeId.User]) => Typedef.User
  ): F[NEList[BaboonIssue.TyperIssue], DomainMember.User] = {
    for {
      converted <- F.flatTraverseAccumErrors(dto.members) {
        case p: RawDtoMember.ContractRef =>
          dto match {
            case _: RawDto =>
              contractContent(p, dto.meta, p.meta)
                .map(_.flatMap(_.fields))
            case _: RawContract =>
              F.pure(List.empty)
          }
        case f: RawDtoMember.FieldDef =>
          dtoFieldToDefs(f.field, f.meta)
        case p: RawDtoMember.ParentDef =>
          dtoParentToDefs(p.parent, dto.meta, p.meta)
        case _ =>
          F.pure(Seq.empty)
      }
      removed <- F.flatTraverseAccumErrors(dto.members) {
        case f: RawDtoMember.UnfieldDef =>
          dtoFieldToDefs(f.field, f.meta)
        case p: RawDtoMember.UnparentDef =>
          dtoParentToDefs(p.parent, dto.meta, p.meta)
        case _ =>
          F.pure(Seq.empty)
      }
      intersectionLimiters <- F.flatTraverseAccumErrors(dto.members) {
        case p: RawDtoMember.IntersectionDef =>
          dtoParentToDefs(p.parent, dto.meta, p.meta)
        case _ =>
          F.pure(Seq.empty)
      }

      adtContracts <- dto match {
        case _: RawDto =>
          id.owner match {
            case Owner.Adt(xid) =>
              defined(xid) match {
                case DomainMember.User(_, defn: Typedef.Adt, _) =>
                  F.flatTraverseAccumErrors(defn.contracts) {
                    c => readContractContent(c, dto.meta)
                  }
                case o =>
                  F.fail(NEList(BaboonIssue.WrongParent(id, o.id, dto.meta)))
              }
            case _ =>
              F.pure(List.empty)
          }
        case _: RawContract =>
          F.pure(List.empty)
      }

      localContracts <- F.flatTraverseAccumErrors(dto.members) {
        case p: RawDtoMember.ContractRef =>
          contractContent(p, dto.meta, p.meta)
        case _ =>
          F.pure(Seq.empty)
      }
      contracts      = (adtContracts ++ localContracts).distinct
      contractFields = contracts.flatMap(_.fields)

      removedSet      = removed.toSet
      intersectionSet = intersectionLimiters.toSet

      withoutRemoved = (adtContracts.flatMap(_.fields) ++ converted)
        .filterNot(f => removedSet.contains(f))
        .distinct

      finalFields =
        if (intersectionSet.isEmpty) {
          withoutRemoved
        } else {
          withoutRemoved.filter(f => intersectionSet.contains(f))
        }

      missingIrremovable = contractFields.distinct.diff(withoutRemoved.distinct)
      _ <- dto match {
        case _: RawDto =>
          F.when(missingIrremovable.nonEmpty)(
            F.fail(NEList(MissingContractFields(id, missingIrremovable, dto.meta)))
          )
        case _: RawContract =>
          F.pure(())
      }

      _ <- F.fromEither {
        finalFields
          .map(m => (m.name.name.toLowerCase, m))
          .toUniqueMap(e => NEList(BaboonIssue.NonUniqueFields(id, e, dto.meta)))
      }
      contractRefs = contracts.flatMap(_.refs).distinct
    } yield {
      DomainMember.User(
        isRoot,
        produce(id, finalFields, contractRefs),
        dto.meta,
      )
    }
  }

  private def convertAdt(
    id: TypeId.User,
    isRoot: Boolean,
    adt: RawAdt,
    thisScope: NestedScope[ExtendedRawDefn],
  ): F[NEList[BaboonIssue.TyperIssue], NEList[DomainMember.User]] = {
    for {
      converted <- F.sequenceAccumErrors {
        adt.members.collect { case d: RawAdtMember => d }
          .map(
            member =>
              scopeSupport
                .resolveUserTypeId(member.defn.name, thisScope, pkg, member.meta)
          )
      }
      nel <- F.fromOption(NEList(BaboonIssue.EmptyAdt(id, adt.meta))) {
        NEList.from(converted)
      }
      contracts <- F
        .traverseAccumErrors(adt.contracts) {
          ref =>
            scopeSupport.resolveScopedRef(ref.contract.tpe, defn, pkg, ref.meta)
        }
        .map(_.toList)
      fields <- F
        .flatTraverseAccumErrors(adt.contracts) {
          c =>
            contractContent(c, adt.meta, c.meta)
              .map(_.flatMap(_.fields))
        }
        .map(_.toList)
    } yield {
      NEList(
        DomainMember
          .User(isRoot, Typedef.Adt(id, nel, contracts, fields), adt.meta)
      )
    }
  }

  private def convertService(
    id: TypeId.User,
    isRoot: Boolean,
    svc: RawService,
    thisScope: NestedScope[ExtendedRawDefn],
  ): F[NEList[BaboonIssue.TyperIssue], NEList[DomainMember.User]] = {

    for {
      defs <- F.traverseAccumErrors(svc.defns)(f => convertMethod(svc, f))
      _ <- F.fromEither {
        defs
          .map(m => (m.name.name.toLowerCase, m))
          .toUniqueMap(e => NEList(???))
      }
    } yield {
      NEList(
        DomainMember
          .User(isRoot, Typedef.Service(id, defs.toList), svc.meta)
      )
    }
  }

  private def convertArg(svc: RawService, fn: RawFunc, f: RawFuncArg): F[NEList[BaboonIssue.TyperIssue], (String, TypeRef)] = {

    f match {
      case RawFuncArg.Ref(ref, marker, meta) =>
        for {
          tpe <- convertTpe(ref, meta)
        } yield {
          (marker, tpe)
        }
      case RawFuncArg.Struct(defn) =>
        for {
          tpe <- convertTpe(RawTypeRef.Simple(defn.name, List(svc.name, RawTypeName(fn.name))), defn.meta)
        } yield {
          (defn.name.name, tpe)
        }

    }

  }

  private def convertMethod(svc: RawService, f: RawFunc): F[NEList[BaboonIssue.TyperIssue], MethodDef] = {

    for {
      args   <- F.traverseAccumErrors(f.sig)(arg => convertArg(svc, f, arg))
      inargs  = args.filter(_._1.toLowerCase == "in").map(_._2)
      outargs = args.filter(_._1.toLowerCase == "out").map(_._2)
      errargs = args.filter(_._1.toLowerCase == "err").map(_._2)

      _ <- F.when(outargs.size != 1)(F.fail(???))
      _ <- F.when(outargs.size > 1)(F.fail(???))
      _ <- F.when(errargs.size > 1)(F.fail(???))
    } yield {
      MethodDef(MethodName(f.name), inargs.head, outargs.headOption, errargs.headOption)
    }
  }

  private def convertTpe(
    tpe: RawTypeRef,
    meta: RawNodeMeta,
  ): F[NEList[BaboonIssue.TyperIssue], TypeRef] = {
    tpe match {
      case RawTypeRef.Simple(name, prefix) =>
        for {
          id <- scopeSupport.resolveTypeId(prefix, name, defn, pkg, meta)
          asScalar <- id match {
            case scalar: TypeId.Scalar =>
              F.pure(scalar)
            case _ =>
              F.fail(NEList(BaboonIssue.ScalarExpected(id, meta)))
          }
        } yield {
          TypeRef.Scalar(asScalar)
        }
      case RawTypeRef.Constructor(name, params, prefix) =>
        for {
          _ <- F.when(prefix.nonEmpty)(
            F.fail(NEList(ScopedRefToNamespacedGeneric(prefix, meta)))
          )
          id <- scopeSupport.resolveTypeId(prefix, name, defn, pkg, meta)
          asCollection <- id match {
            case coll: TypeId.BuiltinCollection =>
              F.pure(coll)
            case _ =>
              F.fail(NEList(BaboonIssue.CollectionExpected(id, meta)))
          }
          args <- F.traverseAccumErrors(params.toList)(convertTpe(_, meta))
          nel <- F.fromOption(NEList(BaboonIssue.EmptyGenericArgs(id, meta))) {
            NEList.from(args)
          }
        } yield {
          TypeRef.Constructor(asCollection, nel)
        }
    }
  }

}
