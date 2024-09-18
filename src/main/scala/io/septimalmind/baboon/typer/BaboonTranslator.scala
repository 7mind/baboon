package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.*
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.parser.model.issues.BaboonIssue.{
  MissingContractFields,
  ScopedRefToNamespacedGeneric
}
import io.septimalmind.baboon.typer.BaboonTyper.{
  FullRawDefn,
  ScopeInContext,
  ScopedDefn
}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.IzEither.*
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList

class BaboonTranslator(pkg: Pkg,
                       defn: ScopedDefn,
                       defined: Map[TypeId, DomainMember],
                       scopeSupport: ScopeSupport) {
  def translate()
    : Either[NEList[BaboonIssue.TyperIssue], List[DomainMember]] = {

    for {
      rawDefn <- Right(defn.thisScope.defn)
      id <- scopeSupport.resolveUserTypeId(
        rawDefn.defn.name,
        defn.sic,
        pkg,
        rawDefn.defn.meta
      )
      members <- convertMember(id, rawDefn, defn.sic)
    } yield {
      members
    }
  }

  private def convertMember(id: TypeId.User,
                            defn: FullRawDefn,
                            thisScope: ScopeInContext,
  ): Either[NEList[BaboonIssue.TyperIssue], List[DomainMember.User]] = {
    val root = defn.gcRoot
    defn.defn match {
      case d: RawDto =>
        convertDto(id, root, d) {
          case (id, finalFields, contractRefs) =>
            Typedef.Dto(id, finalFields, contractRefs)
        }.map(d => List(d))
      case e: RawEnum    => converEnum(id, root, e).map(e => List(e))
      case a: RawAdt     => convertAdt(id, root, a, thisScope).map(_.toList)
      case f: RawForeign => convertForeign(id, root, f).map(_.toList)
      case c: RawContract =>
        convertDto(id, root, c) {
          case (id, finalFields, contractRefs) =>
            Typedef.Contract(id, finalFields, contractRefs)
        }.map(d => List(d))
      case _: RawNamespace => // namespace itself is not a typedef :3
        Right(List.empty)
    }
  }

  private def convertForeign(id: TypeId.User,
                             isRoot: Boolean,
                             f: RawForeign,
  ): Either[NEList[BaboonIssue.TyperIssue], NEList[DomainMember.User]] = {
    Right(
      NEList(DomainMember.User(isRoot, Typedef.Foreign(id, f.defns), f.meta))
    )
  }

  private def converEnum(
    id: TypeId.User,
    isRoot: Boolean,
    choice: RawEnum
  ): Either[NEList[BaboonIssue.TyperIssue], DomainMember.User] = {
    for {
      converted <- choice.members.biTraverse { raw =>
        for {
          name <- Right(raw.value)
          _ <- SymbolNames.validEnumMemberName(name, raw.meta)

        } yield {
          EnumMember(name, raw.associated.map {
            case RawEnumConst.RawInt(int) => int
          })
        }
      }
      _ <- converted
        .map(m => (m.name.toLowerCase, m))
        .toUniqueMap(
          e => NEList(BaboonIssue.NonUniqueEnumBranches(e, id, choice.meta))
        )
      nel <- NEList
        .from(converted)
        .toRight(NEList(BaboonIssue.EmptyEnum(id, choice.meta)))
    } yield {
      DomainMember.User(isRoot, Typedef.Enum(id, nel), choice.meta)
    }
  }

  private def dtoFieldToDefs(
    field: RawField,
    meta: RawNodeMeta
  ): Either[NEList[BaboonIssue.TyperIssue], Seq[Field]] = {
    for {
      name <- Right(FieldName(field.name.name))
      _ <- SymbolNames.validFieldName(name, meta)
      tpe <- convertTpe(field.tpe, meta)
    } yield {
      Seq(Field(name, tpe))
    }
  }

  private def dtoParentToDefs(
    parent: ScopedRef,
    meta: RawNodeMeta,
    refMeta: RawNodeMeta
  ): Either[NEList[BaboonIssue.TyperIssue], Seq[Field]] = {
    for {
      id <- scopeSupport.resolveScopedRef(parent, defn.sic, pkg, refMeta)
      parentDef = defined(id)
      out <- parentDef match {
        case DomainMember.User(_, defn: Typedef.Dto, _) =>
          Right(defn.fields)
        case o =>
          Left(NEList(BaboonIssue.WrongParent(id, o.id, meta)))
      }
    } yield {
      out
    }
  }

  def readContractContent(id: TypeId.User,
                          meta: RawNodeMeta,
  ): Either[NEList[BaboonIssue.WrongParent], List[ContractContent]] = {
    for {
      parentDef <- Right(defined(id))
      fields <- parentDef match {
        case DomainMember.User(_, defn: Typedef.Contract, _) =>
          for {
            parents <- defn.contracts.map { c =>
              readContractContent(c, meta)
            }.biFlatten

          } yield {
            parents ++ Set(ContractContent(defn.fields, Set(id)))
          }
        case o =>
          Left(NEList(BaboonIssue.WrongParent(id, o.id, meta)))
      }
    } yield {
      fields
    }
  }

  def contractContent(c: RawDtoMember.ContractRef,
                      meta: RawNodeMeta,
                      refMeta: RawNodeMeta,
  ): Either[NEList[BaboonIssue.TyperIssue], List[ContractContent]] = {
    for {
      id <- scopeSupport.resolveScopedRef(
        c.contract.tpe,
        defn.sic,
        pkg,
        refMeta
      )
      content <- readContractContent(id, meta)
    } yield {
      content
    }
  }

  case class ContractContent(fields: Seq[Field], refs: Set[TypeId.User])

  private def convertDto(id: TypeId.User, isRoot: Boolean, dto: RawDtoid)(
    produce: (TypeId.User, List[Field], List[TypeId.User]) => Typedef.User
  ): Either[NEList[BaboonIssue.TyperIssue], DomainMember.User] = {
    for {
      converted <- dto.members.biFlatTraverse {
        case p: RawDtoMember.ContractRef =>
          dto match {
            case _: RawDto =>
              contractContent(p, dto.meta, p.meta)
                .map(_.flatMap(_.fields))
            case _: RawContract =>
              Right(List.empty)
          }
        case f: RawDtoMember.FieldDef =>
          dtoFieldToDefs(f.field, f.meta)
        case p: RawDtoMember.ParentDef =>
          dtoParentToDefs(p.parent, dto.meta, p.meta)
        case _ =>
          Right(Seq.empty)
      }
      removed <- dto.members.biFlatTraverse {
        case f: RawDtoMember.UnfieldDef =>
          dtoFieldToDefs(f.field, f.meta)
        case p: RawDtoMember.UnparentDef =>
          dtoParentToDefs(p.parent, dto.meta, p.meta)
        case _ =>
          Right(Seq.empty)
      }
      intersectionLimiters <- dto.members.biFlatTraverse {
        case p: RawDtoMember.IntersectionDef =>
          dtoParentToDefs(p.parent, dto.meta, p.meta)
        case _ =>
          Right(Seq.empty)
      }

      adtContracts <- dto match {
        case _: RawDto =>
          id.owner match {
            case Owner.Adt(xid) =>
              defined(xid) match {
                case DomainMember.User(_, defn: Typedef.Adt, _) =>
                  defn.contracts
                    .map(c => readContractContent(c, dto.meta))
                    .biFlatten
                case o =>
                  Left(NEList(BaboonIssue.WrongParent(id, o.id, dto.meta)))
              }
            case _ =>
              Right(List.empty)
          }
        case _: RawContract =>
          Right(List.empty)
      }

      localContracts <- dto.members.biFlatTraverse {
        case p: RawDtoMember.ContractRef =>
          contractContent(p, dto.meta, p.meta)
        case _ =>
          Right(Seq.empty)
      }
      contracts = (adtContracts ++ localContracts).distinct
      contractFields = contracts.flatMap(_.fields)

      removedSet = removed.toSet
      intersectionSet = intersectionLimiters.toSet

      withoutRemoved = (adtContracts.flatMap(_.fields) ++ converted)
        .filterNot(f => removedSet.contains(f))
        .distinct

      finalFields = if (intersectionSet.isEmpty) {
        withoutRemoved
      } else {
        withoutRemoved.filter(f => intersectionSet.contains(f))
      }

      missingIrremovable = contractFields.distinct.diff(withoutRemoved.distinct)
      _ <- dto match {
        case _: RawDto =>
          Either.ifThenFail(missingIrremovable.nonEmpty)(
            NEList(MissingContractFields(id, missingIrremovable, dto.meta))
          )
        case _: RawContract =>
          Right(())
      }

      _ <- finalFields
        .map(m => (m.name.name.toLowerCase, m))
        .toUniqueMap(e => NEList(BaboonIssue.NonUniqueFields(id, e, dto.meta)))
      contractRefs = contracts.flatMap(_.refs).distinct
    } yield {
      DomainMember.User(
        isRoot,
        produce(id, finalFields, contractRefs),
        dto.meta
      )
    }
  }

  private def convertAdt(id: TypeId.User,
                         isRoot: Boolean,
                         adt: RawAdt,
                         thisScope: ScopeInContext,
  ): Either[NEList[BaboonIssue.TyperIssue], NEList[DomainMember.User]] = {
    for {
      converted <- adt.members
        .collect { case d: RawAdtMember => d }
        .map(
          member =>
            scopeSupport
              .resolveUserTypeId(member.defn.name, thisScope, pkg, member.meta)
        )
        .biSequence
      nel <- NEList
        .from(converted)
        .toRight(NEList(BaboonIssue.EmptyAdt(id, adt.meta)))
      contracts <- adt.contracts
        .map(
          ref =>
            scopeSupport
              .resolveScopedRef(ref.contract.tpe, defn.sic, pkg, ref.meta)
        )
        .biSequence
        .map(_.toList)
      fields <- adt.contracts
        .biFlatTraverse { c =>
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

  private def convertTpe(
    tpe: RawTypeRef,
    meta: RawNodeMeta
  ): Either[NEList[BaboonIssue.TyperIssue], TypeRef] = {
    tpe match {
      case RawTypeRef.Simple(name, prefix) =>
        for {
          id <- scopeSupport.resolveTypeId(prefix, name, defn.sic, pkg, meta)
          asScalar <- id match {
            case scalar: TypeId.Scalar =>
              Right(scalar)
            case _ =>
              Left(NEList(BaboonIssue.ScalarExpected(id, meta)))
          }
        } yield {
          TypeRef.Scalar(asScalar)
        }
      case RawTypeRef.Constructor(name, params, prefix) =>
        for {
          _ <- Either.ifThenFail(prefix.nonEmpty)(
            NEList(ScopedRefToNamespacedGeneric(prefix, meta))
          )
          id <- scopeSupport.resolveTypeId(prefix, name, defn.sic, pkg, meta)
          asCollection <- id match {
            case coll: TypeId.BuiltinCollection =>
              Right(coll)
            case _ =>
              Left(NEList(BaboonIssue.CollectionExpected(id, meta)))
          }
          args <- params.toList.biTraverse(convertTpe(_, meta))
          nel <- NEList
            .from(args)
            .toRight(NEList(BaboonIssue.EmptyGenericArgs(id, meta)))
        } yield {
          TypeRef.Constructor(asCollection, nel)
        }
    }
  }

}
