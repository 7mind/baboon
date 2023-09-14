package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.*
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.BaboonTyper.{FullRawDefn, ScopedDefn}
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Scope.NestedScope
import izumi.functional.IzEither.*
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList

class BaboonTranslator(pkg: Pkg,
                       path: NEList[Scope[FullRawDefn]],
                       defined: Map[TypeId, DomainMember],
                       scopeSupport: ScopeSupport) {
  def translate(
    defn: ScopedDefn
  ): Either[NEList[BaboonIssue.TyperIssue], List[DomainMember]] = {

    for {
      rawDefn <- Right(defn.thisScope.defn)
      id <- scopeSupport.resolveUserTypeId(rawDefn.defn.name, path, pkg)
      members <- convertMember(id, rawDefn, defn.thisScope)
    } yield {
      members.toList
    }
  }

  private def convertMember(id: TypeId.User,
                            defn: FullRawDefn,
                            thisScope: NestedScope[FullRawDefn],
  ): Either[NEList[BaboonIssue.TyperIssue], NEList[DomainMember.User]] = {
    val root = defn.gcRoot
    defn.defn match {
      case d: RawDto  => convertDto(id, root, d).map(d => NEList(d))
      case e: RawEnum => converEnum(id, root, e).map(e => NEList(e))
      case a: RawAdt  => convertAdt(id, root, a, thisScope)
    }
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
          _ <- SymbolNames.validEnumMemberName(name)

        } yield {
          EnumMember(name)
        }
      }
      _ <- converted
        .map(m => (m.name.toLowerCase, m))
        .toUniqueMap(e => NEList(BaboonIssue.NonUniqueEnumBranches(e, id)))
      nel <- NEList
        .from(converted)
        .toRight(NEList(BaboonIssue.EmptyEnum(id)))
    } yield {
      DomainMember.User(isRoot, Typedef.Enum(id, nel))
    }
  }

  private def convertDto(
    id: TypeId.User,
    isRoot: Boolean,
    dto: RawDto
  ): Either[NEList[BaboonIssue.TyperIssue], DomainMember.User] = {
    for {
      converted <- dto.members.biFlatTraverse {
        case f: RawDtoMember.FieldDef =>
          for {
            name <- Right(FieldName(f.field.name.name))
            _ <- SymbolNames.validFieldName(name)
            tpe <- convertTpe(f.field.tpe)
          } yield {
            Seq(Field(name, tpe))
          }
        case p: RawDtoMember.ParentDef =>
          for {
            id <- scopeSupport.resolveScopedRef(p.parent, path, pkg)
            parentDef = defined(id)
            out <- parentDef match {
              case DomainMember.User(_, defn: Typedef.Dto) =>
                Right(defn.fields)
              case o =>
                Left(NEList(BaboonIssue.WrongParent(id, o.id)))
            }

          } yield {
            out
          }

      }
      _ <- converted
        .map(m => (m.name.name.toLowerCase, m))
        .toUniqueMap(e => NEList(BaboonIssue.NonUniqueFields(id, e)))
    } yield {
      DomainMember.User(isRoot, Typedef.Dto(id, converted.toList))
    }
  }

  private def convertAdt(id: TypeId.User,
                         isRoot: Boolean,
                         adt: RawAdt,
                         thisScope: NestedScope[FullRawDefn],
  ): Either[NEList[BaboonIssue.TyperIssue], NEList[DomainMember.User]] = {
    for {
      converted <- adt.members
        .map(
          member =>
            scopeSupport
              .resolveUserTypeId(member.dto.name, path :+ thisScope, pkg)
        )
        .biSequence
      nel <- NEList
        .from(converted)
        .toRight(NEList(BaboonIssue.EmptyAdt(id)))
    } yield {
      NEList(DomainMember.User(isRoot, Typedef.Adt(id, nel)))
    }
  }

  private def convertTpe(
    tpe: RawTypeRef
  ): Either[NEList[BaboonIssue.TyperIssue], TypeRef] = {
    tpe match {
      case RawTypeRef.Simple(name) =>
        for {
          id <- scopeSupport.resolveTypeId(name, path, pkg)
          asScalar <- id match {
            case scalar: TypeId.Scalar =>
              Right(scalar)
            case _ =>
              Left(NEList(BaboonIssue.ScalarExpected(id)))
          }
        } yield {
          TypeRef.Scalar(asScalar)
        }
      case RawTypeRef.Constructor(name, params) =>
        for {
          id <- scopeSupport.resolveTypeId(name, path, pkg)
          asCollection <- id match {
            case coll: TypeId.BuiltinCollection =>
              Right(coll)
            case _ =>
              Left(NEList(BaboonIssue.CollectionExpected(id)))
          }
          args <- params.toList.biTraverse(convertTpe)
          nel <- NEList
            .from(args)
            .toRight(NEList(BaboonIssue.EmptyGenericArgs(id)))
        } yield {
          TypeRef.Constructor(asCollection, nel)
        }
    }
  }

}
