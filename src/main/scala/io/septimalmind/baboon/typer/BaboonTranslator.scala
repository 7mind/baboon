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
      id <- scopeSupport.resolveUserTypeId(rawDefn.defn.name, path, pkg, rawDefn.defn.meta)
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
          _ <- SymbolNames.validEnumMemberName(name, raw.meta)

        } yield {
          EnumMember(name)
        }
      }
      _ <- converted
        .map(m => (m.name.toLowerCase, m))
        .toUniqueMap(e => NEList(BaboonIssue.NonUniqueEnumBranches(e, id, choice.meta)))
      nel <- NEList
        .from(converted)
        .toRight(NEList(BaboonIssue.EmptyEnum(id, choice.meta)))
    } yield {
      DomainMember.User(isRoot, Typedef.Enum(id, nel), choice.meta)
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
            _ <- SymbolNames.validFieldName(name, f.meta)
            tpe <- convertTpe(f.field.tpe, f.meta)
          } yield {
            Seq(Field(name, tpe))
          }
        case p: RawDtoMember.ParentDef =>
          for {
            id <- scopeSupport.resolveScopedRef(p.parent, path, pkg, p.meta)
            parentDef = defined(id)
            out <- parentDef match {
              case DomainMember.User(_, defn: Typedef.Dto, dto.meta) =>
                Right(defn.fields)
              case o =>
                Left(NEList(BaboonIssue.WrongParent(id, o.id, dto.meta)))
            }

          } yield {
            out
          }

      }
      _ <- converted
        .map(m => (m.name.name.toLowerCase, m))
        .toUniqueMap(e => NEList(BaboonIssue.NonUniqueFields(id, e, dto.meta)))
    } yield {
      DomainMember.User(isRoot, Typedef.Dto(id, converted.toList), dto.meta)
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
              .resolveUserTypeId(member.dto.name, path :+ thisScope, pkg, member.meta)
        )
        .biSequence
      nel <- NEList
        .from(converted)
        .toRight(NEList(BaboonIssue.EmptyAdt(id, adt.meta)))
    } yield {
      NEList(DomainMember.User(isRoot, Typedef.Adt(id, nel), adt.meta))
    }
  }

  private def convertTpe(
                          tpe: RawTypeRef,
                          meta: RawNodeMeta
                        ): Either[NEList[BaboonIssue.TyperIssue], TypeRef] = {
    tpe match {
      case RawTypeRef.Simple(name) =>
        for {
          id <- scopeSupport.resolveTypeId(name, path, pkg, meta)
          asScalar <- id match {
            case scalar: TypeId.Scalar =>
              Right(scalar)
            case _ =>
              Left(NEList(BaboonIssue.ScalarExpected(id, meta)))
          }
        } yield {
          TypeRef.Scalar(asScalar)
        }
      case RawTypeRef.Constructor(name, params) =>
        for {
          id <- scopeSupport.resolveTypeId(name, path, pkg, meta)
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
