package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.*
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.*
import izumi.functional.IzEitherAggregations.*
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NonEmptyList

class BaboonTranslator(acc: Map[TypeId, DomainMember], pkg: Pkg, owner: Owner) {
  def translate(
    defn: RawTLDef,
  ): Either[NonEmptyList[BaboonIssue.TyperIssue], Map[TypeId, DomainMember]] = {
    for {
      translated <- translate(defn.value, defn.root)
      duplications = acc.keySet.intersect(translated.keySet)
      next <- if (duplications.nonEmpty) {
        Left(
          NonEmptyList(
            BaboonIssue.DuplicatedTypeId(duplications, pkg, owner, defn)
          )
        )
      } else {
        Right(translated)
      }
    } yield {
      acc ++ next
    }
  }

  def translate(defn: RawDefn,
                isRoot: Boolean): Either[NonEmptyList[BaboonIssue.TyperIssue],
                                         Map[TypeId, DomainMember.User]] = {
    for {
      id <- convertDefnUserId(defn.name)
      members <- convertMember(id, isRoot, defn)
      uniqueMembers <- members
        .map(m => (m.id: TypeId, m))
        .toList
        .toUniqueMap(
          e => NonEmptyList(BaboonIssue.DuplicatedTypedef(e, pkg, owner, defn))
        )

    } yield {
      uniqueMembers
    }
  }

  private def convertDefnUserId(
    name: RawTypeName,
  ): Either[NonEmptyList[BaboonIssue.TyperIssue], TypeId.User] = {
    for {
      id <- convertId(name, owner)
      userId <- id match {
        case id: TypeId.Builtin =>
          Left(NonEmptyList(BaboonIssue.UnexpectedBuiltin(id, owner)))
        case u: TypeId.User =>
          Right(u)
      }
    } yield {
      userId
    }
  }

  private def convertId(name: RawTypeName,
                        ownedBy: Owner,
  ): Either[NonEmptyList[BaboonIssue.TyperIssue], TypeId] = {
    for {
      name <- Right(TypeName(name.name))
      _ <- Right(()) // TODO: validate name
    } yield {
      if (TypeId.Builtins.collections.map(_.name).contains(name)) {
        TypeId.BuiltinCollection(name)
      } else if (TypeId.Builtins.scalars.map(_.name).contains(name)) {
        TypeId.BuiltinScalar(name)
      } else {
        TypeId.User(pkg, ownedBy, name)
      }
    }
  }

  private def convertMember(id: TypeId.User,
                            isRoot: Boolean,
                            defn: RawDefn): Either[NonEmptyList[
    BaboonIssue.TyperIssue
  ], NonEmptyList[DomainMember.User]] = {

    defn match {
      case d: RawDto  => convertDto(id, isRoot, d).map(d => NonEmptyList(d))
      case e: RawEnum => converEnum(id, isRoot, e).map(e => NonEmptyList(e))
      case a: RawAdt  => convertAdt(id, isRoot, a)
    }
  }

  private def converEnum(
    id: TypeId.User,
    isRoot: Boolean,
    choice: RawEnum
  ): Either[NonEmptyList[BaboonIssue.TyperIssue], DomainMember.User] = {
    for {
      converted <- choice.members.biMapAggregate { raw =>
        for {
          name <- Right(raw.value)
          _ <- Right(()) // TODO: validate names

        } yield {
          EnumMember(name)
        }
      }
      _ <- converted
        .map(m => (m.name.toLowerCase, m))
        .toUniqueMap(
          e => NonEmptyList(BaboonIssue.NonUniqueEnumBranches(e, id))
        )
      nel <- NonEmptyList
        .from(converted)
        .toRight(NonEmptyList(BaboonIssue.EmptyEnum(id)))
    } yield {
      DomainMember.User(isRoot, Typedef.Enum(id, nel))
    }
  }

  private def convertDto(
    id: TypeId.User,
    isRoot: Boolean,
    dto: RawDto
  ): Either[NonEmptyList[BaboonIssue.TyperIssue], DomainMember.User] = {
    for {
      converted <- dto.members.biMapAggregate { raw =>
        for {
          name <- Right(FieldName(raw.field.name.name))
          _ <- Right(()) // TODO: validate names
          tpe <- convertTpe(raw.field.tpe)
        } yield {
          Field(name, tpe)
        }
      }
      _ <- converted
        .map(m => (m.name.name.toLowerCase, m))
        .toUniqueMap(e => NonEmptyList(BaboonIssue.NonUniqueFields(id, e)))
    } yield {
      DomainMember.User(isRoot, Typedef.Dto(id, converted.toList))
    }
  }

  private def convertAdt(id: TypeId.User,
                         isRoot: Boolean,
                         adt: RawAdt): Either[NonEmptyList[
    BaboonIssue.TyperIssue
  ], NonEmptyList[DomainMember.User]] = {
    for {
      sub <- Right(new BaboonTranslator(acc, pkg, Owner.Adt(id)))

      converted <- adt.members.biFlatMapAggregate { raw =>
        for {
          nested <- sub.translate(raw.dto, isRoot = false)
        } yield {
          nested.values
        }
      }

      nel <- NonEmptyList
        .from(converted.map(_.id))
        .toRight(NonEmptyList(BaboonIssue.EmptyAdt(id)))
    } yield {
      NonEmptyList(DomainMember.User(isRoot, Typedef.Adt(id, nel)), converted)
    }
  }

  private def convertTpe(
    tpe: RawTypeRef
  ): Either[NonEmptyList[BaboonIssue.TyperIssue], TypeRef] = {
    tpe match {
      case RawTypeRef.Simple(name) =>
        for {
          id <- convertId(name, Owner.Toplevel)
          asScalar <- id match {
            case scalar: TypeId.Scalar =>
              Right(scalar)
            case o =>
              Left(NonEmptyList(BaboonIssue.ScalarExpected(id)))
          }
        } yield {
          TypeRef.Scalar(asScalar)
        }
      case RawTypeRef.Constructor(name, params) =>
        for {
          id <- convertId(name, Owner.Toplevel)
          asCollection <- id match {
            case coll: TypeId.BuiltinCollection =>
              Right(coll)
            case o =>
              Left(NonEmptyList(BaboonIssue.CollectionExpected(id)))
          }
          args <- params.toList.biMapAggregate(convertTpe)
          nel <- NonEmptyList
            .from(args)
            .toRight(NonEmptyList(BaboonIssue.EmptyGenericArgs(id)))
        } yield {
          TypeRef.Constructor(asCollection, nel)
        }
    }
  }

}
