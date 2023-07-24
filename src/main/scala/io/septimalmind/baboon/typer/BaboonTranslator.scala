package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.*
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.parser.model.issues.BaboonIssue.TODOIssue
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
      next <- if (acc.keySet.intersect(translated.keySet).nonEmpty) {
        Left(NonEmptyList(TODOIssue()))
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
      id <- convertUserId(defn.name)
      members <- convertMember(id, isRoot, defn)
      uniqueMembers <- members
        .map(m => (m.id: TypeId, m))
        .toList
        .toUniqueMap(_ => NonEmptyList(TODOIssue()))

    } yield {
      uniqueMembers
    }
  }

  private def convertUserId(
    name: RawTypeName,
  ): Either[NonEmptyList[BaboonIssue.TyperIssue], TypeId.User] = {
    for {
      id <- convertId(name)
      userId <- id match {
        case _: TypeId.Builtin =>
          Left(NonEmptyList(TODOIssue()))
        case u: TypeId.User =>
          Right(u)
      }
    } yield {
      userId
    }
  }

  private def convertId(
    name: RawTypeName,
  ): Either[NonEmptyList[BaboonIssue.TyperIssue], TypeId] = {
    for {
      name <- Right(TypeName(name.name))
      _ <- Right(()) // TODO: validate name
    } yield {
      if (TypeId.Builtins.all.map(_.name).contains(name)) {
        TypeId.Builtin(name)
      } else {
        TypeId.User(pkg, owner, name)
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
    enum: RawEnum
  ): Either[NonEmptyList[BaboonIssue.TyperIssue], DomainMember.User] = {
    for {
      converted <- enum.members.biMapAggregate { raw =>
        for {
          name <- Right(raw.value)
          _ <- Right(()) // TODO: validate names

        } yield {
          EnumMember(name)
        }
      }
      unique <- converted
        .map(m => (m.name.toLowerCase, m))
        .toUniqueMap(_ => NonEmptyList(TODOIssue()))
      nel <- NonEmptyList.from(unique.values).toRight(NonEmptyList(TODOIssue()))
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
      unique <- converted
        .map(m => (m.name.name.toLowerCase, m))
        .toUniqueMap(_ => NonEmptyList(TODOIssue()))
    } yield {
      DomainMember.User(isRoot, Typedef.Dto(id, unique.values.toList))
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
        .toRight(NonEmptyList(TODOIssue()))
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
          id <- convertId(name)
        } yield {
          TypeRef.Scalar(id)
        }
      case RawTypeRef.Constructor(name, params) =>
        for {
          id <- convertId(name)
          args <- params.toList.biMapAggregate(convertTpe)
          nel <- NonEmptyList.from(args).toRight(NonEmptyList(TODOIssue()))
        } yield {
          TypeRef.Constructor(id, nel)
        }
    }
  }

}
