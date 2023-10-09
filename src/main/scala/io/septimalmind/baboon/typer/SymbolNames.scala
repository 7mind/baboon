package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.RawNodeMeta
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.parser.model.issues.BaboonIssue.TyperIssue
import io.septimalmind.baboon.typer.model.{FieldName, TypeName}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.functional.IzEither.*

object SymbolNames {
  def validEnumMemberName(name: String, meta: RawNodeMeta): Either[NEList[TyperIssue], Unit] = {
    for {
      _ <- Either.ifThenFail(
        !(name.forall(l => l.isLetterOrDigit || l == '_') && name.head.isLetter)
      )(NEList(BaboonIssue.BadEnumName(name, meta)))
    } yield {}
  }

  def validFieldName(name: FieldName, meta: RawNodeMeta): Either[NEList[TyperIssue], Unit] = {
    for {
      _ <- Either.ifThenFail(
        !(name.name
          .forall(l => l.isLetterOrDigit || l == '_') && name.name.head.isLetter)
      )(NEList(BaboonIssue.BadFieldName(name.name, meta)))
      _ <- Either.ifThenFail(!name.name.head.isLower)(
        NEList(BaboonIssue.BadFieldName(name.name, meta))
      )
    } yield {}
  }
  def validTypeName(name: TypeName, meta: RawNodeMeta): Either[NEList[TyperIssue], Unit] = {
    for {
      _ <- Either.ifThenFail(
        !(name.name
          .forall(l => l.isLetterOrDigit || l == '_') && name.name.head.isLetter)
      )(NEList(BaboonIssue.BadTypeName(name.name, meta)))
    } yield {}
  }
}
