package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.RawNodeMeta
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.parser.model.issues.BaboonIssue.TyperIssue
import io.septimalmind.baboon.typer.model.{FieldName, TypeName}
import izumi.functional.IzEither.*
import izumi.fundamentals.collections.nonempty.NEList

object SymbolNames {
  def validEnumMemberName(
    name: String,
    meta: RawNodeMeta,
  ): Either[NEList[TyperIssue], Unit] = {
    for {
      _ <- Either.ifThenFail(
        !(name.forall(l => l.isLetterOrDigit || l == '_') && name.head.isLetter)
      )(NEList(BaboonIssue.BadEnumName(name, meta)))
      _ <- Either.ifThenFail(name.toLowerCase.startsWith("baboon"))(
        NEList(BaboonIssue.BadEnumName(name, meta))
      )
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
      _ <- Either.ifThenFail(name.name.toLowerCase.startsWith("baboon"))(
        NEList(BaboonIssue.BadFieldName(name.name, meta))
      )
    } yield {}
  }
  def validTypeName(name: TypeName, meta: RawNodeMeta): Either[NEList[TyperIssue], Unit] = {
    for {
      _ <- Either.ifThenFail(
        !(name.name
          .forall(l => l.isLetterOrDigit || l == '_') && (name.name.head.isLetter || name.name.head == '_'))
      )(NEList(BaboonIssue.BadTypeName(name.name, meta)))
      _ <- Either.ifThenFail(name.name.toLowerCase.startsWith("baboon"))(
        NEList(BaboonIssue.BadFieldName(name.name, meta))
      )
    } yield {}
  }
}
