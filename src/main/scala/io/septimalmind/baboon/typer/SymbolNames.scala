package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.RawNodeMeta
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TyperIssue}
import io.septimalmind.baboon.typer.model.{FieldName, TypeName}
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList

object SymbolNames {

  def validEnumMemberName[F[+_, +_]: Error2](name: String, meta: RawNodeMeta): F[NEList[BaboonIssue], Unit] = {
    for {
      _ <- F.when(
        !(name.forall(l => l.isLetterOrDigit || l == '_') && name.head.isLetter)
      )(F.fail(BaboonIssue.of(TyperIssue.BadEnumName(name, meta))))
      _ <- F.when(name.toLowerCase.startsWith("baboon"))(
        F.fail(BaboonIssue.of(TyperIssue.BadEnumName(name, meta)))
      )
    } yield {}
  }

  def validFieldName[F[+_, +_]: Error2](name: FieldName, meta: RawNodeMeta): F[NEList[BaboonIssue], Unit] = {
    for {
      _ <- F.when(
        !(name.name
          .forall(l => l.isLetterOrDigit || l == '_') && name.name.head.isLetter)
      )(F.fail(BaboonIssue.of(TyperIssue.BadFieldName(name.name, meta))))
      _ <- F.when(!name.name.head.isLower)(
        F.fail(BaboonIssue.of(TyperIssue.BadFieldName(name.name, meta)))
      )
      _ <- F.when(name.name.toLowerCase.startsWith("baboon"))(
        F.fail(BaboonIssue.of(TyperIssue.BadFieldName(name.name, meta)))
      )
    } yield {}
  }

  def validTypeName[F[+_, +_]: Error2](name: TypeName, meta: RawNodeMeta): F[NEList[BaboonIssue], Unit] = {
    for {
      _ <- F.when(
        !(name.name
          .forall(l => l.isLetterOrDigit || l == '_') && (name.name.head.isLetter || name.name.head == '_'))
      )(F.fail(BaboonIssue.of(TyperIssue.BadTypeName(name.name, meta))))
      _ <- F.when(name.name.toLowerCase.startsWith("baboon"))(
        F.fail(BaboonIssue.of(TyperIssue.BadFieldName(name.name, meta)))
      )
    } yield {}
  }
}
