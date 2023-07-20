package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.parser.model.issues.BaboonIssue.TODOIssue
import io.septimalmind.baboon.parser.model.{
  RawDomain,
  RawHeader,
  RawTLDef,
  RawVersion
}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.IzEitherAggregations.*
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NonEmptyList

trait BaboonTyper {
  def process(
    model: RawDomain
  ): Either[NonEmptyList[BaboonIssue.TyperIssue], Domain]
}

object BaboonTyper {
  class BaboonTyperImpl() extends BaboonTyper {

    override def process(
      model: RawDomain
    ): Either[NonEmptyList[BaboonIssue.TyperIssue], Domain] = {
      for {
        id <- parsePkg(model.header)
        version <- parseVersion(model.version)
        defs <- runTyper(id, model.members)
        indexedDefs <- defs
          .map(d => (d.id, d))
          .toUniqueMap(_ => NonEmptyList(TODOIssue()))

      } yield {
        Domain(id, version, indexedDefs)
      }
    }

    private def parsePkg(
      header: RawHeader
    ): Either[NonEmptyList[BaboonIssue.TyperIssue], Pkg] = {
      for {
        nel <- NonEmptyList.from(header.name).toRight(NonEmptyList(TODOIssue()))
        // TODO: validate format
      } yield {
        Pkg(nel)
      }
    }

    private def parseVersion(
      version: RawVersion
    ): Either[NonEmptyList[BaboonIssue.TyperIssue], Version] = {
      for {
        v <- Right(version.value)
        // TODO: validate format
      } yield {
        Version(v)
      }
    }

    private def runTyper(
      pkg: Pkg,
      members: Seq[RawTLDef]
    ): Either[NonEmptyList[BaboonIssue.TyperIssue], List[DomainMember]] = {
      for {
        initial <- TypeId.Builtins.all
          .map(id => (id: TypeId, DomainMember.Builtin(id): DomainMember))
          .toUniqueMap(_ => NonEmptyList(TODOIssue()))
        // we don't support inheritance, so order doesn't matter here
        out <- members.biFoldLeft(initial) {
          case (acc, defn) =>
            new BaboonTranslator(acc, pkg, Owner.Toplevel).translate(defn)
        }
      } yield {
        out.values.toList
      }
    }

  }

}
