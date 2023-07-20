package io.septimalmind.baboon

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.parser.model.issues.BaboonIssue.TODOIssue
import io.septimalmind.baboon.typer.BaboonTyper
import io.septimalmind.baboon.typer.model.{Domain, Pkg, Version}
import izumi.functional.IzEitherAggregations.*
import izumi.fundamentals.collections.nonempty.{NonEmptyList, NonEmptyMap}
import izumi.fundamentals.collections.IzCollections.*

case class BaboonLineage(pkg: Pkg, versions: NonEmptyMap[Version, Domain])
case class BaboonFamily(domains: NonEmptyMap[Pkg, BaboonLineage])

trait BaboonFamilyManager {
  def load(
    definitions: List[BaboonParser.Input]
  ): Either[NonEmptyList[BaboonIssue], BaboonFamily]
}

object BaboonFamilyManager {
  class BaboonFamilyManagerImpl() extends BaboonFamilyManager {
    override def load(
      definitions: List[BaboonParser.Input]
    ): Either[NonEmptyList[BaboonIssue], BaboonFamily] = {
      for {
        parser <- Right(new BaboonParser.BaboonParserImpl())
        typer <- Right(new BaboonTyper.BaboonTyperImpl())
        domains <- definitions.toList.biMapAggregate { input =>
          for {
            parsed <- parser.parse(input)
            typed <- typer.process(parsed)
          } yield {
            typed
          }
        }

        lineages <- domains
          .map(d => (d.id, d))
          .toMultimap
          .toSeq
          .biMapAggregate {
            case (pkg, domains) =>
              for {
                uniqueVersions <- domains
                  .map(d => (d.version, d))
                  .toUniqueMap(_ => NonEmptyList(TODOIssue()))
                nel <- NonEmptyMap
                  .from(uniqueVersions)
                  .toRight(NonEmptyList(TODOIssue()))
              } yield {
                BaboonLineage(pkg, nel)
              }
          }

        uniqueLineages <- lineages
          .map(l => (l.pkg, l))
          .toUniqueMap(_ => NonEmptyList(TODOIssue()))

        nem <- NonEmptyMap
          .from(uniqueLineages)
          .toRight(NonEmptyList(TODOIssue()))
      } yield {
        BaboonFamily(nem)
      }

    }
  }
}
