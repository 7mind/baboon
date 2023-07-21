package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.parser.model.issues.BaboonIssue.TODOIssue
import io.septimalmind.baboon.typer.model.{BaboonFamily, BaboonLineage}
import izumi.functional.IzEitherAggregations.*
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.{NonEmptyList, NonEmptyMap}




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
