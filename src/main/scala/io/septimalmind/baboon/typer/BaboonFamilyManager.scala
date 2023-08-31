package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.{BaboonFamily, BaboonLineage}
import io.septimalmind.baboon.util.BLogger
import izumi.functional.IzEither.*
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.{NEList, NEMap}
import izumi.fundamentals.platform.strings.TextTree.Quote

trait BaboonFamilyManager {
  def load(
    definitions: List[BaboonParser.Input]
  ): Either[NEList[BaboonIssue], BaboonFamily]
}

object BaboonFamilyManager {
  class BaboonFamilyManagerImpl(parser: BaboonParser,
                                typer: BaboonTyper,
                                comparator: BaboonComparator,
                                logger: BLogger)
      extends BaboonFamilyManager {
    override def load(
      definitions: List[BaboonParser.Input]
    ): Either[NEList[BaboonIssue], BaboonFamily] = {
      for {
        domains <- definitions.biTraverse { input =>
          for {
            parsed <- parser.parse(input)
            typed <- typer.process(parsed)
          } yield {
            typed
          }
        }

        _ <- Right(
          domains.sortBy(d => (d.id.toString, d.version.version)).foreach { d =>
            logger.message(
              d.id.toString,
              q"${d.version}: retained definitions: ${d.defs.meta.nodes.size}, unreachable definitions: ${d.excludedIds.size}"
            )
          }
        )

        lineages <- domains
          .map(d => (d.id, d))
          .toMultimap
          .toSeq
          .biTraverse {
            case (pkg, domains) =>
              for {
                uniqueVersions <- domains
                  .map(d => (d.version, d))
                  .toUniqueMap(
                    v => NEList(BaboonIssue.NonUniqueDomainVersions(v))
                  )
                nel <- NEMap
                  .from(uniqueVersions)
                  .toRight(NEList(BaboonIssue.EmptyDomainFamily(pkg)))
                evo <- comparator.evolve(pkg, nel)
              } yield {
                BaboonLineage(pkg, nel, evo)
              }
          }

        uniqueLineages <- lineages
          .map(l => (l.pkg, l))
          .toUniqueMap(e => NEList(BaboonIssue.NonUniqueLineages(e)))

        nem <- NEMap
          .from(uniqueLineages)
          .toRight(NEList(BaboonIssue.EmptyFamily(definitions)))
      } yield {
        BaboonFamily(nem)
      }

    }
  }
}
