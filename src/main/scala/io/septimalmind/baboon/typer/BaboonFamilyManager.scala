package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.{BaboonFamily, BaboonLineage}
import io.septimalmind.baboon.util.BLogger
import io.septimalmind.baboon.util.functional.ParallelAccumulatingOps2
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.{NEList, NEMap}
import izumi.fundamentals.platform.strings.TextTree.Quote

trait BaboonFamilyManager[F[+_, +_]] {
  def load(
    definitions: List[BaboonParser.Input]
  ): F[NEList[BaboonIssue], BaboonFamily]
}

object BaboonFamilyManager {
  class BaboonFamilyManagerImpl[F[+_, +_]: Error2: ParallelAccumulatingOps2](
    parser: BaboonParser[F],
    typer: BaboonTyper[F],
    comparator: BaboonComparator[F],
    logger: BLogger,
  ) extends BaboonFamilyManager[F] {

    override def load(
      definitions: List[BaboonParser.Input]
    ): F[NEList[BaboonIssue], BaboonFamily] = {
      for {
        domains <- F.parTraverseAccumErrors(definitions) {
          input =>
            for {
              parsed <- parser.parse(input)
              typed  <- typer.process(parsed)
            } yield {
              typed
            }
        }

        _ <- F.pure(
          domains.sortBy(d => (d.id.toString, d.version)).foreach {
            d =>
              logger.message(
                d.id.toString,
                q"${d.version}: retained definitions: ${d.defs.meta.nodes.size}, unreachable definitions: ${d.excludedIds.size}",
              )
          }
        )

        lineages <- F.parTraverseAccumErrors(
          domains
            .map(d => (d.id, d))
            .toMultimap
            .toSeq
        ) {
          case (pkg, domains) =>
            for {
              uniqueVersions <- F.fromEither {
                domains
                  .map(d => (d.version, d))
                  .toUniqueMap(v => NEList(BaboonIssue.NonUniqueDomainVersions(v)))
              }
              nel <- F.fromOption(NEList(BaboonIssue.EmptyDomainFamily(pkg))) {
                NEMap.from(uniqueVersions)
              }
              evo <- comparator.evolve(pkg, nel)
            } yield {
              BaboonLineage(pkg, nel, evo)
            }
        }

        uniqueLineages <- F.fromEither {
          lineages
            .map(l => (l.pkg, l))
            .toUniqueMap(e => NEList(BaboonIssue.NonUniqueLineages(e)))
        }

        nem <- F.fromOption(NEList(BaboonIssue.EmptyFamily(definitions))) {
          NEMap.from(uniqueLineages)
        }
      } yield {
        BaboonFamily(nem)
      }

    }

  }
}
