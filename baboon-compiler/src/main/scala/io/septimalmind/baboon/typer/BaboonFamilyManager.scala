package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TyperIssue}
import io.septimalmind.baboon.parser.model.{RawContent, RawDomain, RawTLDef, RawTypeName}
import io.septimalmind.baboon.typer.model.{BaboonFamily, BaboonLineage}
import io.septimalmind.baboon.util.BLogger
import izumi.functional.bio.unsafe.MaybeSuspend2
import izumi.functional.bio.{Error2, F, ParallelErrorAccumulatingOps2}
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.{NEList, NEMap}
import izumi.fundamentals.graphs.struct.AdjacencyPredList
import izumi.fundamentals.graphs.{DAG, GraphMeta}
import izumi.fundamentals.platform.strings.TextTree.Quote

import scala.collection.mutable

trait BaboonFamilyManager[F[+_, +_]] {
  def load(
    definitions: List[BaboonParser.Input]
  ): F[NEList[BaboonIssue], BaboonFamily]
}

object BaboonFamilyManager {
  case class Key(id: String, version: String)

  class BaboonFamilyManagerImpl[F[+_, +_]: Error2: MaybeSuspend2: ParallelErrorAccumulatingOps2](
    parser: BaboonParser[F],
    typer: BaboonTyper[F],
    comparator: BaboonComparator[F],
    logger: BLogger,
  ) extends BaboonFamilyManager[F] {

    override def load(
      definitions: List[BaboonParser.Input]
    ): F[NEList[BaboonIssue], BaboonFamily] = {
      for {
        parsed          <- F.parTraverseAccumErrors(definitions)(parser.parse)
        resolvedImports <- resolveImports(parsed)
        domains         <- F.parTraverseAccumErrors(resolvedImports)(typer.process)
        _ <- F.maybeSuspend {
          domains.sortBy(d => (d.id.toString, d.version)).foreach {
            d =>
              logger.message(
                d.id.toString,
                q"${d.version}: retained definitions: ${d.defs.meta.nodes.size}, unreachable definitions: ${d.excludedIds.size}",
              )
          }
        }

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
                  .toUniqueMap(v => BaboonIssue.of(TyperIssue.NonUniqueDomainVersions(v)))
              }
              nel <- F.fromOption(BaboonIssue.of(TyperIssue.EmptyDomainFamily(pkg))) {
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
            .toUniqueMap(e => BaboonIssue.of(TyperIssue.NonUniqueLineages(e)))
        }

        nem <- F.fromOption(BaboonIssue.of(TyperIssue.EmptyFamily(definitions))) {
          NEMap.from(uniqueLineages)
        }
      } yield {
        BaboonFamily(nem)
      }

    }

    def toposorted[N, M](g: DAG[N, M]): Seq[N] = {
      val roots = g.noPredcessors.toSeq
      def go(out: Seq[N]): Seq[N] = {
        out ++ out.flatMap(n => go(g.successors.links(n).toSeq))
      }
      go(roots)
    }

    def withImports(id: Key, idx: collection.Map[Key, RawDomain]): RawDomain = {
      val current = idx(id)
      assert(current.members.includes.isEmpty)

      val importedMembers = current.imported
        .foldLeft(List.empty[(RawTypeName, RawTLDef)]) {
          case (acc, v) =>
            val importedDom = idx(Key(id.id, v.value))

            val imported = importedDom.members.defs

            val recursivelyImported = importedDom.imported.toSeq.flatMap(i => withImports(Key(id.id, i.value), idx).members.defs)

            val allImported = (imported ++ recursivelyImported).map(d => (d.value.name, d)).filterNot { case (n, _) => v.without.contains(n) }

            acc ++ allImported
        }.toMultimap

      val currentMembers = current.members.defs.map(d => (d.value.name, d)).toMultimap

      val fullMembers = (importedMembers ++ currentMembers).unwrap.map(_._2)

      RawDomain(current.header, current.version, None, RawContent(Seq.empty, fullMembers))
    }

    private def resolveImports(parsed: List[RawDomain]): F[NEList[BaboonIssue], List[RawDomain]] = {
      import izumi.fundamentals.collections.IzCollections.*

      for {
        indexed <- F.fromEither(
          parsed
            .map(d => (Key(d.header.name.mkString("."), d.version.value), d))
            .toUniqueMap(e => BaboonIssue.of(TyperIssue.NonUniqueRawDomainVersion(e)))
        )
        graphNodes = GraphMeta(indexed)
        deps       = indexed.view.mapValues(d => d.imported.map(i => Key(d.header.name.mkString("."), i.value)).toSet).toMap
        preds      = AdjacencyPredList(deps)
        // TODO: BUG in fromPred, it same as fromSucc but should be transposed
        graph <- F.fromEither(DAG.fromPred(preds, graphNodes).left.map(e => BaboonIssue.of(TyperIssue.DagError(e, parsed.head.header.meta))))
        sorted = toposorted(graph)
      } yield {

        val wip = mutable.HashMap.from(indexed)

        sorted.foreach {
          id =>
            wip.put(id, withImports(id, wip))
        }

        wip.values.toList
      }

    }

  }
}
