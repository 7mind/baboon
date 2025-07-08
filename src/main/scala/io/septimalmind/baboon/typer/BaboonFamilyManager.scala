package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.{RawContent, RawDomain, RawTLDef, RawTypeName, RawVersion}
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.{BaboonFamily, BaboonLineage}
import io.septimalmind.baboon.util.BLogger
import izumi.functional.bio.unsafe.MaybeSuspend2
import izumi.functional.bio.{Error2, F, ParallelErrorAccumulatingOps2}
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.{NEList, NEMap}
import izumi.fundamentals.graphs.{DAG, DG, GraphMeta}
import izumi.fundamentals.graphs.struct.IncidenceMatrix
import izumi.fundamentals.platform.strings.TextTree.Quote

import scala.collection.mutable

trait BaboonFamilyManager[F[+_, +_]] {
  def load(
    definitions: List[BaboonParser.Input]
  ): F[NEList[BaboonIssue], BaboonFamily]
}

object BaboonFamilyManager {
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
          domains.sortBy(d => (d.id.toString, d.version.version)).foreach {
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

    def toposorted[N, M](g: DAG[N, M]): Seq[N] = {
      val roots = g.predecessors.links.view.filter(_._2.isEmpty).keys.toSeq
      def go(out: Seq[N]): Seq[N] = {
        out ++ out.flatMap(n => go(g.successors.links(n).toSeq))
      }
      go(roots)
    }
    case class Key(id: String, version: String)

    private def resolveImports(parsed: List[RawDomain]): F[NEList[BaboonIssue], List[RawDomain]] = {
      import MapTools.*
      import izumi.fundamentals.collections.IzCollections.*

      for {
        indexed   <- F.fromEither(parsed.map(d => (Key(d.header.name.mkString("."), d.version.value), d)).toUniqueMap(e => NEList(???)))
        graphNodes = GraphMeta(indexed)
        deps       = indexed.view.mapValues(d => d.imported.map(i => Key(d.header.name.mkString("."), i.value)).toSet).toMap
        preds      = IncidenceMatrix(deps)
        // TODO: BUG in fromPred, it same as fromSucc but should be transposed
        graph <- F.fromEither(DAG.fromPred(preds.transposed, graphNodes).left.map(e => NEList(???)))
        sorted = toposorted(graph)
      } yield {

        val wip = mutable.HashMap.from(indexed)

        sorted.foreach {
          id =>
            val current = wip(id)
            assert(current.members.includes.isEmpty)

            val importedMembers = current.imported
              .foldLeft(List.empty[(RawTypeName, RawTLDef)]) {
                case (acc, v) =>
                  val imported = wip(Key(id.id, v.value)).members.defs
                    .map(d => (d.value.name, d))
                    .filterNot { case (n, _) => v.without.contains(n) }

                  acc ++ imported
              }.toMultimap

            val currentMembers = current.members.defs.map(d => (d.value.name, d)).toMultimap

            val fullMembers = (importedMembers ++ currentMembers).unwrap.map(_._2)

            wip.put(id, RawDomain(current.header, current.version, None, RawContent(Seq.empty, fullMembers)))
        }

        wip.values.toList
      }

    }

  }
}

// TODO: move to izumi
object MapTools {
  implicit class MapSetOps[K, V](val map: Map[K, Set[V]]) extends AnyVal {
    def unwrap: List[(K, V)] =
      map.toList.flatMap { case (k, vs) => vs.toList.map(v => (k, v)) }
  }

  implicit class MutableMapSetOps[K, V](val map: mutable.Map[K, mutable.Set[V]]) extends AnyVal {
    def unwrap: List[(K, V)] =
      map.toList.flatMap { case (k, vs) => vs.toList.map(v => (k, v)) }
  }
}
