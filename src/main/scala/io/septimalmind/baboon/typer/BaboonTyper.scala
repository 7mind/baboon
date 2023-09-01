package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.parser.model.{
  RawDomain,
  RawHeader,
  RawTLDef,
  RawVersion
}
import io.septimalmind.baboon.typer.model.*
import izumi.distage.LocalContext
import izumi.functional.IzEither.*
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.graphs.struct.IncidenceMatrix
import izumi.fundamentals.graphs.tools.{Toposort, ToposortLoopBreaker}
import izumi.fundamentals.graphs.{DG, GraphMeta}
import izumi.fundamentals.platform.crypto.IzSha256Hash
import izumi.fundamentals.platform.functional.Identity

import scala.annotation.tailrec

trait BaboonTyper {
  def process(model: RawDomain): Either[NEList[BaboonIssue.TyperIssue], Domain]
}

object BaboonTyper {
  class BaboonTyperImpl(enquiries: BaboonEnquiries,
                        translator: LocalContext[Identity, BaboonTranslator])
      extends BaboonTyper {
    override def process(
      model: RawDomain
    ): Either[NEList[BaboonIssue.TyperIssue], Domain] = {
      for {
        id <- parsePkg(model.header)
        version <- parseVersion(model.version)
        defs <- runTyper(id, model.members)
        indexedDefs <- defs
          .map(d => (d.id, d))
          .toUniqueMap(e => NEList(BaboonIssue.DuplicatedTypedefs(model, e)))
        roots = indexedDefs.collect {
          case (k, v: DomainMember.User) if v.root =>
            (k, v)
        }
        predecessors <- buildDependencies(
          indexedDefs,
          roots,
          roots.keySet.map(t => (t, None)).toList
        )
        predMatrix = IncidenceMatrix(predecessors)
        graph = DG.fromPred(predMatrix, GraphMeta(indexedDefs.filter {
          case (k, _) => predMatrix.links.contains(k)
        }))
//        _ <- LoopDetector.Impl.findLoopMember(graph.predecessors) match {
//          case Some(_) => Left(NEList(TODOTyperIssue()))
//          case None    => Right(())
//        }
        excludedIds = indexedDefs.keySet.diff(graph.meta.nodes.keySet)
        shallowSchema = graph.meta.nodes.view
          .mapValues(enquiries.shallowId)
          .toMap
        sorted <- Toposort
          .cycleBreaking(graph.predecessors, ToposortLoopBreaker.dontBreak)
          .left
          .map(e => NEList(BaboonIssue.CircularDependency(model, e)))
        deepSchema <- computeDeepSchema(id, graph, sorted)
      } yield {
        Domain(id, version, graph, excludedIds, shallowSchema, deepSchema)
      }
    }

    private def computeDeepSchema(
      pkg: Pkg,
      graph: DG[TypeId, DomainMember],
      sorted: Seq[TypeId]
    ): Either[NEList[BaboonIssue.TyperIssue], Map[TypeId, DeepSchemaId]] = {

      for {
        missing <- Right(sorted.filter(id => !graph.meta.nodes.contains(id)))
        _ <- Either.ifThenFail(missing.nonEmpty)(
          NEList(BaboonIssue.MissingTypeId(pkg, missing.toSet))
        )
      } yield {
        sorted.foldLeft(Map.empty[TypeId, DeepSchemaId]) {
          case (idx, id) =>
            assert(!idx.contains(id))
            val defn = graph.meta.nodes(id)
            val deps = enquiries.directDepsOf(defn)

            val depList = deps.toList
              .map(id => (enquiries.wrap(id), idx(id).id))
              .sortBy(_._1)

            val normalizedRepr =
              s"[${enquiries.wrap(id)};${depList
                .map({ case (k, v) => s"$k=deep/$v" })
                .mkString(",")}]"

            idx.updated(id, DeepSchemaId(IzSha256Hash.hash(normalizedRepr)))
        }
      }
    }

    @tailrec
    private def buildDependencies(defs: Map[TypeId, DomainMember],
                                  current: Map[TypeId, DomainMember],
                                  predecessors: List[(TypeId, Option[TypeId])],
    ): Either[NEList[BaboonIssue.TyperIssue], Map[TypeId, Set[TypeId]]] = {
      val nextDepMap = current.toList.flatMap {
        case (id, defn) =>
          enquiries.directDepsOf(defn).toList.map(dep => (id, Some(dep)))
      }
      val nextDeps = nextDepMap.map(_._2).toSet

      val next = defs.collect {
        case (k, v) if nextDeps.contains(Some(k)) =>
          (k, v)
      }

      import izumi.fundamentals.collections.IzCollections.*

      // here we may extract circular dependencies, that is fine
      val newPredecessors = predecessors ++ nextDepMap
      val todo = defs.removedAll(nextDepMap.map(_._1))

      if (next.isEmpty) {
        Right(
          newPredecessors.toMultimapView.view
            .mapValues(_.flatMap(_.toSeq).toSet)
            .toMap
        )
      } else {
        buildDependencies(todo, next, newPredecessors)
      }
    }

    private def parsePkg(
      header: RawHeader
    ): Either[NEList[BaboonIssue.TyperIssue], Pkg] = {
      for {
        nel <- NEList
          .from(header.name)
          .toRight(NEList(BaboonIssue.EmptyPackageId(header)))
        // TODO: validate format
      } yield {
        Pkg(nel)
      }
    }

    private def parseVersion(
      version: RawVersion
    ): Either[NEList[BaboonIssue.TyperIssue], Version] = {
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
    ): Either[NEList[BaboonIssue.TyperIssue], List[DomainMember]] = {
      for {
        initial <- TypeId.Builtins.all
          .map(id => (id: TypeId, DomainMember.Builtin(id): DomainMember))
          .toUniqueMap(e => NEList(BaboonIssue.NonUniqueBuiltins(e)))
        // we don't support inheritance, so order doesn't matter here
        out <- members.biFoldLeft(initial) {
          case (acc, defn) =>
            translator
              .provide(acc)
              .provide(pkg)
              .provide(Owner.Toplevel: Owner)
              .produce()
              .use(_.translate(defn))
        }
      } yield {
        out.values.toList
      }
    }

  }

}
