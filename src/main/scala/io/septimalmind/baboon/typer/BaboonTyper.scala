package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.parser.model.issues.BaboonIssue.TODOTyperIssue
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
import izumi.fundamentals.graphs.struct.IncidenceMatrix
import izumi.fundamentals.graphs.tools.{Toposort, ToposortLoopBreaker}
import izumi.fundamentals.graphs.{DG, GraphMeta}
import izumi.fundamentals.platform.crypto.IzSha256Hash

import scala.annotation.tailrec

trait BaboonTyper {
  def process(
    model: RawDomain
  ): Either[NonEmptyList[BaboonIssue.TyperIssue], Domain]
}

object BaboonTyper {
  class BaboonTyperImpl() extends BaboonTyper {
    private val enquiries = new BaboonEnquiries.BaboonEnquiriesImpl()

    override def process(
      model: RawDomain
    ): Either[NonEmptyList[BaboonIssue.TyperIssue], Domain] = {
      for {
        id <- parsePkg(model.header)
        version <- parseVersion(model.version)
        defs <- runTyper(id, model.members)
        indexedDefs <- defs
          .map(d => (d.id, d))
          .toUniqueMap(_ => NonEmptyList(TODOTyperIssue()))
        roots = indexedDefs.collect {
          case (k, v: DomainMember.User) if v.root =>
            (k, v)
        }
        predecessors <- buildDependencies(indexedDefs, roots, List.empty)
        predMatrix = IncidenceMatrix(predecessors)
        graph = DG.fromPred(predMatrix, GraphMeta(indexedDefs.filter {
          case (k, _) => predMatrix.links.contains(k)
        }))
//        _ <- LoopDetector.Impl.findLoopMember(graph.predecessors) match {
//          case Some(_) => Left(NonEmptyList(TODOTyperIssue()))
//          case None    => Right(())
//        }
        excludedIds = indexedDefs.keySet.diff(graph.meta.nodes.keySet)
        shallowSchema = graph.meta.nodes.view
          .mapValues(enquiries.shallowId)
          .toMap
        sorted <- Toposort
          .cycleBreaking(graph.predecessors, ToposortLoopBreaker.dontBreak)
          .left
          .map(_ => NonEmptyList(TODOTyperIssue()))
        deepSchema <- computeDeepSchema(graph, shallowSchema, sorted)
      } yield {
        println(sorted)
        Domain(id, version, graph, excludedIds, shallowSchema, deepSchema)
      }
    }

    private def computeDeepSchema(graph: DG[TypeId, DomainMember],
                                  shallowSchema: Map[TypeId, ShallowSchemaId],
                                  sorted: Seq[TypeId])
      : Either[NonEmptyList[BaboonIssue.TyperIssue], Map[TypeId,
                                                         DeepSchemaId]] = {
      val out = sorted.foldLeft(Map.empty[TypeId, DeepSchemaId]) {
        case (idx, id) =>
          assert(!idx.contains(id))
          val defn = graph.meta.nodes(id)
          val deps = enquiries.directDepsOf(defn)

          val shallowId = shallowSchema(id)
          val depList = deps.toList
            .map(id => (enquiries.wrap(id), idx(id).id))
            .sortBy(_._1)

          val normalizedRepr =
            s"[${enquiries.wrap(id)};${shallowId};${depList.mkString(",")}]"

          idx.updated(id, DeepSchemaId(IzSha256Hash.hash(normalizedRepr)))
      }
      Right(out)
    }

    @tailrec
    private def buildDependencies(defs: Map[TypeId, DomainMember],
                                  current: Map[TypeId, DomainMember],
                                  predecessors: List[(TypeId, TypeId)],
    ): Either[NonEmptyList[BaboonIssue.TyperIssue], Map[TypeId, Set[TypeId]]] = {
      val nextDepMap = current.toList.flatMap {
        case (id, defn) =>
          enquiries.directDepsOf(defn).toList.map(dep => (id, dep))
      }
      val nextDeps = nextDepMap.map(_._2).toSet

      val next = defs.collect {
        case (k, v) if nextDeps.contains(k) =>
          (k, v)
      }

      import izumi.fundamentals.collections.IzCollections.*

      // here we may extract circular dependencies, that is fine
      val newPredecessors = predecessors ++ nextDepMap
      val todo = defs.removedAll(nextDepMap.map(_._1))

      if (next.isEmpty) {
        Right(newPredecessors.toMultimap)
      } else {
        buildDependencies(todo, next, newPredecessors)
      }
    }

    private def parsePkg(
      header: RawHeader
    ): Either[NonEmptyList[BaboonIssue.TyperIssue], Pkg] = {
      for {
        nel <- NonEmptyList
          .from(header.name)
          .toRight(NonEmptyList(TODOTyperIssue()))
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
          .toUniqueMap(_ => NonEmptyList(TODOTyperIssue()))
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
