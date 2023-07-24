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
import izumi.fundamentals.graphs.struct.IncidenceMatrix
import izumi.fundamentals.graphs.{DG, GraphMeta}

import scala.annotation.tailrec

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
        roots = indexedDefs.collect {
          case (k, v: DomainMember.User) if v.root =>
            (k, v)
        }
        predecessors <- buildDependencies(
          indexedDefs,
          roots,
          List.empty
        )
        predMatrix = IncidenceMatrix(predecessors)
        graph = DG.fromPred(predMatrix, GraphMeta(indexedDefs.filter {
          case (k, _) => predMatrix.links.contains(k)
        }))
        excludedIds = indexedDefs.keySet.diff(graph.meta.nodes.keySet)
      } yield {
        Domain(id, version, graph, excludedIds)
      }
    }

    private def explode(tpe: TypeRef): Set[TypeId] = tpe match {
      case TypeRef.Scalar(id) => Set(id)
      case TypeRef.Constructor(id, args) =>
        Set(id) ++ args.toList.flatMap(a => explode(a))
    }

    private def depsOf(defn: DomainMember): Set[TypeId] = defn match {
      case _: DomainMember.Builtin => Set.empty
      case u: DomainMember.User =>
        u.defn match {
          case t: Typedef.Dto  => t.fields.flatMap(f => explode(f.tpe)).toSet
          case _: Typedef.Enum => Set.empty
          case t: Typedef.Adt  => t.members.toSet
        }
    }

    @tailrec
    private def buildDependencies(defs: Map[TypeId, DomainMember],
                                  current: Map[TypeId, DomainMember],
                                  predecessors: List[(TypeId, TypeId)],
    ): Either[NonEmptyList[BaboonIssue.TyperIssue], Map[TypeId, Set[TypeId]]] = {
      val nextDepMap = current.toList.flatMap {
        case (id, defn) =>
          depsOf(defn).toList.map(dep => (id, dep))
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
