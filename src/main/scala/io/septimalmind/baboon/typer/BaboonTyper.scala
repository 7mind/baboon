package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.*
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Scope.*
import izumi.distage.LocalContext
import izumi.functional.IzEither.*
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.{NEList, NEMap}
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
  case class FullRawDefn(defn: RawDefn, gcRoot: Boolean)

  case class ScopedDefn(thisScope: NestedScope[FullRawDefn],
                        path: NEList[Scope[FullRawDefn]])

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
      import izumi.fundamentals.platform.strings.IzString.*
      for {
        initial <- Right(
          TypeId.Builtins.all.map(id => DomainMember.Builtin(id))
        )

        scopes <- buildScopes(pkg, members)
        flattened = flattenScopes(scopes)

        // we don't support inheritance, so order doesn't matter here
        out <- flattened
          .map(
            defn =>
              translator
                .provide(pkg)
                .provide(defn.path)
                .produce()
                .use(_.translate(defn))
          )
          .biFlatten

        indexed <- (initial ++ out)
          .map(m => (m.id, m))
          .toUniqueMap(e => NEList(BaboonIssue.NonUniqueTypedefs(e)))
      } yield {
        indexed.values.toList
      }
    }

    private def flattenScopes(
      root: RootScope[FullRawDefn]
    ): List[ScopedDefn] = {
      root.nested.values
        .flatMap(defn => flattenScopes(NEList(root), defn))
        .toList
    }

    private def flattenScopes(
      path: NEList[Scope[FullRawDefn]],
      current: NestedScope[FullRawDefn]
    ): List[ScopedDefn] = {

      current match {
        case s: SubScope[FullRawDefn] =>
          List(ScopedDefn(s, path)) ++ s.nested.toMap.values
            .flatMap(n => flattenScopes(path :+ current, n))
            .toList
        case l: LeafScope[FullRawDefn] =>
          List(ScopedDefn(l, path))
      }
    }

    private def buildScopes(
      pkg: Pkg,
      members: Seq[RawTLDef]
    ): Either[NEList[BaboonIssue.TyperIssue], RootScope[FullRawDefn]] = {
      for {
        sub <- members.map(m => buildScope(m.value, m.root)).biSequence
        asMap <- sub
          .map(s => (s.name, s))
          .toUniqueMap(nus => NEList(BaboonIssue.NonUniqueScope(nus)))
      } yield {
        RootScope(pkg, asMap)
      }

    }

    private def buildScope(
      member: RawDefn,
      isRoot: Boolean
    ): Either[NEList[BaboonIssue.TyperIssue], NestedScope[FullRawDefn]] = {
      member match {
        case dto: RawDto =>
          Right(LeafScope(ScopeName(dto.name.name), FullRawDefn(dto, isRoot)))
        case e: RawEnum =>
          Right(LeafScope(ScopeName(e.name.name), FullRawDefn(e, isRoot)))
        case adt: RawAdt =>
          for {
            sub <- adt.members
              .map(m => buildScope(m.dto, isRoot = false))
              .biSequence
            asMap <- sub
              .map(s => (s.name, s))
              .toUniqueMap(nus => NEList(BaboonIssue.NonUniqueScope(nus)))
            asNEMap <- NEMap
              .from(asMap)
              .toRight(NEList(BaboonIssue.ScopeCannotBeEmpty(member)))
          } yield {
            SubScope(
              ScopeName(adt.name.name),
              FullRawDefn(adt, isRoot),
              asNEMap
            )
          }
      }
    }

  }

}
