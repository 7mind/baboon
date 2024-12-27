package io.septimalmind.baboon.typer

import distage.Subcontext
import io.septimalmind.baboon.parser.model.*
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Scope.*
import izumi.functional.IzEither.*
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.graphs.struct.IncidenceMatrix
import izumi.fundamentals.graphs.tools.{Toposort, ToposortLoopBreaker}
import izumi.fundamentals.graphs.{DG, GraphMeta}

import scala.annotation.tailrec

trait BaboonTyper {
  def process(model: RawDomain): Either[NEList[BaboonIssue.TyperIssue], Domain]
}

object BaboonTyper {

  class BaboonTyperImpl(
    enquiries: BaboonEnquiries,
    translator: Subcontext[BaboonTranslator],
    scopeSupport: ScopeSupport,
    types: TypeInfo,
  ) extends BaboonTyper {
    override def process(
      model: RawDomain
    ): Either[NEList[BaboonIssue.TyperIssue], Domain] = {
      for {
        id      <- parsePkg(model.header)
        version <- parseVersion(model.version)
        defs    <- runTyper(id, model.members.defs, model.header.meta)
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
          roots.keySet.map(t => (t, None)).toList,
        )
        predMatrix = IncidenceMatrix(predecessors)
        graph = DG.fromPred(
          predMatrix,
          GraphMeta(indexedDefs.filter {
            case (k, _) => predMatrix.links.contains(k)
          }),
        )
        excludedIds = indexedDefs.keySet.diff(graph.meta.nodes.keySet)
        shallowSchema = graph.meta.nodes.view
          .mapValues(enquiries.shallowId)
          .toMap
        deepSchema <- computeDeepSchema(graph)
        loops       = enquiries.loopsOf(graph.meta.nodes)
        typeMeta = graph.meta.nodes.keySet.map {
          id =>
            (id, TypeMeta(shallowSchema(id), deepSchema(id)))
        }.toMap
        refMeta <- makeRefMeta(graph.meta.nodes)
      } yield {
        Domain(id, version, graph, excludedIds, typeMeta, loops, refMeta)
      }
    }

    private def makeRefMeta(
      defs: Map[TypeId, DomainMember]
    ): Either[NEList[BaboonIssue.TyperIssue], Map[TypeRef, RefMeta]] = {

      for {
        allRefs <- Right(defs.values.flatMap(enquiries.allRefs))
        meta = allRefs.map {
          ref =>
            (ref, RefMeta(enquiries.uebaLen(defs, ref)))
        }.toMap
      } yield {
        meta
      }
    }

    private def deepSchemaRepr(id: TypeId, defs: Map[TypeId, DomainMember], seen: List[TypeId]): List[String] = {
      val self = enquiries.wrap(id)

      val nseen = seen :+ id
      if (seen.contains(id)) {
        List(s"[recursive:$self]")
      } else {
        val maybedef = defs.get(id)
        assert(maybedef.nonEmpty, s"BUG: $id not found")
        maybedef.get match {
          case _: DomainMember.Builtin =>
            List(s"[builtin:$self]")
          case u: DomainMember.User =>
            u.defn match {
              case d: Typedef.Dto =>
                val content = d.fields.flatMap {
                  m =>
                    val exploded = enquiries
                      .explode(m.tpe)
                      .toList
                      .flatMap(id => deepSchemaRepr(id, defs, nseen))
                      .sorted
                    List(s"{", m.name.name) ++ exploded ++ List("}")
                }
                List(s"[dto:$self]") ++ content ++ List(s"/[dto:$self]")
              case d: Typedef.Contract =>
                val content = d.fields.flatMap {
                  m =>
                    val exploded = enquiries
                      .explode(m.tpe)
                      .toList
                      .flatMap(id => deepSchemaRepr(id, defs, nseen))
                      .sorted
                    List(s"{", m.name.name) ++ exploded ++ List("}")
                }
                List(s"[contract:$self]") ++ content ++ List(s"/[contract:$self]")
              case d: Typedef.Adt =>
                val content = d.fields.flatMap {
                  m =>
                    val exploded = enquiries
                      .explode(m.tpe)
                      .toList
                      .flatMap(id => deepSchemaRepr(id, defs, nseen))
                      .sorted
                    List(s"{", m.name.name) ++ exploded ++ List("}")
                }
                val branches = List("{", "branches") ++ d.members.toList
                  .flatMap(id => deepSchemaRepr(id, defs, nseen)) ++
                  List("}", "{", "contracts") ++
                  d.contracts.toList
                    .flatMap(id => deepSchemaRepr(id, defs, nseen)) ++ List("}")

                List(s"[adt:$self]") ++ content ++ branches ++ List(
                  s"/[adt:$self]"
                )

              case d: Typedef.Enum =>
                List(s"[enum:$self]") ++ d.members.map(m => s"${m.name}/${m.const.map(_.toString).getOrElse("NoVal")}") ++ List(s"/[enum:$self]")
              case d: Typedef.Foreign =>
                List(
                  s"[foreign:$self:${d.bindings.map { case (k, v) => s"$k->$v" }.mkString(",")}]"
                )

            }
        }
      }
    }
    private def deepSchemaOf(
      id: TypeId,
      defs: Map[TypeId, DomainMember],
    ): Either[NEList[BaboonIssue.TyperIssue], DeepSchemaId] = {
      for {
        repr <- Right(deepSchemaRepr(id, defs, List.empty))
      } yield {
        DeepSchemaId(s"[${enquiries.wrap(id)};${repr
            .mkString(",")}]")
      }
    }

    private def computeDeepSchema(
      graph: DG[TypeId, DomainMember]
    ): Either[NEList[BaboonIssue.TyperIssue], Map[TypeId, DeepSchemaId]] = {

      for {
        out <- graph.meta.nodes.map {
          case (id, _) =>
            deepSchemaOf(id, graph.meta.nodes).map(s => (id, s))
        }.biSequence
      } yield {
        out.toMap
      }
    }

    @tailrec
    private def buildDependencies(defs: Map[TypeId, DomainMember], current: Map[TypeId, DomainMember], predecessors: List[(TypeId, Option[TypeId])])
      : Either[NEList[BaboonIssue.TyperIssue], Map[TypeId, Set[TypeId]]] = {
      val nextDepMap = current.toList.flatMap {
        case (id, defn) =>
          enquiries.fullDepsOfDefn(defn).toList.map(dep => (id, Some(dep)))
      }
      val nextDeps = nextDepMap.map(_._2).toSet

      val next = defs.collect {
        case (k, v) if nextDeps.contains(Some(k)) =>
          (k, v)
      }

      import izumi.fundamentals.collections.IzCollections.*

      // here we may extract circular dependencies, that is fine
      val newPredecessors = predecessors ++ nextDepMap
      val todo            = defs.removedAll(nextDepMap.map(_._1))

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
      members: Seq[RawTLDef],
      meta: RawNodeMeta,
    ): Either[NEList[BaboonIssue.TyperIssue], List[DomainMember]] = {
      for {
        initial <- Right(
          types.allBuiltins.map(id => DomainMember.Builtin(id))
        )
        builder   = new ScopeBuilder()
        scopes   <- builder.buildScopes(pkg, members, meta)
        flattened = flattenScopes(scopes)
        ordered  <- order(pkg, flattened, meta)

        out <- ordered.biFoldLeft(Map.empty[TypeId, DomainMember]) {
          case (acc, defn) =>
            for {
              next <- translator
                .provide(pkg)
                .provide(defn)
                .provide(acc)
                .produce()
                .use(_.translate())
              mapped = next.map(m => (m.id, m))
              dupes  = acc.keySet.intersect(mapped.map(_._1).toSet)
              _ <- Either.ifThenFail(dupes.nonEmpty)(
                NEList(BaboonIssue.DuplicatedTypes(dupes, meta))
              )
            } yield {
              acc ++ mapped
            }
        }

        indexed <- (initial.map(m => (m.id, m)) ++ out.toSeq)
          .toUniqueMap(e => NEList(BaboonIssue.NonUniqueTypedefs(e, meta)))
      } yield {
        indexed.values.toList
      }
    }

    private def order(
      pkg: Pkg,
      flattened: List[NestedScope[ExtendedRawDefn]],
      meta: RawNodeMeta,
    ): Either[NEList[BaboonIssue.TyperIssue], List[
      NestedScope[ExtendedRawDefn]
    ]] = {
      for {
        depmap <- flattened
          .map(d => deps(pkg, d))
          .biSequence
        asMap <- depmap.toUniqueMap(
          bad => {
            NEList(BaboonIssue.BadInheritance(bad, meta))
          }
        )

        predMatrix = IncidenceMatrix(asMap.view.mapValues(_._1).toMap)
        sorted <- Toposort
          .cycleBreaking(predMatrix, ToposortLoopBreaker.dontBreak)
          .left
          .map(e => NEList(BaboonIssue.CircularInheritance(e, meta)))

      } yield {
        sorted.map(id => asMap(id)._2).toList
      }
    }

    private def deps(pkg: Pkg, defn: NestedScope[ExtendedRawDefn]): Either[NEList[
      BaboonIssue.TyperIssue
    ], (TypeId.User, (Set[TypeId.User], NestedScope[ExtendedRawDefn]))] = {
      for {
        rawDefn <- Right(defn.defn)
        id <- scopeSupport.resolveUserTypeId(
          rawDefn.defn.name,
          defn,
          pkg,
          rawDefn.defn.meta,
        )
        mappedDeps <- enquiries
          .hardDepsOfRawDefn(defn.defn.defn)
          .map(
            v =>
              scopeSupport
                .resolveScopedRef(v, defn, pkg, rawDefn.defn.meta)
          )
          .biSequence
      } yield {
        val adtMemberDependsOnAdt = id.owner match {
          case Owner.Adt(id) => Set(id)
          case _             => Set.empty
        }

        (id, (mappedDeps ++ adtMemberDependsOnAdt, defn))
      }
    }

    private def flattenScopes(
      root: RootScope[ExtendedRawDefn]
    ): List[NestedScope[ExtendedRawDefn]] = {
      def flattenScopes(
        current: NestedScope[ExtendedRawDefn]
      ): List[NestedScope[ExtendedRawDefn]] = {
        current match {
          case s: SubScope[ExtendedRawDefn] =>
            List(s) ++ s.nested.toMap.values
              .flatMap(n => flattenScopes(n))
              .toList
          case l: LeafScope[ExtendedRawDefn] =>
            List(l)
        }
      }

      root.nested.values
        .flatMap(defn => flattenScopes(defn))
        .toList
    }

  }

}
