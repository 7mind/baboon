package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.*
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.parser.model.issues.TyperIssue
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Scope.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.graphs.struct.{AdjacencyList, AdjacencyPredList}
import izumi.fundamentals.graphs.tools.{Toposort, ToposortLoopBreaker}
import izumi.fundamentals.graphs.{DG, GraphMeta}
import izumi.fundamentals.platform.crypto.IzSha256HashFunction

import scala.annotation.tailrec
import scala.collection.mutable

import izumi.functional.bio._

trait BaboonTyper[F[+_, +_]] {
  def process(model: RawDomain): F[NEList[BaboonIssue], Domain]
}

object BaboonTyper {

  class BaboonTyperImpl[F[+_, +_]: Error2](
    enquiries: BaboonEnquiries,
    translator: BaboonTranslator.Factory[F],
    scopeSupport: ScopeSupport[F],
    componentParsers: ComponentParsers[F],
    types: TypeInfo,
  ) extends BaboonTyper[F] {

    private case class TyperOutput(defs: List[DomainMember], renames: Map[TypeId.User, TypeId.User])

    override def process(
      model: RawDomain
    ): F[NEList[BaboonIssue], Domain] = {
      for {
        id      <- componentParsers.parsePkg(model.header)
        version <- componentParsers.parseVersion(model.version)
        typed   <- runTyper(id, model.members.defs, model.header.meta)
        defs     = typed.defs
        indexedDefs <- F.fromEither {
          defs
            .map(d => (d.id, d))
            .toUniqueMap(e => BaboonIssue.of(TyperIssue.DuplicatedTypedefs(model, e)))
        }
        roots = indexedDefs.collect {
          case (k, v: DomainMember.User) if v.root =>
            (k, v)
        }
        predecessors <- buildDependencies(
          indexedDefs,
          roots,
          roots.keySet.map(t => (t, None)).toList,
        )
        predMatrix = AdjacencyPredList(predecessors)
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
        refMeta     <- makeRefMeta(graph.meta.nodes)
        derivations <- computeDerivations(graph.meta.nodes)
        renames      = typed.renames
      } yield {
        Domain(id, version, graph, excludedIds, typeMeta, loops, refMeta, derivations, roots.keySet, renames)
      }
    }

    def recursiveDepsOfDefn(defs: Map[TypeId, DomainMember], defn: DomainMember, seen: mutable.Set[TypeId]): Unit = {
      val notYetSeen = enquiries.fullDepsOfDefn(defn).diff(seen)
      seen.add(defn.id)
      seen.addAll(notYetSeen)

      notYetSeen.foreach {
        id =>
          recursiveDepsOfDefn(defs, defs(id), seen)
      }
    }

    // TODO: predMatrix from above can be reused here, currently the job is done twice
    private def computeDerivations(
      defs: Map[TypeId, DomainMember]
    ): F[NEList[BaboonIssue], Map[RawMemberMeta, Set[TypeId]]] = {
      import izumi.fundamentals.collections.IzCollections.*

      val out = defs.values.collect { case u: DomainMember.User => u }.flatMap {
        td =>
          td.derivations.map(d => (d, td))
      }.toMultimapView.mapValues {
        roots =>
          val seen = mutable.HashSet.empty[TypeId]
          roots.foreach {
            root =>
              recursiveDepsOfDefn(defs, root, seen)
          }
          seen.toSet
      }.toMap

      F.pure(out)
    }

    private def computeRenames(
      pkg: Pkg,
      flattened: List[NestedScope[ExtendedRawDefn]],
    ): F[NEList[BaboonIssue], Map[TypeId.User, TypeId.User]] = {
      def asPath(scope: Scope[ExtendedRawDefn]): List[Scope[ExtendedRawDefn]] = {
        def go(s: Scope[ExtendedRawDefn]): NEList[Scope[ExtendedRawDefn]] = {
          s match {
            case r: RootScope[ExtendedRawDefn] => NEList(r)
            case n: NestedScope[ExtendedRawDefn] =>
              go(n.defn.parentOf(n)) ++ NEList(n)
          }
        }
        go(scope).toList
      }

      def findScope(needles: NEList[ScopeName], scope: Scope[ExtendedRawDefn]): Option[NestedScope[ExtendedRawDefn]] = {
        val head = needles.head

        val headScope = scope match {
          case s: RootScope[ExtendedRawDefn] =>
            s.nested.get(head)

          case s: LeafScope[ExtendedRawDefn] =>
            Some(s)
              .filter(_.name == head)
              .orElse(findScope(needles, s.defn.parentOf(s)))

          case s: SubScope[ExtendedRawDefn] =>
            Some(s)
              .filter(_.name == head)
              .orElse(s.nested.toMap.get(head))
              .orElse(findScope(needles, s.defn.parentOf(s)))
        }

        NEList.from(needles.tail) match {
          case Some(value) =>
            headScope.flatMap(nested => findScope(value, nested))
          case None =>
            headScope
        }
      }

      def ownerForPrefix(
        prefix: List[RawTypeName],
        scope: NestedScope[ExtendedRawDefn],
      ): F[NEList[BaboonIssue], Owner] = {
        val pathNames = NEList.unsafeFrom(prefix.map(p => ScopeName(p.name)))
        findScope(pathNames, scope) match {
          case Some(found) =>
            found.defn.defn match {
              case adt: RawAdt =>
                scopeSupport
                  .resolveUserTypeId(adt.name, found, pkg, adt.meta)
                  .map(adtId => Owner.Adt(adtId))
              case _ =>
                val ns = asPath(found).collect {
                  case s: SubScope[ExtendedRawDefn]
                      if s.defn.defn.isInstanceOf[RawNamespace] || s.defn.defn.isInstanceOf[RawService] =>
                    TypeName(s.name.name)
                }
                F.pure {
                  if (ns.isEmpty) Owner.Toplevel else Owner.Ns(ns)
                }
            }
          case None =>
            F.pure(Owner.Ns(prefix.map(p => TypeName(p.name))))
        }
      }

      val renameRefs = flattened.flatMap {
        scope =>
          val rawDefn = scope.defn.defn
          val derived = rawDefn match {
            case d: RawDto     => d.derived
            case e: RawEnum    => e.derived
            case a: RawAdt     => a.derived
            case f: RawForeign => f.derived
            case _             => Set.empty[RawMemberMeta]
          }
          derived.collect {
            case RawMemberMeta.Was(ref: RawTypeRef.Simple) =>
              (scope, rawDefn, ref)
          }
      }

      F.traverseAccumErrors(renameRefs) {
        case (scope, rawDefn, ref) =>
          for {
            newId <- scopeSupport.resolveUserTypeId(rawDefn.name, scope, pkg, rawDefn.meta)
            oldOwner <- if (ref.prefix.isEmpty) {
              F.pure(newId.owner)
            } else {
              ownerForPrefix(ref.prefix, scope)
            }
            oldId = TypeId.User(pkg, oldOwner, TypeName(ref.name.name))
          } yield {
            (newId, oldId)
          }
      }.map(_.toMap)
    }

    private def makeRefMeta(
      defs: Map[TypeId, DomainMember]
    ): F[NEList[BaboonIssue], Map[TypeRef, RefMeta]] = {
      for {
        allRefs <- F.pure(defs.values.flatMap(enquiries.allRefs))
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
              case s: Typedef.Service =>
                val content = s.methods.flatMap {
                  m =>
                    val tpes = Set(m.sig) ++ m.err ++ m.out

                    val exploded = tpes
                      .flatMap(enquiries.explode)
                      .toList
                      .flatMap(id => deepSchemaRepr(id, defs, nseen))
                      .sorted
                    List(s"{", m.name.name) ++ exploded ++ List("}")

                }

                List(s"[service:$self]") ++ content ++ List(s"/[service:$self]")

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
                  d.contracts
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
    ): F[NEList[BaboonIssue], DeepSchemaId] = {
      for {
        repr <- F.pure(deepSchemaRepr(id, defs, List.empty))
        fullRepr = s"[${enquiries.wrap(id)};${repr
            .mkString(",")}]"
      } yield {
        DeepSchemaId(IzSha256HashFunction.hash(fullRepr))
      }
    }

    private def computeDeepSchema(
      graph: DG[TypeId, DomainMember]
    ): F[NEList[BaboonIssue], Map[TypeId, DeepSchemaId]] = {

      for {
        out <- F.sequenceAccumErrors(graph.meta.nodes.map {
          case (id, _) =>
            deepSchemaOf(id, graph.meta.nodes).map(s => (id, s))
        })
      } yield {
        out.toMap
      }
    }

    @tailrec
    private def buildDependencies(
      defs: Map[TypeId, DomainMember],
      current: Map[TypeId, DomainMember],
      predecessors: List[(TypeId, Option[TypeId])],
    ): F[NEList[BaboonIssue], Map[TypeId, Set[TypeId]]] = {
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
        F.pure(
          newPredecessors.toMultimapView.view
            .mapValues(_.flatMap(_.toSeq).toSet)
            .toMap
        )
      } else {
        buildDependencies(todo, next, newPredecessors)
      }
    }

    private def runTyper(
      pkg: Pkg,
      members: Seq[RawTLDef],
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], TyperOutput] = {
      for {
        initial <- F.pure(
          types.allBuiltins.map(id => DomainMember.Builtin(id))
        )
        builder   = new ScopeBuilder[F]()
        scopes   <- builder.buildScopes(pkg, members, meta)
        flattened = flattenScopes(scopes)
        renames  <- computeRenames(pkg, flattened)
        ordered  <- order(pkg, flattened, meta)

        out <- F.foldLeft(ordered)(Map.empty[TypeId, DomainMember]) {
          case (acc, defn) =>
            for {
              next  <- translator(pkg, defn, acc).translate()
              mapped = next.map(m => (m.id, m))
              dupes  = acc.keySet.intersect(mapped.map(_._1).toSet)
              _ <- F.when(dupes.nonEmpty)(
                F.fail(BaboonIssue.of(TyperIssue.DuplicatedTypes(dupes, meta)))
              )
            } yield {
              acc ++ mapped
            }
        }

        indexed <- F.fromEither {
          (initial.map(m => (m.id, m)) ++ out.toSeq)
            .toUniqueMap(e => BaboonIssue.of(TyperIssue.NonUniqueTypedefs(e, meta)))
        }
      } yield {
        TyperOutput(indexed.values.toList, renames)
      }
    }

    private def order(
      pkg: Pkg,
      flattened: List[NestedScope[ExtendedRawDefn]],
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], List[NestedScope[ExtendedRawDefn]]] = {
      for {
        depmap <- F.traverseAccumErrors(flattened)(d => deps(pkg, d))
        asMap <- F.fromEither {
          depmap.toUniqueMap(
            bad => {
              BaboonIssue.of(TyperIssue.BadInheritance(bad, meta))
            }
          )
        }

        predMatrix = AdjacencyList(asMap.view.mapValues(_._1).toMap)
        sorted <- F.fromEither {
          Toposort.cycleBreaking(predMatrix, ToposortLoopBreaker.dontBreak)
        }.leftMap(e => BaboonIssue.of(TyperIssue.CircularInheritance(e, meta)))

      } yield {
        sorted.map(id => asMap(id)._2).toList
      }
    }

    private def deps(
      pkg: Pkg,
      defn: NestedScope[ExtendedRawDefn],
    ): F[NEList[BaboonIssue], (TypeId.User, (Set[TypeId.User], NestedScope[ExtendedRawDefn]))] = {
      val rawDefn = defn.defn
      for {
        id <- scopeSupport.resolveUserTypeId(
          rawDefn.defn.name,
          defn,
          pkg,
          rawDefn.defn.meta,
        )
        mappedDeps <- F.traverseAccumErrors(
          enquiries.hardDepsOfRawDefn(defn.defn.defn)
        ) {
          v =>
            scopeSupport.resolveScopedRef(v, defn, pkg, rawDefn.defn.meta)
        }
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
