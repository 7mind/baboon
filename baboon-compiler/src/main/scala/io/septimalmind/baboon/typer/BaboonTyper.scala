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
    rootExtractor: RootExtractor,
    adtInheritanceExpander: AdtInheritanceExpander[F],
    templateRegistryBuilder: TemplateRegistryBuilder[F],
    templateInstantiator: TemplateInstantiator[F],
  ) extends BaboonTyper[F] {

    private case class TyperOutput(
      defs: List[DomainMember],
      renames: Map[TypeId.User, TypeId.User],
      aliases: List[AliasInfo],
      templateRegistry: TemplateRegistry,
      // T40 (auto-extracted-contracts): out-of-band `mirror`-variant reachability edges
      // host-instantiation→B, consulted by `buildDependencies` so a `mirror` B is GC-reachable iff
      // its host instantiation is, WITHOUT surfacing in any `Typedef.contracts` list.
      mirrorExtractionEdges: List[TemplateInstantiator.MirrorExtractionEdge],
    )

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
        directRoots = rootExtractor.roots(indexedDefs)
        // Root aliases contribute their resolved targets to the root set
        aliasRootIds: Set[TypeId] = typed.aliases
          .filter(_.root).flatMap {
            a =>
              enquiries.explode(a.resolvedTarget)
          }.toSet
        aliasRoots = indexedDefs.filter { case (k, _) => aliasRootIds.contains(k) }
        roots      = directRoots ++ aliasRoots
        // T40 (auto-extracted-contracts): `mirror`-variant reachability edges host-instantiation→B,
        // keyed by host id. These are NOT model-level relationships (no `Typedef.contracts` entry —
        // load-bearing for all 9 backends); they feed the @root GC only so a `mirror` B survives iff
        // its host instantiation is reachable. Filtered to ids actually present in `indexedDefs`.
        mirrorEdgeMap: Map[TypeId, Set[TypeId]] = typed.mirrorExtractionEdges
          .filter(e => indexedDefs.contains(e.host) && indexedDefs.contains(e.contract))
          .groupBy(_.host: TypeId)
          .view.mapValues(_.map(_.contract: TypeId).toSet).toMap
        predecessors <- buildDependencies(
          indexedDefs,
          roots,
          roots.keySet.map(t => (t, None)).toList,
          mirrorEdgeMap,
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
        aliases      = typed.aliases
      } yield {
        Domain(
          id,
          version,
          graph,
          excludedIds,
          typeMeta,
          loops,
          refMeta,
          derivations,
          roots.keySet,
          renames,
          model.pragmas.map(p => (p.key, p.value)).toMap,
          aliases,
          templateRegistry = typed.templateRegistry,
        )
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
                  case s: SubScope[ExtendedRawDefn] if s.defn.defn.isInstanceOf[RawNamespace] || s.defn.defn.isInstanceOf[RawService] =>
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
            case d: RawDto        => d.derived
            case d: RawIdentifier => d.derived
            case e: RawEnum       => e.derived
            case a: RawAdt        => a.derived
            case f: RawForeign    => f.derived
            case _                => Set.empty[RawMemberMeta]
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
            oldOwner <-
              if (ref.prefix.isEmpty) {
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
                  .map(id => deepSchemaRepr(id, defs, nseen))
                  .sortBy(_.mkString(" "))
                  .flatten ++
                  List("}", "{", "contracts") ++
                  d.contracts
                    .map(id => deepSchemaRepr(id, defs, nseen))
                    .sortBy(_.mkString(" "))
                    .flatten ++ List("}")

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
      mirrorEdges: Map[TypeId, Set[TypeId]],
    ): F[NEList[BaboonIssue], Map[TypeId, Set[TypeId]]] = {
      val nextDepMap = current.toList.flatMap {
        case (id, defn) =>
          // T40: union the structural dependencies with any `mirror`-variant extraction edges keyed
          // by this id, so a `mirror` B is pulled into the reachable closure when its host is, while
          // never appearing in `Typedef.contracts`.
          val structuralDeps = enquiries.fullDepsOfDefn(defn)
          val mirrorDeps     = mirrorEdges.getOrElse(id, Set.empty)
          (structuralDeps ++ mirrorDeps).toList.map(dep => (id, Some(dep)))
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
        buildDependencies(todo, next, newPredecessors, mirrorEdges)
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
        // PR-29.4: extract templates from the raw member list BEFORE any other pass sees them.
        // Templates are registered in the TemplateRegistry and removed from the member list so
        // that no `DomainMember` is ever produced for a template declaration.
        registryResult                        <- templateRegistryBuilder.build(pkg, members)
        (nonTemplateMembers, templateRegistry) = registryResult

        // PR-33.2-D05: validate that no DTO/Contract/Identifier/ADT-branch body references a
        // registered template by bare name (without `[…]`) in `+`/`-`/`^` arm position. The
        // toposort `order` (below) reports such bare refs as hard deps, which then resolve
        // through the regular scope tree and produce a confusing `NameNotFound` instead of the
        // precise diagnostic. Catch the error here, before order() runs.
        _ <- templateInstantiator.validateNoBareTemplateRefs(pkg, nonTemplateMembers, templateRegistry)

        builder = new ScopeBuilder[F]()
        // Build an initial scope tree over the as-parsed (PR-62) raw AST so we can resolve
        // `+ X` / `- X` / `^ X` refs in ADT bodies. The PR-63 typer-early pass uses this
        // initial tree to rewrite each `RawAdt`'s member list, replacing inheritance arms with
        // literal `RawAdtMemberDto` entries pulled from the referenced ADTs.
        initialScopes   <- builder.buildScopes(pkg, nonTemplateMembers, meta)
        initialFlattened = flattenScopes(initialScopes)
        initialOrdered  <- order(pkg, initialFlattened, meta)
        expandedMembers <- adtInheritanceExpander.expand(pkg, nonTemplateMembers, initialOrdered, meta)

        // PR-29.5: instantiate template aliases. Every `RawTLDef.Alias` whose RHS is a
        // `RawTypeRef.Constructor` over a registered template is replaced by the corresponding
        // concrete `RawTLDef.{DTO|ADT|Contract|Service}` keyed by the alias's name (locked
        // decision #4). Aliases NOT pointing at a template pass through unchanged.
        instantiateResult <- templateInstantiator.instantiate(pkg, expandedMembers, templateRegistry)
        (instantiatedMembers, mirrorExtractionEdges) = instantiateResult

        // T38 (auto-extracted-contracts): synthesize one sibling `RawContract` per `has` clause on a
        // TEMPLATED data/id/adt host, BETWEEN `instantiate` and the second `buildScopes`, so the
        // synthesized contracts are first-class everywhere downstream (scope tree, `is B` refs,
        // toposort hard-deps, LSP, evolution) with zero special cases. Each contract's body is the
        // host's param-free resolved field set (sentinel-substitution drops template-dependent
        // members). Returns the augmented member list plus the synthesized contracts' coordinates
        // for the post-translation emptiness check.
        synthResult <- synthesizeExtractions(pkg, instantiatedMembers, templateRegistry)
        (membersWithExtractions, synthesizedExtractions) = synthResult

        // Re-build the scope tree over the rewritten defns so that re-emitted branches are
        // registered as nested scopes under the receiving ADT (otherwise
        // `convertAdt` → `scopeSupport.resolveUserTypeId` would fail to find the synthesized
        // branch DTOs). After this point the `RawAdtMember.{Include, Exclude, Intersect}`
        // arms have been desugared and the standard pipeline runs unchanged.
        scopes   <- builder.buildScopes(pkg, membersWithExtractions, meta)
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

        // T38: empty-extraction check (post-translation). Raw-level emptiness is insufficient
        // (e.g. `+ EmptyDto` resolves empty), so verify each synthesized contract's RESOLVED content
        // is non-empty using the translated `out` map.
        //
        // T40: an extraction whose resolved content is purely a contract edge (`is Base`, with no
        // direct fields) is NON-empty — this is the canonical ADT-host shape, where the ADT's
        // extraction input is its ADT-level `is` refs (`raw.contracts`) rather than direct fields.
        // B then carries `Base` as a contract edge (empty `fields`, non-empty `contracts`) and the
        // instantiated ADT `is B` absorbs Base transitively into its branches. Only a B with BOTH
        // empty `fields` AND empty `contracts` (e.g. a data host where every member depends on the
        // template parameter) is genuinely empty.
        _ <- F.traverseAccumErrors_(synthesizedExtractions) {
          ext =>
            out.get(ext.id) match {
              case Some(DomainMember.User(_, c: Typedef.Contract, _, _)) if c.fields.isEmpty && c.contracts.isEmpty =>
                F.fail(BaboonIssue.of(TyperIssue.ExtractionEmpty(ext.name, ext.hostName, ext.meta)))
              case _ =>
                F.unit
            }
        }

        indexed <- F.fromEither {
          (initial.map(m => (m.id, m)) ++ out.toSeq)
            .toUniqueMap(e => BaboonIssue.of(TyperIssue.NonUniqueTypedefs(e, meta)))
        }
        aliases <- F.traverseAccumErrors(flattened.filter(_.defn.defn.isInstanceOf[RawAlias])) {
          scope =>
            translator(pkg, scope, out).resolveAliasInfo().map(_.get)
        }
      } yield {
        TyperOutput(indexed.values.toList, renames, aliases.toList, templateRegistry, mirrorExtractionEdges)
      }
    }

    // ─── T38: auto-extracted-contracts synthesis pass ─────────────────────────

    /** Coordinates of one synthesized extraction contract, used for the post-translation
      * emptiness check. `id` is the contract's resolved `TypeId.User`; `name`/`hostName`/`meta`
      * feed the `ExtractionEmpty` diagnostic.
      */
    private case class SynthesizedExtraction(id: TypeId.User, name: String, hostName: String, meta: RawNodeMeta)

    /** One pending extraction collected from a templated host, before contract synthesis. */
    private case class PendingExtraction(
      owner: Owner,
      hostName: String,
      typeParams: List[RawTypeName],
      structuralMembers: Seq[RawDtoMember],
      clause: RawDtoMember.ExtractionDef,
    )

    /** Synthesize one sibling `RawContract` per `has` clause carried by a TEMPLATED data/id/adt
      * host. See the `runTyper` call-site comment for placement rationale (Q7/Q9). Returns the
      * augmented member list (originals + injected contracts) and the synthesized contracts'
      * coordinates for the post-translation emptiness check.
      *
      * No-op guarantee (Q12): a model with zero `has` clauses anywhere returns `members` UNCHANGED
      * and an empty coordinate list.
      */
    private def synthesizeExtractions(
      pkg: Pkg,
      members: Seq[RawTLDef],
      registry: TemplateRegistry,
    ): F[NEList[BaboonIssue], (Seq[RawTLDef], List[SynthesizedExtraction])] = {
      // Fast no-op: no extraction clause anywhere (neither in the registry bodies nor in the
      // non-template member list). Return members byte-identical.
      val registryHasExtractions = registry.templates.values.exists(b => templateBodyHasExtraction(b))
      val membersHaveExtractions = members.exists(rawTLDefHasExtraction)
      if (!registryHasExtractions && !membersHaveExtractions) {
        F.pure((members, List.empty))
      } else {
        for {
          // (1) Host-gating: ANY `has` clause on a non-template member (or inside an ADT branch) is
          // host-invalid — the feature extracts NON-template fields and presupposes template params.
          _ <- F.traverseAccumErrors_(members)(m => gateNonTemplateMember(m))
          // (2) Host-gating + collection over the registry: templated data/id/adt are valid hosts;
          // a templated contract or any extraction inside a templated ADT branch is invalid.
          pendings <- collectPendingExtractions(registry)
          // (3) Collision check, then synthesize + inject one RawContract per pending extraction.
          existingCoords = collectTypeCoords(members) ++ registry.templates.keys.map { case (_, o, n) => (o, n.name) }.toSet
          result <- buildAndInject(pkg, members, pendings, existingCoords, registry)
        } yield result
      }
    }

    private def templateBodyHasExtraction(body: TemplateBody): Boolean = body.rawDefn match {
      case RawTemplateDefn.Dto(raw)        => raw.members.exists(_.isInstanceOf[RawDtoMember.ExtractionDef])
      case RawTemplateDefn.Identifier(raw) => raw.members.exists(_.isInstanceOf[RawDtoMember.ExtractionDef])
      case RawTemplateDefn.Adt(raw) =>
        raw.extractions.nonEmpty || raw.members.exists(adtBranchHasExtraction)
      case RawTemplateDefn.Contract(raw) => raw.members.exists(_.isInstanceOf[RawDtoMember.ExtractionDef])
      case RawTemplateDefn.Service(_)    => false
    }

    private def adtBranchHasExtraction(m: RawAdtMember): Boolean = m match {
      case b: RawAdtMemberDto      => b.dto.members.exists(_.isInstanceOf[RawDtoMember.ExtractionDef])
      case b: RawAdtMemberContract => b.contract.members.exists(_.isInstanceOf[RawDtoMember.ExtractionDef])
      case _                       => false
    }

    /** True iff a non-template `RawTLDef` carries a `has` clause anywhere reachable (its own body,
      * an ADT's extractions / branch bodies, or a nested namespace's children). Used by the no-op
      * fast-path scan.
      */
    private def rawTLDefHasExtraction(tldef: RawTLDef): Boolean = tldef match {
      case RawTLDef.DTO(_, raw)        => raw.members.exists(_.isInstanceOf[RawDtoMember.ExtractionDef])
      case RawTLDef.Identifier(_, raw) => raw.members.exists(_.isInstanceOf[RawDtoMember.ExtractionDef])
      case RawTLDef.Contract(_, raw)   => raw.members.exists(_.isInstanceOf[RawDtoMember.ExtractionDef])
      case RawTLDef.ADT(_, raw)        => raw.extractions.nonEmpty || raw.members.exists(adtBranchHasExtraction)
      case RawTLDef.Namespace(ns)      => ns.defns.exists(rawTLDefHasExtraction)
      case _                           => false
    }

    /** Host-gating for non-template members: every `has` clause reachable here is invalid, because
      * the synthesis input must be a TEMPLATED data/id/adt. Emits `ExtractionHostInvalid` per stray
      * clause, with a host-description appropriate to the carrier.
      */
    private def gateNonTemplateMember(tldef: RawTLDef): F[NEList[BaboonIssue], Unit] = {
      tldef match {
        case RawTLDef.DTO(_, raw) =>
          failExtractionsIn(raw.members, raw.name.name, "a non-templated data")
        case RawTLDef.Identifier(_, raw) =>
          failExtractionsIn(raw.members, raw.name.name, "a non-templated id")
        case RawTLDef.Contract(_, raw) =>
          failExtractionsIn(raw.members, raw.name.name, "a contract body")
        case RawTLDef.ADT(_, raw) =>
          val adtLevel: F[NEList[BaboonIssue], Unit] = NEList.from(raw.extractions.toList) match {
            case Some(extrs) => F.fail(extrs.map(e => BaboonIssue.Typer(TyperIssue.ExtractionHostInvalid(raw.name.name, "a non-templated adt", e.meta))))
            case None        => F.unit
          }
          for {
            _ <- adtLevel
            _ <- F.traverseAccumErrors_(raw.members)(gateAdtBranch)
          } yield ()
        case RawTLDef.Namespace(ns) =>
          F.traverseAccumErrors_(ns.defns)(gateNonTemplateMember)
        case _ =>
          F.unit
      }
    }

    private def gateAdtBranch(m: RawAdtMember): F[NEList[BaboonIssue], Unit] = m match {
      case b: RawAdtMemberDto      => failExtractionsIn(b.dto.members, b.dto.name.name, "an ADT branch")
      case b: RawAdtMemberContract => failExtractionsIn(b.contract.members, b.contract.name.name, "an ADT branch")
      case _                       => F.unit
    }

    private def failExtractionsIn(members: Seq[RawDtoMember], hostName: String, descr: String): F[NEList[BaboonIssue], Unit] = {
      val extrs = members.collect { case e: RawDtoMember.ExtractionDef => e }
      NEList.from(extrs.toList) match {
        case Some(es) => F.fail(es.map(e => BaboonIssue.Typer(TyperIssue.ExtractionHostInvalid(hostName, descr, e.meta))))
        case None     => F.unit
      }
    }

    /** Collect pending extractions from the registry. Templated data/id are hosts whose extraction
      * input is their member list; a templated ADT's extraction input is its ADT-level `is` refs
      * (`contracts`), rendered as `ContractRef` members. Templated contracts and extractions inside
      * a templated ADT branch are host-invalid.
      */
    private def collectPendingExtractions(
      registry: TemplateRegistry,
    ): F[NEList[BaboonIssue], Seq[PendingExtraction]] = {
      F.flatTraverseAccumErrors(registry.templates.toList) {
        case ((_, owner, _), body) =>
          body.rawDefn match {
            case RawTemplateDefn.Dto(raw) =>
              val clauses = raw.members.collect { case e: RawDtoMember.ExtractionDef => e }
              F.pure(clauses.toList.map(c => PendingExtraction(owner, raw.name.name, body.typeParams, raw.members, c)))
            case RawTemplateDefn.Identifier(raw) =>
              val clauses = raw.members.collect { case e: RawDtoMember.ExtractionDef => e }
              F.pure(clauses.toList.map(c => PendingExtraction(owner, raw.name.name, body.typeParams, raw.members, c)))
            case RawTemplateDefn.Adt(raw) =>
              // ADT-level extraction input is the ADT's `is` contract refs; branches are not hosts.
              val branchExtrs = raw.members.flatMap {
                case b: RawAdtMemberDto      => b.dto.members.collect { case e: RawDtoMember.ExtractionDef => e }
                case b: RawAdtMemberContract => b.contract.members.collect { case e: RawDtoMember.ExtractionDef => e }
                case _                       => Seq.empty
              }
              NEList.from(branchExtrs.toList) match {
                case Some(es) =>
                  F.fail(es.map(e => BaboonIssue.Typer(TyperIssue.ExtractionHostInvalid(raw.name.name, "an ADT branch", e.meta))))
                case None =>
                  val input = raw.contracts.map(c => c: RawDtoMember)
                  F.pure(raw.extractions.toList.map(c => PendingExtraction(owner, raw.name.name, body.typeParams, input, c)))
              }
            case RawTemplateDefn.Contract(raw) =>
              val clauses = raw.members.collect { case e: RawDtoMember.ExtractionDef => e }
              NEList.from(clauses.toList) match {
                case Some(es) => F.fail(es.map(e => BaboonIssue.Typer(TyperIssue.ExtractionHostInvalid(raw.name.name, "a contract body", e.meta))))
                case None     => F.pure(List.empty)
              }
            case RawTemplateDefn.Service(_) =>
              F.pure(List.empty)
          }
      }
    }

    /** Collect `(Owner, typeName)` coordinates for every user-declared type in the member tree,
      * for the extraction-name-collision check.
      */
    private def collectTypeCoords(members: Seq[RawTLDef]): Set[(Owner, String)] = {
      def go(tldef: RawTLDef, owner: Owner): Set[(Owner, String)] = tldef match {
        case RawTLDef.Namespace(ns) =>
          val nextOwner = owner match {
            case Owner.Toplevel => Owner.Ns(List(TypeName(ns.name.name)))
            case Owner.Ns(path) => Owner.Ns(path.toList :+ TypeName(ns.name.name))
            case adt: Owner.Adt => throw new IllegalStateException(s"Namespace inside an ADT scope is structurally impossible; got $adt")
          }
          ns.defns.flatMap(d => go(d, nextOwner)).toSet
        case other =>
          Set((owner, other.value.name.name))
      }
      members.flatMap(m => go(m, Owner.Toplevel)).toSet
    }

    /** Build a `RawContract` per pending extraction, check for name collisions, and inject each
      * contract at its host owner's level in the member tree. Returns the augmented member list and
      * the synthesized coordinates.
      */
    private def buildAndInject(
      pkg: Pkg,
      members: Seq[RawTLDef],
      pendings: Seq[PendingExtraction],
      existingCoords: Set[(Owner, String)],
      registry: TemplateRegistry,
    ): F[NEList[BaboonIssue], (Seq[RawTLDef], List[SynthesizedExtraction])] = {
      val synthCoords = scala.collection.mutable.Set.empty[(Owner, String)]
      F.flatTraverseAccumErrors(pendings) {
        p =>
          val coord = (p.owner, p.clause.name.name)
          val collides = existingCoords.contains(coord) || synthCoords.contains(coord)
          if (collides) {
            F.fail(BaboonIssue.of(TyperIssue.ExtractionNameCollision(p.clause.name.name, p.hostName, p.clause.meta)))
          } else {
            synthCoords.add(coord)
            templateInstantiator
              .resolveExtractionBody(pkg, p.owner, p.hostName, p.typeParams, p.structuralMembers, registry, p.clause.meta)
              .map {
                resolvedMembers =>
                  val contract = RawContract(
                    name       = p.clause.name,
                    members    = resolvedMembers,
                    meta       = p.clause.meta,
                    typeParams = Nil,
                  )
                  val tldef = RawTLDef.Contract(root = false, contract)
                  val id    = TypeId.User(pkg, p.owner, TypeName(p.clause.name.name))
                  List((p.owner, tldef, SynthesizedExtraction(id, p.clause.name.name, p.hostName, p.clause.meta)))
              }
          }
      }.map {
        built =>
          val byOwner = built.groupBy(_._1).view.mapValues(_.map(_._2).toList).toMap
          val coords  = built.map(_._3).toList
          val injected = injectByOwner(members, byOwner, Owner.Toplevel)
          (injected, coords)
      }
    }

    /** Inject synthesized contract TLDefs at their host-owner level. Top-level contracts append to
      * the root member list; namespaced contracts descend into (or create) the matching namespace.
      * Namespaces may be missing from `members` when the template registry builder dropped them
      * (all children were templates); in that case we create the needed namespace containers so that
      * multi-level namespace targets (e.g. `Owner.Ns(["some","ns"])` when `some` was dropped) are
      * still reached.
      */
    private def injectByOwner(
      members: Seq[RawTLDef],
      byOwner: Map[Owner, List[RawTLDef]],
      currentOwner: Owner,
    ): Seq[RawTLDef] = {
      val here = byOwner.getOrElse(currentOwner, List.empty)
      // Descend into existing namespaces.
      val rewritten = members.map {
        case nsTL @ RawTLDef.Namespace(ns) =>
          val nextOwner = currentOwner match {
            case Owner.Toplevel => Owner.Ns(List(TypeName(ns.name.name)))
            case Owner.Ns(path) => Owner.Ns(path.toList :+ TypeName(ns.name.name))
            case adt: Owner.Adt => throw new IllegalStateException(s"Namespace inside an ADT scope is structurally impossible; got $adt")
          }
          nsTL.copy(value = ns.copy(defns = injectByOwner(ns.defns, byOwner, nextOwner)))
        case other => other
      }
      // Owners that target a namespace not present among the current children must be created.
      // This includes DEEP descendants: if `some.ns` needs a contract but `some` was dropped from
      // `members` (all its children were templates, removed by TemplateRegistryBuilder), we must
      // create `some` as a namespace container and recurse into it to handle `some.ns`.
      val existingNsNames = members.collect { case RawTLDef.Namespace(ns) => ns.name.name }.toSet
      val currentPrefix: List[TypeName] = currentOwner match {
        case Owner.Toplevel => List.empty
        case Owner.Ns(path) => path.toList
        case _: Owner.Adt   => List.empty // ADT-owned scopes cannot contain synthesized contracts.
      }
      // Collect the next-segment names for ALL byOwner owners that are descendants of currentOwner.
      val missingChildNames = byOwner.keys.collect {
        case Owner.Ns(path)
            if path.toList.startsWith(currentPrefix) && path.size > currentPrefix.size
              && !existingNsNames.contains(path.toList.drop(currentPrefix.size).head.name) =>
          path.toList.drop(currentPrefix.size).head.name
      }.toSet
      val createdNamespaces: Seq[RawTLDef] = missingChildNames.toList.map {
        childName =>
          val childOwner = currentOwner match {
            case Owner.Toplevel => Owner.Ns(List(TypeName(childName)))
            case Owner.Ns(path) => Owner.Ns(path.toList :+ TypeName(childName))
            case adt: Owner.Adt => throw new IllegalStateException(s"Namespace inside an ADT scope is structurally impossible; got $adt")
          }
          RawTLDef.Namespace(RawNamespace(RawTypeName(childName), injectByOwner(Seq.empty, byOwner, childOwner), emptyMeta))
      }
      rewritten ++ here ++ createdNamespaces
    }

    private val emptyMeta: RawNodeMeta = RawNodeMeta(InputPointer.Undefined)

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
