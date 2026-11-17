package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.*
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.parser.model.issues.TyperIssue
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Scope.{NestedScope, *}
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.{NEList, NEMap}

import java.util.concurrent.atomic.AtomicInteger

class ScopeBuilder[F[+_, +_]: Error2] {
  trait UIDGen {
    val uidGen = new AtomicInteger(0)

    def next(): ScopeUID = {
      ScopeUID(uidGen.getAndIncrement())
    }
  }

  def buildScopes(
    pkg: Pkg,
    members: Seq[RawTLDef],
    meta: RawNodeMeta,
  ): F[NEList[BaboonIssue], RootScope[ExtendedRawDefn]] = {

    val gen = new UIDGen {}

    for {
      sub <- F.traverseAccumErrors(members)(m => buildScope(m.value, m.root, gen))
      asMap <- F.fromEither {
        sub
          .map(s => (s.name, s))
          .toUniqueMap(nus => BaboonIssue.of(TyperIssue.NonUniqueScope(nus, meta)))
      }
    } yield {
      val out     = RootScope(gen.next(), pkg, asMap)
      val context = ScopeContext(out.identifyParents, out.index, declaringAdtsOf(out))

      out.map(defn => ExtendedRawDefn(defn, context))
    }

  }

  /** Maps the scope of every re-emitted ADT branch to the scope of the ADT that declares it. */
  private def declaringAdtsOf(root: RootScope[FullRawDefn]): Map[ScopeUID, ScopeUID] = {
    def adtAt(path: NEList[RawTypeName]): SubScope[FullRawDefn] = {
      val found = path.toList.foldLeft(Option[Scope[FullRawDefn]](root)) {
        case (Some(scope: RootScope[FullRawDefn]), name) => scope.nested.get(ScopeName(name.name))
        case (Some(scope: SubScope[FullRawDefn]), name)  => scope.nested.toMap.get(ScopeName(name.name))
        case _                                           => None
      }
      found match {
        case Some(adt: SubScope[FullRawDefn]) if adt.defn.defn.isInstanceOf[RawAdt] => adt
        case other =>
          throw new IllegalStateException(s"BUG: a re-emitted ADT branch names ${path.toList.map(_.name).mkString(".")} as its declaring ADT, which is $other")
      }
    }

    def reEmittedIn(scope: NestedScope[FullRawDefn]): Iterable[(ScopeUID, ScopeUID)] = {
      scope match {
        case sub: SubScope[FullRawDefn] =>
          val branches = sub.nested.toMap
          val own = sub.defn.defn match {
            case adt: RawAdt =>
              adt.members.collect {
                case RawAdtMemberDto(dto, _, RawAdtBranchOrigin.ReEmitted(declaringAdt))           => (dto.name, declaringAdt)
                case RawAdtMemberContract(contract, _, RawAdtBranchOrigin.ReEmitted(declaringAdt)) => (contract.name, declaringAdt)
              }.map { case (branch, declaringAdt) => (branches(ScopeName(branch.name)).id, adtAt(declaringAdt).id) }
            case _ =>
              Seq.empty
          }
          own ++ branches.values.flatMap(reEmittedIn)
        case _: LeafScope[FullRawDefn] =>
          Seq.empty
      }
    }

    root.nested.values.flatMap(reEmittedIn).toMap
  }

  private def buildScope(
    member: RawDefn,
    isRoot: Boolean,
    gen: UIDGen,
  ): F[NEList[BaboonIssue], NestedScope[FullRawDefn]] = {
    def mkSub(defn: RawDefn, nested: NEMap[ScopeName, NestedScope[FullRawDefn]]) = {
      SubScope(
        gen.next(),
        ScopeName(defn.name.name),
        FullRawDefn(defn, isRoot),
        nested,
      )
    }

    def mkLeaf(defn: RawDefn) = {
      LeafScope(
        gen.next(),
        ScopeName(defn.name.name),
        FullRawDefn(defn, isRoot),
      )
    }

    def wrapScope(defn: RawDefn, nested: Seq[NestedScope[FullRawDefn]]) = {
      for {
        asMap <- F.fromEither {
          nested
            .map(s => (s.name, s))
            .toUniqueMap(nus => BaboonIssue.of(TyperIssue.NonUniqueScope(nus, member.meta)))
        }
        asNEMap <- F.fromOption(BaboonIssue.of(TyperIssue.ScopeCannotBeEmpty(member))) {
          NEMap.from(asMap)
        }
      } yield {
        mkSub(defn, asNEMap)
      }
    }

    member match {
      case namespace: RawNamespace =>
        for {
          sub <- F.traverseAccumErrors(namespace.defns)(m => buildScope(m.value, isRoot = m.root, gen))
          out <- wrapScope(namespace, sub)
        } yield {
          out
        }

      case adt: RawAdt =>
        for {
          sub <- F.sequenceAccumErrors {
            // Collect only members that define a concrete nested type; the M20 inheritance
            // arms (Include, Exclude, Intersect) carry no inline defn — they are desugared
            // in the PR-63 typer pre-pass.
            adt.members.collect { case d: RawAdtMemberDto => d; case d: RawAdtMemberContract => d }
              .map(m => buildScope(m.defn, isRoot = false, gen))
          }
          // When an ADT has *only* inheritance arms (no concrete `data` members yet), treat it
          // as a leaf during the initial scope build. The PR-63 typer-early pass uses this
          // initial tree solely to resolve `+ X` / `- X` / `^ X` refs by *name* — it never
          // descends into the ADT's branches at this stage. After the pre-pass rewrites every
          // inheritance arm into literal `RawAdtMemberDto` entries, `runTyper` rebuilds the
          // scope tree from scratch and the receiving ADT becomes a proper SubScope with its
          // re-emitted branches registered as nested entries.
          out <- if (sub.isEmpty) F.pure(mkLeaf(adt)) else wrapScope(adt, sub)
        } yield {
          out
        }

      case service: RawService =>
        // BAB-G01: desugar absent `data in` to an empty `data in {}` so the
        // synthesised input struct is registered into the scope tree alongside
        // genuinely-parsed inline structs. Without this, BaboonTranslator#convertArg
        // would later fail to resolve the synthetic name.
        val desugaredService = service.copy(defns = service.defns.map {
          f =>
            val hasIn = f.sig.exists {
              case RawFuncArg.Ref(_, marker, _) => marker.toLowerCase == "in"
              case RawFuncArg.Struct(d)         => d.name.name.toLowerCase == "in"
            }
            if (hasIn) f
            else f.copy(sig = RawFuncArg.Struct(RawDto(RawTypeName("in"), Nil, Set.empty, f.meta, Nil)) +: f.sig)
        })
        for {
          inlineDefns <- F.pure(desugaredService.defns.map(defn => (defn, defn.sig.collect { case s: RawFuncArg.Struct => s.defn })).filterNot(_._2.isEmpty))
          sub <- F.traverseAccumErrors(inlineDefns) {
            case (func, defns) =>
              for {
                sub <- F.traverseAccumErrors(defns)(m => buildScope(m, isRoot = isRoot, gen))
                out <- wrapScope(RawNamespace(RawTypeName(func.name), Seq.empty, func.meta), sub)
              } yield {
                out
              }
          }
          out <- if (sub.isEmpty) F.pure(mkLeaf(desugaredService)) else wrapScope(desugaredService, sub)
        } yield {
          out
        }

      case dto: RawDto =>
        F.pure(mkLeaf(dto))

      case identifier: RawIdentifier =>
        F.pure(mkLeaf(identifier))

      case contract: RawContract =>
        F.pure(mkLeaf(contract))

      case e: RawEnum =>
        F.pure(mkLeaf(e))

      case f: RawForeign =>
        F.pure(mkLeaf(f))

      case alias: RawAlias =>
        F.pure(mkLeaf(alias))
    }
  }

}
