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
          .toUniqueMap(nus => NEList(TyperIssue.NonUniqueScope(nus, meta): BaboonIssue))
      }
    } yield {
      val out     = RootScope(gen.next(), pkg, asMap)
      val parents = out.identifyParents
      val index   = out.index

      out.map(defn => ExtendedRawDefn(defn, ScopeContext(parents, index)))
    }

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
            .toUniqueMap(nus => NEList(TyperIssue.NonUniqueScope(nus, member.meta): BaboonIssue))
        }
        asNEMap <- F.fromOption(NEList(TyperIssue.ScopeCannotBeEmpty(member): BaboonIssue)) {
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
            adt.members.collect { case d: RawAdtMember => d }
              .map(m => buildScope(m.defn, isRoot = false, gen))
          }
          out <- wrapScope(adt, sub)
        } yield {
          out
        }

      case service: RawService =>
        for {
          inlineDefns <- F.pure(service.defns.map(defn => (defn, defn.sig.collect { case s: RawFuncArg.Struct => s.defn })).filterNot(_._2.isEmpty))
          sub <- F.traverseAccumErrors(inlineDefns) {
            case (func, defns) =>
              for {
                sub <- F.traverseAccumErrors(defns)(m => buildScope(m, isRoot = isRoot, gen))
                out <- wrapScope(RawNamespace(RawTypeName(func.name), Seq.empty, func.meta), sub)
              } yield {
                out
              }
          }
          out <- wrapScope(service, sub)
        } yield {
          out
        }

      case dto: RawDto =>
        F.pure(mkLeaf(dto))

      case contract: RawContract =>
        F.pure(mkLeaf(contract))

      case e: RawEnum =>
        F.pure(mkLeaf(e))

      case f: RawForeign =>
        F.pure(mkLeaf(f))
    }
  }

}
