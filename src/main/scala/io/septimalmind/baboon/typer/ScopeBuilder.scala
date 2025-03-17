package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.*
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
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
  ): F[NEList[BaboonIssue.TyperIssue], RootScope[ExtendedRawDefn]] = {

    val gen = new UIDGen {}

    for {
      sub <- F.traverseAccumErrors(members)(m => buildScope(m.value, m.root, gen))
      asMap <- F.fromEither {
        sub
          .map(s => (s.name, s))
          .toUniqueMap(nus => NEList(BaboonIssue.NonUniqueScope(nus, meta)))
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
    // parents: util.IdentityHashMap[NestedScope[FullRawDefn], Scope[FullRawDefn]],
    gen: UIDGen,
  ): F[NEList[BaboonIssue.TyperIssue], NestedScope[FullRawDefn]] = {
    def finish(defn: RawDefn, asNEMap: NEMap[ScopeName, NestedScope[FullRawDefn]]) = {
      SubScope(
        gen.next(),
        ScopeName(defn.name.name),
        FullRawDefn(defn, isRoot),
        asNEMap,
      )
    }

    member match {
      case namespace: RawNamespace =>
        for {
          sub <- F.traverseAccumErrors(namespace.defns)(m => buildScope(m.value, isRoot = m.root, gen))
          asMap <- F.fromEither {
            sub
              .map(s => (s.name, s))
              .toUniqueMap(nus => NEList(BaboonIssue.NonUniqueScope(nus, member.meta)))
          }
          asNEMap <- F.fromOption(NEList(BaboonIssue.ScopeCannotBeEmpty(member))) {
            NEMap.from(asMap)
          }
        } yield {
          finish(namespace, asNEMap)
        }

      case adt: RawAdt =>
        for {
          sub <- F.sequenceAccumErrors {
            adt.members.collect { case d: RawAdtMember => d }
              .map(m => buildScope(m.defn, isRoot = false, gen))
          }
          asMap <- F.fromEither {
            sub
              .map(s => (s.name, s))
              .toUniqueMap(nus => NEList(BaboonIssue.NonUniqueScope(nus, member.meta)))
          }
          asNEMap <- F.fromOption(NEList(BaboonIssue.ScopeCannotBeEmpty(member))) {
            NEMap.from(asMap)
          }
        } yield {
          finish(adt, asNEMap)
        }

      case dto: RawDto =>
        F.pure(
          LeafScope(
            gen.next(),
            ScopeName(dto.name.name),
            FullRawDefn(dto, isRoot),
          )
        )

      case contract: RawContract =>
        F.pure(
          LeafScope(
            gen.next(),
            ScopeName(contract.name.name),
            FullRawDefn(contract, isRoot),
          )
        )

      case service: RawService =>
        F.pure(
          LeafScope(
            gen.next(),
            ScopeName(service.name.name),
            FullRawDefn(service, isRoot),
          )
        )

      case e: RawEnum =>
        F.pure(
          LeafScope(gen.next(), ScopeName(e.name.name), FullRawDefn(e, isRoot))
        )

      case f: RawForeign =>
        F.pure(
          LeafScope(gen.next(), ScopeName(f.name.name), FullRawDefn(f, isRoot))
        )

    }
  }

}
