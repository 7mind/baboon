package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.*
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.BaboonTyper.FullRawDefn
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Scope.*
import izumi.functional.IzEither.*
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.{NEList, NEMap}

class ScopeBuilder() {
  def buildScopes(
    pkg: Pkg,
    members: Seq[RawTLDef],
    meta: RawNodeMeta
  ): Either[NEList[BaboonIssue.TyperIssue], RootScope[FullRawDefn]] = {
    for {
      sub <- members.map(m => buildScope(m.value, m.root)).biSequence
      asMap <- sub
        .map(s => (s.name, s))
        .toUniqueMap(nus => NEList(BaboonIssue.NonUniqueScope(nus, meta)))
    } yield {
      val out = RootScope(pkg, asMap)
      asMap.foreach {
        case (_, s) =>
          s.unsafeGetWriter.setParent(out)
      }

      out
    }

  }

  private def buildScope(
    member: RawDefn,
    isRoot: Boolean
  ): Either[NEList[BaboonIssue.TyperIssue], NestedScope[FullRawDefn]] = {
    def finish(defn: RawDefn,
               asNEMap: NEMap[ScopeName, NestedScope[FullRawDefn]]) = {
      val out = SubScope(
        ScopeName(defn.name.name),
        FullRawDefn(defn, isRoot),
        asNEMap,
      )
      asNEMap.foreach {
        case (_, s) =>
          s.unsafeGetWriter.setParent(out)
      }
      out
    }

    member match {
      case namespace: RawNamespace =>
        for {
          sub <- namespace.defns
            .map(m => buildScope(m.value, isRoot = m.root))
            .biSequence
          asMap <- sub
            .map(s => (s.name, s))
            .toUniqueMap(
              nus => NEList(BaboonIssue.NonUniqueScope(nus, member.meta))
            )
          asNEMap <- NEMap
            .from(asMap)
            .toRight(NEList(BaboonIssue.ScopeCannotBeEmpty(member)))
        } yield {
          finish(namespace, asNEMap)
        }

      case adt: RawAdt =>
        for {
          sub <- adt.members
            .collect { case d: RawAdtMember => d }
            .map(m => buildScope(m.defn, isRoot = false))
            .biSequence
          asMap <- sub
            .map(s => (s.name, s))
            .toUniqueMap(
              nus => NEList(BaboonIssue.NonUniqueScope(nus, member.meta))
            )
          asNEMap <- NEMap
            .from(asMap)
            .toRight(NEList(BaboonIssue.ScopeCannotBeEmpty(member)))
        } yield {
          finish(adt, asNEMap)
        }

      case dto: RawDto =>
        Right(LeafScope(ScopeName(dto.name.name), FullRawDefn(dto, isRoot)))

      case contract: RawContract =>
        Right(
          LeafScope(
            ScopeName(contract.name.name),
            FullRawDefn(contract, isRoot)
          )
        )

      case e: RawEnum =>
        Right(LeafScope(ScopeName(e.name.name), FullRawDefn(e, isRoot)))

      case f: RawForeign =>
        Right(LeafScope(ScopeName(f.name.name), FullRawDefn(f, isRoot)))

    }
  }
}
