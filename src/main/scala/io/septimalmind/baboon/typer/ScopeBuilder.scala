package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.*
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Scope.{NestedScope, *}
import izumi.functional.IzEither.*
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.{NEList, NEMap}

import java.util.concurrent.atomic.AtomicInteger

class ScopeBuilder() {
  trait UIDGen {
    val uidGen = new AtomicInteger(0)

    def next(): ScopeUID = {
      ScopeUID(uidGen.getAndIncrement())
    }
  }

  def buildScopes(
    pkg: Pkg,
    members: Seq[RawTLDef],
    meta: RawNodeMeta
  ): Either[NEList[BaboonIssue.TyperIssue], ScopeTree] = {

    val gen = new UIDGen {}
//    val parents =
//      new util.IdentityHashMap[NestedScope[FullRawDefn], Scope[FullRawDefn]]()

    for {
      sub <- members.map(m => buildScope(m.value, m.root, gen)).biSequence
      asMap <- sub
        .map(s => (s.name, s))
        .toUniqueMap(nus => NEList(BaboonIssue.NonUniqueScope(nus, meta)))
    } yield {
      val out = RootScope(gen.next(), pkg, asMap)

//      asMap.foreach {
//        case (_, s) =>
//          assert(!parents.containsKey(s))
//          parents.put(s, out)
//      }
//
//      import scala.jdk.CollectionConverters.*
//      val tree = parents.asScala
//      asMap.foreach {
//        case (_, s) =>
//          assert(tree(s) == out)
//      }



      ScopeTree(out, out.identifyParents, out.index)
    }

  }

  private def buildScope(member: RawDefn,
                         isRoot: Boolean,
                         //parents: util.IdentityHashMap[NestedScope[FullRawDefn], Scope[FullRawDefn]],
                         gen: UIDGen,
  ): Either[NEList[BaboonIssue.TyperIssue], NestedScope[FullRawDefn]] = {
    def finish(defn: RawDefn,
               asNEMap: NEMap[ScopeName, NestedScope[FullRawDefn]]) = {
      val out = SubScope(
        gen.next(),
        ScopeName(defn.name.name),
        FullRawDefn(defn, isRoot),
        asNEMap,
      )
//      asNEMap.foreach {
//        case (_, s) =>
//          assert(!parents.containsKey(s))
//          val prev = parents.put(s, out)
//          assert(prev == null)
//      }
      out
    }

    member match {
      case namespace: RawNamespace =>
        for {
          sub <- namespace.defns
            .map(m => buildScope(m.value, isRoot = m.root, gen))
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
            .map(m => buildScope(m.defn, isRoot = false, gen))
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
        Right(
          LeafScope(
            gen.next(),
            ScopeName(dto.name.name),
            FullRawDefn(dto, isRoot)
          )
        )

      case contract: RawContract =>
        Right(
          LeafScope(
            gen.next(),
            ScopeName(contract.name.name),
            FullRawDefn(contract, isRoot)
          )
        )

      case e: RawEnum =>
        Right(
          LeafScope(gen.next(), ScopeName(e.name.name), FullRawDefn(e, isRoot))
        )

      case f: RawForeign =>
        Right(
          LeafScope(gen.next(), ScopeName(f.name.name), FullRawDefn(f, isRoot))
        )

    }
  }

}
