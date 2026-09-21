package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, EvolutionIssue}
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.BaboonFamilyManager
import io.septimalmind.baboon.typer.model.Conversion.{CopyEnumByName, CustomConversionRequired, DtoConversion, FieldOp, RemovedTypeNoConversion}
import io.septimalmind.baboon.typer.model.{BaboonFamily, Conversion, DerivationFailure, DomainMember, EvolutionStep, Field, Pkg, TypeId, Typedef, Version}
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.reflect.TagKK

/** Soundness of declared `was` renames across a version chain.
  *
  * Two independent hazards, both rooted in the fact that a `was` annotation is carried forward by
  * every later version of a type while the per-pair diffs only ever see two adjacent versions:
  *
  *   1. a carried-forward annotation must not be mistaken for a typo (its ancestry is validated
  *      once per package, against every earlier version, rather than per version pair);
  *   2. a rename whose target name the previous version also used — a name swap, or a rename onto
  *      the name of a field being dropped — must produce rename ops rather than being silently
  *      accounted for as a positional transfer. A swap that preserves field order leaves both
  *      structural signatures untouched, so the type has to be classified as locally modified on
  *      the strength of the annotation alone.
  */
final class RenameSoundnessTest extends RenameSoundnessTestBase[Either]

abstract class RenameSoundnessTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def makeInput(name: String, body: String): BaboonParser.Input =
    BaboonParser.Input(FSPath.parse(NEString.unsafeFrom(name)), body)

  private def load(manager: BaboonFamilyManager[F], bodies: (String, String)*): F[NEList[BaboonIssue], BaboonFamily] =
    manager.load(bodies.toList.map { case (n, b) => makeInput(n, b) })

  private def conversionsOf(family: BaboonFamily, pkg: String, from: String, to: String): List[Conversion] = {
    val lineage = family.domains.toMap(Pkg(NEList.unsafeFrom(pkg.split('.').toList)))
    lineage.evolution.rules(EvolutionStep(Version.parse(from), Version.parse(to))).conversions
  }

  private def dtoConversion(family: BaboonFamily, pkg: String, from: String, to: String, name: String): DtoConversion = {
    val found = conversionsOf(family, pkg, from, to).collect {
      case c: DtoConversion if c.targetTpe.name.name == name => c
    }
    assert(found.size == 1, s"expected exactly one DtoConversion for $name in $from->$to, got $found")
    found.head
  }

  private def fieldsOf(family: BaboonFamily, pkg: String, version: String, name: String): List[Field] = {
    val lineage = family.domains.toMap(Pkg(NEList.unsafeFrom(pkg.split('.').toList)))
    val domain  = lineage.versions.toMap(Version.parse(version))
    domain.defs.meta.nodes.values.collect {
      case DomainMember.User(_, d: Typedef.Dto, _, _) if d.id.name.name == name => d.fields
    }.toList match {
      case one :: Nil => one
      case other      => fail(s"expected exactly one dto named $name in $version, got $other")
    }
  }

  private def forwardRun(family: BaboonFamily, pkg: String, at: String, typeName: String): List[(String, String)] = {
    val lineage = family.domains.toMap(Pkg(NEList.unsafeFrom(pkg.split('.').toList)))
    val entries = lineage.evolution.typesForwardReadable(Version.parse(at)).collect {
      case (id: TypeId.User, fr) if id.name.name == typeName => fr
    }.toList
    assert(entries.size == 1, s"expected exactly one type named $typeName at $at, got $entries")
    entries.head.readable.toList.map { case (v, g) => (v.toString, g.wireName) }
  }

  private def renames(c: DtoConversion): Set[(String, String)] = c.ops.collect {
    case r: FieldOp.Rename => (r.sourceFieldName.name, r.targetField.name.name)
  }.toSet

  private def transfers(c: DtoConversion): Set[String] = c.ops.collect {
    case t: FieldOp.Transfer => t.targetField.name.name
  }.toSet

  "declared renames" should {

    "convert a name swap that also reorders the fields" in {
      (manager: BaboonFamilyManager[F]) =>
        val v1 = """model rsnd.swapreordered
                   |version "1.0.0"
                   |root data S { a: i32  b: i32 }
                   |""".stripMargin
        val v2 = """model rsnd.swapreordered
                   |version "1.1.0"
                   |import "1.0.0" { * } without { S }
                   |root data S { b: i32 was a  a: i32 was b }
                   |""".stripMargin
        load(manager, "v1.baboon" -> v1, "v2.baboon" -> v2).map {
          family =>
            val c = dtoConversion(family, "rsnd.swapreordered", "1.0.0", "1.1.0", "S")
            assert(renames(c) == Set(("a", "b"), ("b", "a")), s"ops: ${c.ops}")
            assert(transfers(c).isEmpty, s"ops: ${c.ops}")
            assert(c.removed.isEmpty, s"removed: ${c.removed}")
        }
    }

    "convert a name swap that leaves both structural signatures intact" in {
      (manager: BaboonFamilyManager[F]) =>
        // `shallowId` sorts `name:type` and `deepId` is positional: with the field ORDER preserved,
        // neither signature notices this swap. Only the annotations say the values move.
        val v1 = """model rsnd.swapinplace
                   |version "1.0.0"
                   |root data S { a: i32  b: i32 }
                   |""".stripMargin
        val v2 = """model rsnd.swapinplace
                   |version "1.1.0"
                   |import "1.0.0" { * } without { S }
                   |root data S { a: i32 was b  b: i32 was a }
                   |""".stripMargin
        load(manager, "v1.baboon" -> v1, "v2.baboon" -> v2).map {
          family =>
            val c = dtoConversion(family, "rsnd.swapinplace", "1.0.0", "1.1.0", "S")
            assert(renames(c) == Set(("b", "a"), ("a", "b")), s"ops: ${c.ops}")
            assert(transfers(c).isEmpty, s"ops: ${c.ops}")
        }
    }

    "treat a field whose name a rename takes over as removed" in {
      (manager: BaboonFamilyManager[F]) =>
        val v1 = """model rsnd.takeover
                   |version "1.0.0"
                   |root data S { a: i32  b: str }
                   |""".stripMargin
        val v2 = """model rsnd.takeover
                   |version "1.1.0"
                   |import "1.0.0" { * } without { S }
                   |root data S { b: i32 was a }
                   |""".stripMargin
        load(manager, "v1.baboon" -> v1, "v2.baboon" -> v2).map {
          family =>
            val c = dtoConversion(family, "rsnd.takeover", "1.0.0", "1.1.0", "S")
            assert(renames(c) == Set(("a", "b")), s"ops: ${c.ops}")
            assert(transfers(c).isEmpty, s"ops: ${c.ops}")
            assert(c.removed.map(_.name.name) == Set("b"), s"removed: ${c.removed}")
        }
    }

    "swap enum members by their declared previous names" in {
      (manager: BaboonFamilyManager[F]) =>
        val v1 = """model rsnd.enumswap
                   |version "1.0.0"
                   |enum E { A B }
                   |root data H { e: E }
                   |""".stripMargin
        val v2 = """model rsnd.enumswap
                   |version "1.1.0"
                   |import "1.0.0" { * } without { E H }
                   |enum E { B : was[A] A : was[B] }
                   |root data H { e: E }
                   |""".stripMargin
        load(manager, "v1.baboon" -> v1, "v2.baboon" -> v2).map {
          family =>
            val mappings = conversionsOf(family, "rsnd.enumswap", "1.0.0", "1.1.0").collect {
              case c: CopyEnumByName if c.targetTpe.name.name == "E" => c.memberMapping
            }
            assert(mappings == List(Map("A" -> "B", "B" -> "A")), s"mappings: $mappings")
        }
    }

    "accept an annotation carried forward into a later version" in {
      (manager: BaboonFamilyManager[F]) =>
        // `r` was renamed from `b` in 1.1.0; the annotation survives into 1.2.0, where 1.1.0 has no
        // `b` any more. That is not a typo, and it is not a rename at the 1.1.0 -> 1.2.0 step.
        val v1 = """model rsnd.carried
                   |version "1.0.0"
                   |root data T { a: i32  b: str }
                   |""".stripMargin
        val v2 = """model rsnd.carried
                   |version "1.1.0"
                   |import "1.0.0" { * } without { T }
                   |root data T { a: i32  r: str was b }
                   |""".stripMargin
        val v3 = """model rsnd.carried
                   |version "1.2.0"
                   |import "1.1.0" { * } without { T }
                   |root data T { a: i32  r: str was b  z: opt[i32] }
                   |""".stripMargin
        load(manager, "v1.baboon" -> v1, "v2.baboon" -> v2, "v3.baboon" -> v3).map {
          family =>
            val first = dtoConversion(family, "rsnd.carried", "1.0.0", "1.1.0", "T")
            assert(renames(first) == Set(("b", "r")), s"ops: ${first.ops}")

            val second = dtoConversion(family, "rsnd.carried", "1.1.0", "1.2.0", "T")
            assert(renames(second).isEmpty, s"ops: ${second.ops}")
            assert(transfers(second) == Set("a", "r"), s"ops: ${second.ops}")
        }
    }

    "accept a stale annotation being dropped again" in {
      (manager: BaboonFamilyManager[F]) =>
        val v1 = """model rsnd.dropped
                   |version "1.0.0"
                   |root data T { a: i32  b: str }
                   |""".stripMargin
        val v2 = """model rsnd.dropped
                   |version "1.1.0"
                   |import "1.0.0" { * } without { T }
                   |root data T { a: i32  r: str was b }
                   |""".stripMargin
        val v3 = """model rsnd.dropped
                   |version "1.2.0"
                   |import "1.1.0" { * } without { T }
                   |root data T { a: i32  r: str }
                   |""".stripMargin
        load(manager, "v1.baboon" -> v1, "v2.baboon" -> v2, "v3.baboon" -> v3).map {
          family =>
            val c = dtoConversion(family, "rsnd.dropped", "1.1.0", "1.2.0", "T")
            assert(renames(c).isEmpty, s"ops: ${c.ops}")
            assert(transfers(c) == Set("a", "r"), s"ops: ${c.ops}")

            // Field ops address the TARGET, and `Field` equality includes `prevName`: 1.1.0's `r`
            // still carries `was b` while 1.2.0's does not. Ops keyed off the source fields would
            // leave every target field unmatched in the code generators.
            assert(
              c.ops.map(_.targetField).toSet == fieldsOf(family, "rsnd.dropped", "1.2.0", "T").toSet,
              s"ops: ${c.ops}",
            )
        }
    }

    "treat an ADT branch whose name a rename takes over as removed" in {
      (manager: BaboonFamilyManager[F]) =>
        // The model says the branch now called `X` IS the old `Y`, and the old `X` is gone. The old
        // `X` therefore has nowhere to go: the ADT conversion cannot be derived, and no conversion
        // may be emitted that maps the old `X` onto the new one.
        val v1 = """model rsnd.branchtakeover
                   |version "1.0.0"
                   |root adt A {
                   |   data X { a: i32 }
                   |   data Y { a: i32 }
                   |}
                   |""".stripMargin
        val v2 = """model rsnd.branchtakeover
                   |version "1.1.0"
                   |import "1.0.0" { * } without { A }
                   |root adt A {
                   |   data X : was[Y] { a: i32 }
                   |}
                   |""".stripMargin
        load(manager, "v1.baboon" -> v1, "v2.baboon" -> v2).map {
          family =>
            val convs = conversionsOf(family, "rsnd.branchtakeover", "1.0.0", "1.1.0")

            assert(
              convs.exists {
                case c: CustomConversionRequired =>
                  c.sourceTpe.name.name == "A" && c.reason.isInstanceOf[DerivationFailure.AdtBranchRemoved]
                case _ => false
              },
              s"expected AdtBranchRemoved for A; got $convs",
            )
            assert(
              convs.exists {
                case c: RemovedTypeNoConversion => c.sourceTpe.name.name == "X"
                case _                          => false
              },
              s"expected the old X branch to be reported removed; got $convs",
            )
            assert(
              !convs.exists {
                case c: DtoConversion => c.sourceTpe.name.name == "X" && c.targetTpe.name.name == "X"
                case _                => false
              },
              s"the old X must not be converted into the new X; got $convs",
            )
            assert(
              convs.exists {
                case c: DtoConversion => c.sourceTpe.name.name == "Y" && c.targetTpe.name.name == "X"
                case _                => false
              },
              s"expected the declared Y -> X branch conversion; got $convs",
            )
        }
    }

    "reject a declared swap of two ADT branch names" in {
      (manager: BaboonFamilyManager[F]) =>
        // Both declared sources still exist under their own TypeIds in the new version, so there is
        // nothing to map one onto the other. Discarding the annotations silently, as the rename
        // filter did on its own, loses the declaration without a word.
        val v1 = """model rsnd.branchswap
                   |version "1.0.0"
                   |root adt A {
                   |   data X { a: i32 }
                   |   data Y { b: str }
                   |}
                   |""".stripMargin
        val v2 = """model rsnd.branchswap
                   |version "1.1.0"
                   |import "1.0.0" { * } without { A }
                   |root adt A {
                   |   data X : was[Y] { b: str }
                   |   data Y : was[X] { a: i32 }
                   |}
                   |""".stripMargin
        F.attempt(load(manager, "v1.baboon" -> v1, "v2.baboon" -> v2)).map {
          case Left(issues) =>
            val renames = issues.toList.collect {
              case BaboonIssue.Evolution(i: EvolutionIssue.RenameSourceStillPresent) =>
                (i.typeId.name.name, i.prevTypeId.name.name)
            }.toSet
            assert(renames == Set(("X", "Y"), ("Y", "X")), s"expected both directions reported; got $issues")
          case Right(_) =>
            fail("expected RenameSourceStillPresent, but the model compiled clean")
        }
    }

    "reject a type rename from a type no earlier version ever defined" in {
      (manager: BaboonFamilyManager[F]) =>
        val v1 = """model rsnd.typetypo
                   |version "1.0.0"
                   |root data T { a: i32 }
                   |""".stripMargin
        val v2 = """model rsnd.typetypo
                   |version "1.1.0"
                   |import "1.0.0" { * }
                   |root data U : was[Ghost] { a: i32 }
                   |""".stripMargin
        F.attempt(load(manager, "v1.baboon" -> v1, "v2.baboon" -> v2)).map {
          case Left(issues) =>
            assert(
              issues.toList.exists {
                case BaboonIssue.Evolution(i: EvolutionIssue.InvalidTypeRename) =>
                  i.typeId.name.name == "U" && i.prevTypeId.name.name == "Ghost"
                case _ => false
              },
              s"expected InvalidTypeRename for U was[Ghost]; got $issues",
            )
          case Right(_) =>
            fail("expected InvalidTypeRename, but the model compiled clean")
        }
    }

    "reject a field rename on a type that has no earlier version" in {
      (manager: BaboonFamilyManager[F]) =>
        // `Fresh` is introduced in 1.1.0 and is not a rename target, so `was b` names nothing.
        val v1 = """model rsnd.freshtype
                   |version "1.0.0"
                   |root data T { a: i32 }
                   |""".stripMargin
        val v2 = """model rsnd.freshtype
                   |version "1.1.0"
                   |import "1.0.0" { * }
                   |root data Fresh { r: str was b }
                   |""".stripMargin
        F.attempt(load(manager, "v1.baboon" -> v1, "v2.baboon" -> v2)).map {
          case Left(issues) =>
            assert(
              issues.toList.exists {
                case BaboonIssue.Evolution(i: EvolutionIssue.InvalidFieldRename) => i.typeId.name.name == "Fresh"
                case _                                                           => false
              },
              s"expected InvalidFieldRename for Fresh; got $issues",
            )
          case Right(_) =>
            fail("expected InvalidFieldRename, but the model compiled clean")
        }
    }

    "accept a `was` clause in a single-version model" in {
      (manager: BaboonFamilyManager[F]) =>
        // `BaboonSchemeRenderer` emits one version at a time and the result must load back, so a
        // lone version carrying a rename is a real artifact. There is nothing there for the clause
        // to have been renamed away from, so the clause carries no claim to check.
        val v1 = """model rsnd.single
                   |version "1.0.0"
                   |root data T { a: i32  r: str was b }
                   |enum E { B2 : was[B] }
                   |root data H { e: E }
                   |""".stripMargin
        load(manager, "v1.baboon" -> v1).map {
          family =>
            assert(family.domains.toMap.keySet.exists(_.toString == "rsnd.single"))
        }
    }

    "keep a host's forward bound when a nested type is renamed" in {
      (manager: BaboonFamilyManager[F]) =>
        // Neither format puts a nested value's type name on the wire, so renaming `Leaf` moves no
        // byte of a `Host` payload. The host must keep its bound; the renamed type must not get one
        // of its own, because a top-level payload is identified by the typeId in its envelope.
        val v1 = """model rsnd.nestedrename
                   |version "1.0.0"
                   |data Leaf : derived[json], derived[ueba] { a: i32 }
                   |root data Host : derived[json], derived[ueba] { l: Leaf  b: i32 }
                   |""".stripMargin
        val v2 = """model rsnd.nestedrename
                   |version "1.1.0"
                   |import "1.0.0" { * } without { Leaf Host }
                   |data Renamed : was[Leaf], derived[json], derived[ueba] { a: i32 }
                   |root data Host : derived[json], derived[ueba] { l: Renamed  b: i32 }
                   |""".stripMargin
        load(manager, "v1.baboon" -> v1, "v2.baboon" -> v2).map {
          family =>
            assert(
              forwardRun(family, "rsnd.nestedrename", "1.0.0", "Host") ==
              List(("1.0.0", "identical"), ("1.1.0", "identical"))
            )
            assert(forwardRun(family, "rsnd.nestedrename", "1.0.0", "Leaf") == List(("1.0.0", "identical")))
        }
    }

    "read a renamed ADT branch in UEBA but not in JSON" in {
      (manager: BaboonFamilyManager[F]) =>
        // UEBA writes the branch INDEX, JSON writes the branch NAME. Renaming a branch in place
        // moves no UEBA byte and breaks the JSON discriminator, so the two axes must part company.
        val v1 = """model rsnd.branchrename
                   |version "1.0.0"
                   |root adt A : derived[json], derived[ueba] {
                   |   data X { a: i32 }
                   |   data Y { b: str }
                   |}
                   |""".stripMargin
        val v2 = """model rsnd.branchrename
                   |version "1.1.0"
                   |import "1.0.0" { * } without { A }
                   |root adt A : derived[json], derived[ueba] {
                   |   data X2 : was[X] { a: i32 }
                   |   data Y { b: str }
                   |}
                   |""".stripMargin
        load(manager, "v1.baboon" -> v1, "v2.baboon" -> v2).map {
          family =>
            assert(
              forwardRun(family, "rsnd.branchrename", "1.0.0", "A") ==
              List(("1.0.0", "identical"), ("1.1.0", "ueba-identical"))
            )
            // the untouched branch keeps both axes; the renamed one gets no bound of its own
            assert(
              forwardRun(family, "rsnd.branchrename", "1.0.0", "Y") ==
              List(("1.0.0", "identical"), ("1.1.0", "identical"))
            )
            assert(forwardRun(family, "rsnd.branchrename", "1.0.0", "X") == List(("1.0.0", "identical")))
        }
    }

    "carry the old spelling of a renamed field type on the transfer op" in {
      (manager: BaboonFamilyManager[F]) =>
        // Guards the compiler half of the rename-aware transfer. The translator half -- each
        // backend actually USING `sourceTpe` -- is guarded by the nine codegen lanes compiling
        // `rename-nested-ok`: a renamed type's new name never exists in the old version, so
        // ignoring `sourceTpe` always yields a dangling reference that fails to compile.
        val v1 = """model rsnd.transfersrc
                   |version "1.0.0"
                   |data Leaf { a: i32 }
                   |root data Host { l: Leaf  b: str }
                   |""".stripMargin
        val v2 = """model rsnd.transfersrc
                   |version "1.1.0"
                   |import "1.0.0" { * } without { Leaf Host }
                   |data Renamed : was[Leaf] { a: i32 }
                   |root data Host { l: Renamed  b: str }
                   |""".stripMargin
        load(manager, "v1.baboon" -> v1, "v2.baboon" -> v2).map {
          family =>
            val c = dtoConversion(family, "rsnd.transfersrc", "1.0.0", "1.1.0", "Host")

            val sources = c.ops.collect {
              case t: FieldOp.Transfer => (t.targetField.name.name, t.sourceTpe.toString)
            }.toSet
            assert(
              sources == Set(("l", "rsnd.transfersrc/:#Leaf"), ("b", "#str")),
              s"transfer ops must address the source by its OLD spelling; got $sources",
            )

            val targets = c.ops.collect {
              case t: FieldOp.Transfer => (t.targetField.name.name, t.targetField.tpe.toString)
            }.toSet
            assert(
              targets == Set(("l", "rsnd.transfersrc/:#Renamed"), ("b", "#str")),
              s"transfer ops must build the target with its NEW spelling; got $targets",
            )
        }
    }

    "end a renamed type's run at the step that renames it, not before" in {
      (manager: BaboonFamilyManager[F]) =>
        // The rename happens mid-chain: a 1.0.0 reader can still decode a 1.1.0 `Leaf`, because the
        // typeId is unchanged there, and must not claim 1.2.0, where the envelope would name a type
        // it has never heard of. The host spans the whole chain either way.
        val v1 = """model rsnd.midchain
                   |version "1.0.0"
                   |data Leaf { a: i32 }
                   |root data Host { l: Leaf  b: str }
                   |""".stripMargin
        val v2 = """model rsnd.midchain
                   |version "1.1.0"
                   |import "1.0.0" { * }
                   |""".stripMargin
        val v3 = """model rsnd.midchain
                   |version "1.2.0"
                   |import "1.1.0" { * } without { Leaf Host }
                   |data Renamed : was[Leaf] { a: i32 }
                   |root data Host { l: Renamed  b: str }
                   |""".stripMargin
        load(manager, "v1.baboon" -> v1, "v2.baboon" -> v2, "v3.baboon" -> v3).map {
          family =>
            assert(
              forwardRun(family, "rsnd.midchain", "1.0.0", "Leaf") ==
              List(("1.0.0", "identical"), ("1.1.0", "identical")),
              "a 1.0.0 reader reads 1.1.0's Leaf but must not claim 1.2.0",
            )
            assert(
              forwardRun(family, "rsnd.midchain", "1.0.0", "Host") ==
              List(("1.0.0", "identical"), ("1.1.0", "identical"), ("1.2.0", "identical"))
            )
            assert(forwardRun(family, "rsnd.midchain", "1.1.0", "Leaf") == List(("1.1.0", "identical")))
        }
    }

    "reject a field rename from a name no earlier version ever had" in {
      (manager: BaboonFamilyManager[F]) =>
        val v1 = """model rsnd.fieldtypo
                   |version "1.0.0"
                   |root data T { a: i32  b: str }
                   |""".stripMargin
        val v2 = """model rsnd.fieldtypo
                   |version "1.1.0"
                   |import "1.0.0" { * } without { T }
                   |root data T { a: i32  r: str was qqq }
                   |""".stripMargin
        F.attempt(load(manager, "v1.baboon" -> v1, "v2.baboon" -> v2)).map {
          case Left(issues) =>
            assert(
              issues.toList.exists {
                case BaboonIssue.Evolution(_: EvolutionIssue.InvalidFieldRename) => true
                case _                                                           => false
              },
              s"expected InvalidFieldRename; got $issues",
            )
          case Right(_) =>
            fail("expected InvalidFieldRename, but the model compiled clean")
        }
    }

    "reject an enum member rename from a name no earlier version ever had" in {
      (manager: BaboonFamilyManager[F]) =>
        val v1 = """model rsnd.enumtypo
                   |version "1.0.0"
                   |enum E { A B }
                   |root data H { e: E }
                   |""".stripMargin
        val v2 = """model rsnd.enumtypo
                   |version "1.1.0"
                   |import "1.0.0" { * } without { E H }
                   |enum E { A B2 : was[Q] }
                   |root data H { e: E }
                   |""".stripMargin
        F.attempt(load(manager, "v1.baboon" -> v1, "v2.baboon" -> v2)).map {
          case Left(issues) =>
            assert(
              issues.toList.exists {
                case BaboonIssue.Evolution(_: EvolutionIssue.InvalidEnumMemberRename) => true
                case _                                                                => false
              },
              s"expected InvalidEnumMemberRename; got $issues",
            )
          case Right(_) =>
            fail("expected InvalidEnumMemberRename, but the model compiled clean")
        }
    }
  }
}
