package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, EvolutionIssue}
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.BaboonFamilyManager
import io.septimalmind.baboon.typer.model.Conversion.{CopyEnumByName, DtoConversion, FieldOp}
import io.septimalmind.baboon.typer.model.{BaboonFamily, Conversion, DomainMember, EvolutionStep, Field, Pkg, Typedef, Version}
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
