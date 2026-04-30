package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.BaboonFamilyManager
import io.septimalmind.baboon.typer.model.{BaboonLineage, Conversion, EvolutionStep, Pkg, Version}
import io.septimalmind.baboon.typer.model.Conversion.{DtoConversion, FieldOp}
import izumi.functional.bio.Error2
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.reflect.TagKK

/** PR-58 (M18.5): cross-version id↔data conversion verification.
  *
  * Verifies plan §5.1 and §5.2:
  * - §5.1: BaboonComparator classifies data→id and id→data as Unchanged when the
  *   field shape is identical (isIdentifier is excluded from the shallowId hash in
  *   BaboonEnquiries.shallowId, so the two variants produce the same hash).
  * - §5.2: BaboonRules produces DtoConversion with Transfer ops for all fields
  *   (the existing DTO-conversion path fires for both `data` and `id` because
  *   both are Typedef.Dto).
  *
  * Two evolution directions are tested:
  *   data UserId → id UserId   (promotion)
  *   id UserId   → data UserId (demotion)
  */
final class IdentifierConversionTest extends IdentifierConversionTestBase[Either]

abstract class IdentifierConversionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule]
    extends BaboonTest[F] {

  private def makeInput(name: String, body: String): BaboonParser.Input =
    BaboonParser.Input(FSPath.parse(NEString.unsafeFrom(name)), body)

  // v1: UserId is a plain `data` type.
  private val v1Body =
    """model identifier.evolution
      |version "1.0.0"
      |data UserId : derived[json], derived[ueba] {
      |  u: uid
      |}
      |root data Holder : derived[json], derived[ueba] {
      |  id: UserId
      |}
      |""".stripMargin

  // v2: UserId promoted to `id` — same field shape, only isIdentifier changes.
  private val v2Body =
    """model identifier.evolution
      |version "2.0.0"
      |import "1.0.0" { * }
      |id UserId : derived[json], derived[ueba] {
      |  u: uid
      |}
      |root data Holder : derived[json], derived[ueba] {
      |  id: UserId
      |}
      |""".stripMargin

  // v3: UserId demoted back to `data` — reverse direction.
  private val v3Body =
    """model identifier.evolution
      |version "3.0.0"
      |import "2.0.0" { * }
      |data UserId : derived[json], derived[ueba] {
      |  u: uid
      |}
      |root data Holder : derived[json], derived[ueba] {
      |  id: UserId
      |}
      |""".stripMargin

  "IdentifierConversionTest" should {

    // §5.1: data→id promotion must be classified as Unchanged (not shallowModified).
    // §5.2: BaboonRules must produce DtoConversion with Transfer for the `u` field.
    "data→id promotion: comparator classifies UserId as unmodified, rules produce Transfer" in {
      (manager: BaboonFamilyManager[F]) =>
        for {
          family <- manager.load(
            List(
              makeInput("v1.baboon", v1Body),
              makeInput("v2.baboon", v2Body),
            )
          )
        } yield {
          val pkg  = Pkg(NEList("identifier", "evolution"))
          val v1   = Version.parse("1.0.0")
          val v2   = Version.parse("2.0.0")
          val step = EvolutionStep(v1, v2)

          val lineage: BaboonLineage = family.domains.toMap(pkg)
          val evo                    = lineage.evolution
          val diff                   = evo.diffs(step)

          // §5.1: UserId must be in the unmodified set.
          val unmodifiedNames = diff.changes.unmodified.collect {
            case u: io.septimalmind.baboon.typer.model.TypeId.User => u.name.name
          }
          assert(
            unmodifiedNames.contains("UserId"),
            s"Expected UserId in unmodified for data→id; got unmodified=${unmodifiedNames}, shallowModified=${diff.changes.shallowModified}",
          )

          // §5.2: BaboonRules must produce DtoConversion with Transfer for `u`.
          val ruleset = evo.rules(step)
          val dtoConv = ruleset.conversions.collectFirst {
            case c: DtoConversion if c.sourceTpe.name.name == "UserId" => c
          }.getOrElse(fail("DtoConversion for UserId not found in v1→v2 ruleset"))

          assert(
            dtoConv.ops.forall(_.isInstanceOf[FieldOp.Transfer]),
            s"Expected all ops to be Transfer for data→id; got: ${dtoConv.ops}",
          )

          val transferredFields = dtoConv.ops.collect { case FieldOp.Transfer(f) => f.name.name }
          assert(
            transferredFields.contains("u"),
            s"Expected field `u` to be transferred; got: $transferredFields",
          )
        }
    }

    // §5.1: id→data demotion must also be classified as Unchanged.
    // §5.2: same DtoConversion with Transfer path.
    "id→data demotion: comparator classifies UserId as unmodified, rules produce Transfer" in {
      (manager: BaboonFamilyManager[F]) =>
        for {
          family <- manager.load(
            List(
              makeInput("v1.baboon", v1Body),
              makeInput("v2.baboon", v2Body),
              makeInput("v3.baboon", v3Body),
            )
          )
        } yield {
          val pkg  = Pkg(NEList("identifier", "evolution"))
          val v2   = Version.parse("2.0.0")
          val v3   = Version.parse("3.0.0")
          val step = EvolutionStep(v2, v3)

          val lineage = family.domains.toMap(pkg)
          val evo     = lineage.evolution
          val diff    = evo.diffs(step)

          // §5.1: UserId must be in the unmodified set for id→data direction.
          val unmodifiedNames = diff.changes.unmodified.collect {
            case u: io.septimalmind.baboon.typer.model.TypeId.User => u.name.name
          }
          assert(
            unmodifiedNames.contains("UserId"),
            s"Expected UserId in unmodified for id→data; got unmodified=${unmodifiedNames}, shallowModified=${diff.changes.shallowModified}",
          )

          // §5.2: DtoConversion with Transfer for the `u` field.
          val ruleset = evo.rules(step)
          val dtoConv = ruleset.conversions.collectFirst {
            case c: DtoConversion if c.sourceTpe.name.name == "UserId" => c
          }.getOrElse(fail("DtoConversion for UserId not found in v2→v3 ruleset"))

          assert(
            dtoConv.ops.forall(_.isInstanceOf[FieldOp.Transfer]),
            s"Expected all ops to be Transfer for id→data; got: ${dtoConv.ops}",
          )

          val transferredFields = dtoConv.ops.collect { case FieldOp.Transfer(f) => f.name.name }
          assert(
            transferredFields.contains("u"),
            s"Expected field `u` to be transferred; got: $transferredFields",
          )
        }
    }
  }
}
