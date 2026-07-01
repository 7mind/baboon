package io.septimalmind.baboon.tests

import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.diff.{BaboonDiffRenderer, OpDiffFormatter}
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.BaboonComparator
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.Error2
import izumi.fundamentals.collections.nonempty.NEList
import izumi.reflect.TagKK

/** T172 (G29): structural ScalaTest coverage for the public
  * [[BaboonComparator.compare]] entry point (T166), the shared per-op
  * [[OpDiffFormatter]] (T167), and the whole-domain [[BaboonDiffRenderer]]
  * (T168).
  *
  * Two parts, deliberately disjoint in what they pin:
  *
  *   - PART A drives the real fixture (`testpkg.pkg0` v2.0.0 -> v3.0.0)
  *     through `compare` and `renderJson`. It asserts the coarse
  *     `BaboonChanges.added` type-set — and the JSON `changes.added` array —
  *     surface the types introduced in v3 (`S1`, `I1`, `T7_D1`).
  *
  *   - PART B directly exercises the `ServiceOp` and `ContractOp` arms of
  *     [[OpDiffFormatter.formatOp]]. This is necessary because
  *     `BaboonComparator.compare` computes a per-type `TypedefDiff` ONLY for
  *     `changed` (kept-but-modified) + `renamed` types — never for `added`
  *     types. `S1`/`I1` are *added* in v3, so they carry NO `TypedefDiff` and
  *     no `ContractOp`/`ServiceOp` is produced by the fixture. Part A alone
  *     therefore never executes the service/contract rendering arms. Part B
  *     constructs a `ServiceOp.AddMethod` and a `ContractOp.AddField` directly
  *     and asserts `formatOp` renders their marker text, so deleting or
  *     breaking either arm fails this test (the T167 rendering-coverage gap
  *     this fixture is pinned for).
  */
final class T172DiffRendererTest extends T172DiffRendererTestBase[Either]

abstract class T172DiffRendererTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val testPkg = Pkg(NEList("testpkg", "pkg0"))
  private val v2      = Version.parse("2.0.0")
  private val v3      = Version.parse("3.0.0")

  /** The v3 additions this suite pins. All three are declared in `pkg03.baboon`
    * (v3) and absent from `pkg02.baboon` (v2), so `compare` places them in the
    * `added` type-set.
    */
  private val addedNames = Set("S1", "I1", "T7_D1")

  /** `BaboonDiffRenderer` renders each id as its fully-qualified label
    * (`u.render`, e.g. `testpkg.pkg0.S1`). Match on the final dot-segment so
    * the assertion is robust to the pkg/owner prefix.
    */
  private def containsBareName(labels: Iterable[String], bare: String): Boolean =
    labels.exists(l => l == bare || l.split('.').lastOption.contains(bare))

  "T172 compare + BaboonDiffRenderer (Part A: real fixture)" should {
    "expose v3 additions (S1, I1, T7_D1) in compare's added-set and renderJson" in {
      (loader: BaboonLoader[F], comparator: BaboonComparator[F]) =>
        for {
          family <- loadPkg(loader)
          domV2   = family.domains.toMap(testPkg).versions.toMap(v2)
          domV3   = family.domains.toMap(testPkg).versions.toMap(v3)
          diff   <- comparator.compare(last = domV3, prev = domV2)
        } yield {
          // (a) coarse added-set (by bare type name) contains S1, I1, T7_D1.
          val addedByName = diff.changes.added.collect { case u: TypeId.User => u.name.name }
          addedNames.foreach { n =>
            assert(
              addedByName.contains(n),
              s"Expected $n in compare's added-set; got added=$addedByName",
            )
          }

          // (c) renderJson round-trips through circe and its changes.added array
          // carries labels ending in the same bare names.
          val json = new BaboonDiffRenderer().renderJson(diff)
          val parsed = io.circe.parser
            .parse(json)
            .fold(err => fail(s"renderJson did not produce valid JSON: $err\n$json"), identity)

          val jsonAdded: List[String] =
            parsed.hcursor
              .downField("changes")
              .downField("added")
              .as[List[String]]
              .fold(err => fail(s"changes.added not a string array: $err\n$json"), identity)

          addedNames.foreach { n =>
            assert(
              containsBareName(jsonAdded, n),
              s"Expected renderJson changes.added to contain a label for $n; got $jsonAdded",
            )
          }
        }
    }
  }

  "T172 OpDiffFormatter (Part B: direct ServiceOp/ContractOp arms)" should {
    // Directly exercise the two arms that the fixture's v2->v3 diff cannot
    // reach (added services/contracts carry no TypedefDiff). Deleting either
    // arm from OpDiffFormatter.formatOp fails these assertions.
    "render a constructed ServiceOp marker AND a constructed ContractOp marker" in {
      (_: BaboonLoader[F]) =>
        val formatter = new OpDiffFormatter(useColor = false)

        // ServiceOp.AddMethod — formatOp renders the method's name.
        val methodName = "t172Method"
        val method = Typedef.MethodDef(
          name = Typedef.MethodName(methodName),
          sig  = TypeRef.Scalar(TypeId.Builtins.str),
          out  = None,
          err  = None,
        )
        val serviceRendered = formatter.formatOp(ServiceOp.AddMethod(method))
        assert(
          serviceRendered.contains(methodName),
          s"ServiceOp.AddMethod arm must render the method name; got: $serviceRendered",
        )
        assert(
          serviceRendered.contains("method"),
          s"ServiceOp.AddMethod arm must render the 'method' marker; got: $serviceRendered",
        )

        // ContractOp.AddField — formatOp renders the field's name.
        val fieldName = "t172Field"
        val field = Field(
          name     = FieldName(fieldName),
          tpe      = TypeRef.Scalar(TypeId.Builtins.str),
          prevName = None,
        )
        val contractRendered = formatter.formatOp(ContractOp.AddField(field))
        assert(
          contractRendered.contains(fieldName),
          s"ContractOp.AddField arm must render the field name; got: $contractRendered",
        )

        // The `+` add-marker distinguishes the Add arm from Remove/Keep.
        assert(
          serviceRendered.startsWith("+") && contractRendered.startsWith("+"),
          s"Add arms must render a '+' marker; got service='$serviceRendered' contract='$contractRendered'",
        )
    }
  }
}
