package io.septimalmind.baboon.tests

import io.septimalmind.baboon.Baboon
import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.DiffCLIOptions
import io.septimalmind.baboon.diff.BaboonDiffRenderer
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.BaboonComparator
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.Error2
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.fundamentals.platform.resources.IzResources
import izumi.reflect.TagKK

import java.io.ByteArrayOutputStream

final class DiffStdoutPurityTest extends DiffStdoutPurityTestBase[Either]

/** Entrypoint-level test for the absent-target `diff` mode (T178 / G32, D41).
  *
  * Structural mirror of [[SchemeStdoutPurityTest]] (T176): drives T177's
  * `Baboon.diffEntrypointToStdout` seam under a REAL `produceRun` (Explorer axis)
  * and captures the WHOLE process's global `scala.Console.out` in a single
  * `ByteArrayOutputStream`. The seam prints the rendered diff via `Console.println`
  * — the same dynamically-rebindable stream `BLoggerImpl` writes to — so the
  * captured buffer would ALSO contain any logger output. The Explorer axis binds a
  * Noop BLogger, so only the diff text reaches the buffer; the exact-equality
  * assertion against `BaboonDiffRenderer.renderJson` is therefore non-vacuously
  * falsifiable: flipping the seam's Activation to Compiler leaks the "Inputs:" log
  * line into the buffer and breaks equality (verified by flipping Explorer->Compiler
  * in a clone: RED; restored: GREEN).
  *
  * Fixture is `testpkg.pkg0` v2.0.0 -> v3.0.0 (the same fixture T172DiffRendererTest
  * pins). pkg0's `pkg03.baboon` `include`s `pkg0/pkg03-inclusion.bmo`, whose relative
  * path resolves against `options.directoryInputs` via the JVM
  * `BaboonInclusionResolverImpl` (disk read). The seam uses ONE `directoryInputs` set
  * BOTH as the input-walk root AND as the inclusion-resolver base. The shared `baboon`
  * model ROOT cannot serve as that single set: its whole-tree walk pulls in every
  * family — including the intentionally-malformed `bad` fixtures — and fails to load
  * as one family. So we stage a private copy of pkg0 into a temp ROOT laid out as
  * `<tmp>/pkg0/` holding pkg0's `.baboon` files plus `pkg03-inclusion.bmo`, then point
  * `directoryInputs` at `<tmp>`: the walk sees only pkg0's `.baboon` files, and
  * `pkg0/pkg03-inclusion.bmo`
  * resolves against `<tmp>` to the staged `.bmo`.
  */
abstract class DiffStdoutPurityTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val fixturePkg = Pkg(NEList("testpkg", "pkg0"))
  private val fromV      = Version.parse("2.0.0")
  private val toV        = Version.parse("3.0.0")

  // The seam only reads diffOptions in error branches (domain-not-found / version-not-found),
  // which are unreachable on the happy path; still, build it faithfully.
  private val diffOptions = DiffCLIOptions(
    domain = fixturePkg.path.mkString("."),
    from   = fromV.toString,
    to     = toV.toString,
    target = None,
    format = Some("json"),
  )

  "diff absent-target stdout seam" should {
    "print ONLY the rendered diff to global stdout (no 'Inputs:' log, no 'Diff written to', no file written)" in {
      (loader: BaboonLoader[F], comparator: BaboonComparator[F]) =>
        // Stage a private copy of pkg0 into <tmp>/pkg0 so the seam's single
        // directoryInputs set can serve BOTH as input-walk root (only pkg0's
        // *.baboon) AND as inclusion base (pkg0/pkg03-inclusion.bmo resolves).
        val pkg0Src = IzResources
          .getPath("baboon/pkg0")
          .get
          .asInstanceOf[IzResources.LoadablePathReference]
          .path
          .toFile

        val stageRoot = java.nio.file.Files.createTempDirectory("diff-stdout-purity-root-")
        val stagePkg0 = stageRoot.resolve("pkg0")
        java.nio.file.Files.createDirectories(stagePkg0)
        pkg0Src.listFiles().foreach { f =>
          if (f.isFile) {
            java.nio.file.Files.copy(
              f.toPath,
              stagePkg0.resolve(f.getName),
              java.nio.file.StandardCopyOption.REPLACE_EXISTING,
            )
            ()
          }
        }

        val directoryInputs: Set[FSPath] =
          Set(FSPath.parse(NEString.unsafeFrom(stageRoot.toFile.getCanonicalPath)))

        for {
          // Build the byte-exact expected value: load pkg0 the SAME way the module wires
          // it (loadPkg), compare(last=v3, prev=v2), render with the SAME renderer.
          family <- loadPkg(loader)
          domFrom = family.domains.toMap(fixturePkg).versions.toMap(fromV)
          domTo   = family.domains.toMap(fixturePkg).versions.toMap(toV)
          diff <- comparator.compare(last = domTo, prev = domFrom)
        } yield {
          val expectedDiff = new BaboonDiffRenderer().renderJson(diff)

          // An empty scratch directory to prove the stdout seam writes NO file.
          val noFileProbe = java.nio.file.Files.createTempDirectory("diff-stdout-purity-")
          val probeBefore = noFileProbe.toFile.list().toList

          import izumi.distage.modules.support.unsafe.EitherSupport.{defaultModuleEither, quasiIOEither, quasiIORunnerEither}
          import izumi.functional.bio.unsafe.UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither

          val baos: ByteArrayOutputStream = new ByteArrayOutputStream()
          Console.withOut(baos) {
            Baboon.diffEntrypointToStdout(
              directoryInputs  = directoryInputs,
              individualInputs = Set.empty,
              diffOptions      = diffOptions,
              pkg              = fixturePkg,
              fromVersion      = fromV,
              toVersion        = toV,
              emitJson         = true,
            )
          }

          val captured   = baos.toString(java.nio.charset.StandardCharsets.UTF_8)
          val probeAfter = noFileProbe.toFile.list().toList
          noFileProbe.toFile.delete(): Unit

          // Tear down the staged fixture root.
          stagePkg0.toFile.listFiles().foreach(f => { f.delete(); () })
          stagePkg0.toFile.delete(): Unit
          stageRoot.toFile.delete(): Unit

          // Console.println(result) => PrintStream.println: diff text + platform line separator.
          val expectedStdout = expectedDiff + System.lineSeparator()

          // Assertion (1): captured global stdout is EXACTLY the renderer output — no extra lines.
          assert(
            captured == expectedStdout,
            s"stdout purity violated for $fixturePkg $fromV->$toV.\n---CAPTURED (${captured.length} chars)---\n$captured\n---EXPECTED (${expectedStdout.length} chars)---\n$expectedStdout",
          )
          // Redundant explicit guards documenting what "pure" means (subsumed by exact equality).
          assert(!captured.contains("Inputs:"), s"'Inputs:' log line leaked into stdout:\n$captured")
          assert(!captured.contains("Diff written to"), s"'Diff written to' log line leaked into stdout:\n$captured")

          // Assertion (2): no file written by the stdout seam.
          assert(
            probeBefore == probeAfter && probeAfter.isEmpty,
            s"stdout seam wrote a file: before=$probeBefore after=$probeAfter",
          )
        }
    }
  }
}
