package io.septimalmind.baboon.tests

import io.septimalmind.baboon.Baboon
import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.scheme.BaboonSchemeRenderer
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.Error2
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzResources
import izumi.reflect.TagKK

import java.io.ByteArrayOutputStream

final class SchemeStdoutPurityTest extends SchemeStdoutPurityTestBase[Either]

/** Entrypoint-level test for the absent-target `scheme` mode (T176 / G30).
  *
  * Drives T175's `Baboon.schemeEntrypointToStdout` seam under a REAL `produceRun`
  * (Explorer axis) and captures the WHOLE process's global `scala.Console.out` in a
  * single `ByteArrayOutputStream`. Because the seam prints the rendered scheme via
  * `Console.println` — the same dynamically-rebindable stream `BLoggerImpl` writes to —
  * the captured buffer would ALSO contain any logger output. The Explorer axis binds a
  * Noop BLogger, so only the scheme text reaches the buffer; the exact-equality assertion
  * against `renderer.render` is therefore non-vacuously falsifiable: flipping the seam's
  * Activation to Compiler leaks the "Inputs:" log line into the buffer and breaks equality.
  */
abstract class SchemeStdoutPurityTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  // rename.ns is the SchemeRoundtripTest fixture that is self-contained (two .baboon files, NO
  // .bmo inclusions). That matters: the seam uses ONE `directoryInputs` set both to walk for input
  // *.baboon AND as the inclusion-resolver base (BaboonInclusionResolverImpl resolves against
  // options.directoryInputs). A fixture with inclusions would force directoryInputs to be the shared
  // `baboon` ROOT, whose full-tree walk fails to load as a single family. rename-ns has neither
  // problem: point directoryInputs straight at its own subdir.
  private val fixturePkg     = Pkg(NEList("rename", "ns"))
  private val fixtureVersion = Version.parse("1.0.0")

  "scheme absent-target stdout seam" should {
    "print ONLY the rendered scheme to global stdout (no 'Inputs:' log, no 'Scheme written to', no file written)" in {
      (loader: BaboonLoader[F], renderer: BaboonSchemeRenderer) =>
        // Resolve rename-ns's own subdir; it is self-contained so it doubles safely as both the
        // seam's input-walk root and its inclusion-resolver base.
        val fixtureRoot = IzResources
          .getPath("baboon/rename-ns")
          .get
          .asInstanceOf[IzResources.LoadablePathReference]
          .path

        val directoryInputs: Set[FSPath] =
          Set(FSPath.parse(NEString.unsafeFrom(fixtureRoot.toFile.getCanonicalPath)))

        // Build the byte-exact expected value: load the SAME fixture the SAME way the seam
        // does, then render with the SAME renderer.
        val baboons = IzFiles
          .walk(fixtureRoot.toFile)
          .toList
          .filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))

        for {
          expectedFamily <- loader.load(baboons)
        } yield {
          val expectedScheme = renderer
            .render(expectedFamily, fixturePkg, fixtureVersion)
            .fold(
              e => throw new AssertionError(s"Expected render failed for $fixturePkg@$fixtureVersion: $e"),
              identity,
            )

          // An empty scratch directory used to prove the stdout seam writes NO file: nothing
          // must appear here (the file-mode seam is the only path that writes to disk).
          val noFileProbe = java.nio.file.Files.createTempDirectory("scheme-stdout-purity-")
          val probeBefore = noFileProbe.toFile.list().toList

          import izumi.distage.modules.support.unsafe.EitherSupport.{defaultModuleEither, quasiIOEither, quasiIORunnerEither}
          import izumi.functional.bio.unsafe.UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither

          val baos: ByteArrayOutputStream = new ByteArrayOutputStream()
          Console.withOut(baos) {
            Baboon.schemeEntrypointToStdout(
              directoryInputs  = directoryInputs,
              individualInputs = Set.empty,
              pkg              = fixturePkg,
              version          = fixtureVersion,
            )
          }

          val captured   = baos.toString(java.nio.charset.StandardCharsets.UTF_8)
          val probeAfter = noFileProbe.toFile.list().toList
          val _          = noFileProbe.toFile.delete()

          // Console.println(result) => PrintStream.println: scheme text + platform line separator.
          val expectedStdout = expectedScheme + System.lineSeparator()

          // Assertion (1): captured global stdout is EXACTLY the renderer output — no extra lines.
          assert(
            captured == expectedStdout,
            s"stdout purity violated for $fixturePkg@$fixtureVersion.\n---CAPTURED (${captured.length} chars)---\n$captured\n---EXPECTED (${expectedStdout.length} chars)---\n$expectedStdout",
          )
          // Redundant explicit guards documenting what "pure" means (subsumed by exact equality).
          assert(!captured.contains("Inputs:"), s"'Inputs:' log line leaked into stdout:\n$captured")
          assert(!captured.contains("Scheme written to"), s"'Scheme written to' log line leaked into stdout:\n$captured")

          // Assertion (2): no file written by the stdout seam.
          assert(
            probeBefore == probeAfter && probeAfter.isEmpty,
            s"stdout seam wrote a file: before=$probeBefore after=$probeAfter",
          )
        }
    }
  }
}
