package io.septimalmind.baboon.tests

import distage.plugins.PluginBase
import io.septimalmind.baboon.*
import io.septimalmind.baboon.CompilerTarget.RsTarget
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.translator.BaboonAbstractTranslator
import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.distage.plugins.PluginConfig
import izumi.distage.testkit.model.TestConfig
import izumi.functional.bio.Error2
import izumi.functional.bio.unsafe.UnsafeInstances
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzResources
import izumi.reflect.TagKK

/** BAB-R04 regression: structs whose float lives inside a generic constructor
  * (`opt[f64]`, `lst[f64]`, `map[…, f64]`) or transitively inside another
  * float-bearing struct must NOT emit manual Eq/Ord impls and must NOT call
  * `.total_cmp()`. The previous emitter produced
  * `self.optLat.total_cmp(&other.optLat)` on `Option<f64>` fields, which fails
  * to compile (`total_cmp` is a method of `f64`, not `Option<f64>`).
  *
  * Bare-float fields (a field whose static type is exactly `f32`/`f64`) keep
  * the manual `f64::total_cmp` Eq/Ord impl path so cross-version pkg0 fixtures
  * relying on `set[FloatBearingStruct]` continue to compile. NaN-soundness is
  * a known follow-up — out of scope here.
  */
final class M31BabR04RustEmissionTest extends M31BabR04RustEmissionTestBase[Either]

abstract class M31BabR04RustEmissionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val rsTarget: RsTarget = RsTarget(
    id = "Rust",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-scalatests-rs-m31/")),
      fixturesOutput         = None,
      testsOutput            = None,
    ),
    generic = GenericOptions(
      codecTestIterations = 0
    ),
    language = RsOptions(
      writeEvolutionDict          = false,
      wrappedAdtBranchCodecs      = false,
      generateJsonCodecs          = true,
      generateUebaCodecs          = true,
      generateJsonCodecsByDefault = true,
      generateUebaCodecsByDefault = true,
      serviceResult               = ServiceResultConfig.rustDefault,
      serviceContext              = ServiceContextConfig.default,
      pragmas                     = Map.empty,
      generateDomainFacade        = false,
      asyncServices               = false,
      cratePrefix                 = "crate",
      reexportMode                = "selective",
      edition                     = "2021",
    ),
  )

  private val baseModule: distage.Module =
    new BaboonModuleJvm[Either](
      CompilerOptions(
        debug                    = false,
        individualInputs         = Set.empty,
        directoryInputs          = Set(FSPath.parse(NEString.unsafeFrom("./baboon-compiler/src/test/resources/baboon"))),
        metaWriteEvolutionJsonTo = None,
        lockFile                 = Some(FSPath.parse(NEString.unsafeFrom("./target/baboon.lock"))),
        emitOnly                 = None,
        targets                  = Seq(rsTarget),
      ),
      UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
    )

  private val translatorModule: distage.Module = new BaboonJvmRsModule[Either](rsTarget)
  private val combinedModule: distage.Module   = baseModule overriddenBy translatorModule

  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(combinedModule.morph[PluginBase]),
    activation   = super.config.activation + BaboonModeAxis.Compiler,
  )

  private def loadFamily(loader: BaboonLoader[F]): F[NEList[BaboonIssue], BaboonFamily] = {
    val root = IzResources
      .getPath("baboon/m31-bab-r04/m31_bab_r04.baboon")
      .getOrElse(throw new AssertionError("m31-bab-r04 fixture not found"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons =
      if (root.toFile.isDirectory)
        IzFiles.walk(root.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
      else List(root)
    loader.load(baboons)
  }

  "m31-bab-r04 fixture, Rust target" should {

    "NOT emit impl Eq / impl Ord / total_cmp for wrapped-float structs (BAB-R04)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          def find(suffix: String): String =
            all.collectFirst {
              case (path, content) if path.endsWith(suffix) => content
            }.getOrElse(fail(s"$suffix not found. Paths: ${all.map(_._1)}"))

          val bare    = find("bare_float.rs")
          val wrapped = find("wrapped_float.rs")
          val coll    = find("coll_float.rs")
          val noFlt   = find("no_floats.rs")

          // (1) Wrapped-float / collection-of-float structs must NOT call total_cmp anywhere —
          //     `total_cmp` is a method of `f64` and does not exist on `Option<f64>`/`Vec<f64>`/etc.
          //     (BAB-R04 — the original failure was `self.optLat.total_cmp(&other.optLat)`.)
          for ((label, src) <- Seq(("wrapped", wrapped), ("coll", coll))) {
            assert(
              !src.contains("total_cmp"),
              s"[$label] must NOT contain `total_cmp` (BAB-R04). Source:\n$src",
            )
            assert(
              !src.contains("impl Eq for"),
              s"[$label] must NOT emit `impl Eq for` (BAB-R04). Source:\n$src",
            )
            assert(
              !src.contains("impl Ord for"),
              s"[$label] must NOT emit `impl Ord for` (BAB-R04). Source:\n$src",
            )
            // Wrapped-float structs MUST derive PartialEq/PartialOrd (sound).
            assert(
              src.contains("PartialEq") && src.contains("PartialOrd"),
              s"[$label] must derive PartialEq + PartialOrd. Source:\n$src",
            )
            // Derive line MUST NOT include `Eq,` or `Ord,` tokens.
            val deriveLine = src.linesIterator.find(_.contains("#[derive(")).getOrElse("")
            assert(
              !deriveLine.contains(" Eq,") && !deriveLine.endsWith(" Eq)"),
              s"[$label] derive line must NOT include Eq. Line: $deriveLine",
            )
            assert(
              !deriveLine.contains(" Ord,") && !deriveLine.endsWith(" Ord)"),
              s"[$label] derive line must NOT include Ord. Line: $deriveLine",
            )
          }

          // (2) Bare-float struct keeps the back-compat path: manual Eq/Ord via `f64::total_cmp`
          //     so it remains usable in `BTreeSet`/`BTreeMap`. Pkg0 cross-version fixtures rely
          //     on this.
          assert(
            bare.contains("impl Eq for BareFloat"),
            s"[bare] must keep manual `impl Eq for BareFloat`. Source:\n$bare",
          )
          assert(
            bare.contains("impl Ord for BareFloat"),
            s"[bare] must keep manual `impl Ord for BareFloat`. Source:\n$bare",
          )
          assert(
            bare.contains(".total_cmp("),
            s"[bare] must keep `total_cmp` for f64 fields (back-compat). Source:\n$bare",
          )

          // (3) Non-float struct keeps the full Eq/Ord derive set — regression guard
          //     so we don't accidentally drop Eq/Ord for structs without floats.
          val noFltDerive = noFlt.linesIterator.find(_.contains("#[derive(")).getOrElse("")
          assert(
            noFltDerive.contains("Eq") && noFltDerive.contains("Ord"),
            s"[no_floats] must keep Eq + Ord derives. Line: $noFltDerive",
          )
        }
    }
  }
}
