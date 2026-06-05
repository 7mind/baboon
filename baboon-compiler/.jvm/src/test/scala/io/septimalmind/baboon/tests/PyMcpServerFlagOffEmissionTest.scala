package io.septimalmind.baboon.tests

import distage.plugins.PluginBase
import io.septimalmind.baboon.*
import io.septimalmind.baboon.CompilerTarget.PyTarget
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

/** T18 flag-off test: verifies byte-identity invariant — with
  * `--py-generate-mcp-server=false` the Python generator emits NO MCP files
  * (`baboon_mcp_runtime.py`, `McpToolsMcpServer.py`).
  *
  * The class config binds a `PyTarget` with `generateMcpServer=false` so every
  * injected `translator` in this suite uses the flag-off path.
  */
final class PyMcpServerFlagOffEmissionTest extends PyMcpServerFlagOffEmissionTestBase[Either]

abstract class PyMcpServerFlagOffEmissionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val pyTargetOff: PyTarget = PyTarget(
    id = "Python",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-tests-py-mcp-off/")),
      fixturesOutput         = None,
      testsOutput            = None,
    ),
    generic = GenericOptions(
      codecTestIterations = 0
    ),
    language = PyOptions(
      writeEvolutionDict          = false,
      wrappedAdtBranchCodecs      = false,
      generateJsonCodecs          = true,
      generateUebaCodecs          = true,
      generateJsonCodecsByDefault = true,
      generateUebaCodecsByDefault = true,
      enableDeprecatedEncoders    = false,
      generateDomainFacade        = false,
      asyncServices               = false,
      serviceResult               = ServiceResultConfig(
        noErrors   = false,
        resultType = Some("BaboonEither"),
        pattern    = Some("<$error, $success>"),
        hkt        = None,
      ),
      serviceContext    = ServiceContextConfig.default,
      pragmas           = Map.empty,
      generateMcpServer = false,
    ),
  )

  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(
      (new BaboonModuleJvm[Either](
        CompilerOptions(
          debug                    = false,
          individualInputs         = Set.empty,
          directoryInputs          = Set(FSPath.parse(NEString.unsafeFrom("./baboon-compiler/src/test/resources/baboon/mcp-stub-ok"))),
          metaWriteEvolutionJsonTo = None,
          lockFile                 = Some(FSPath.parse(NEString.unsafeFrom("./target/baboon.lock"))),
          emitOnly                 = None,
          targets                  = Seq(pyTargetOff),
        ),
        UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
      ) overriddenBy new BaboonJvmPyModule[Either](pyTargetOff)).morph[PluginBase]
    ),
    activation = super.config.activation + BaboonModeAxis.Compiler,
  )

  private def loadStubFamily(loader: BaboonLoader[F]): F[NEList[BaboonIssue], BaboonFamily] = {
    val root = IzResources
      .getPath("baboon/mcp-stub-ok")
      .getOrElse(throw new AssertionError("mcp-stub-ok fixture not found"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons =
      if (root.toFile.isDirectory)
        IzFiles.walk(root.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
      else List(root)
    loader.load(baboons)
  }

  "Python MCP server generator flag-off (--py-generate-mcp-server=false)" should {

    "emit no MCP files when --py-generate-mcp-server=false" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadStubFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val files = srcs.files
          assert(
            !files.keys.exists(_.endsWith("baboon_mcp_runtime.py")),
            s"flag-off: baboon_mcp_runtime.py must NOT be emitted with --py-generate-mcp-server=false; files=${files.keys}",
          )
          assert(
            !files.keys.exists(_.endsWith("McpToolsMcpServer.py")),
            s"flag-off: McpToolsMcpServer.py must NOT be emitted with --py-generate-mcp-server=false; files=${files.keys}",
          )
        }
    }
  }
}
