package io.septimalmind.baboon.lsp

import distage.ModuleDef
import io.septimalmind.baboon.lsp.features._
import io.septimalmind.baboon.lsp.state._
import io.septimalmind.baboon.lsp.util.{JvmPathOps, PathOps, PositionConverter}
import izumi.functional.bio.Error2
import izumi.functional.quasi.QuasiIORunner
import izumi.reflect.TagKK

import java.nio.file.Path

class BaboonLspModuleJvm[F[+_, +_]: Error2: TagKK](
  modelDirs: Set[Path],
  port: Option[Int],
  runner: QuasiIORunner[F[Throwable, _]],
  exitCallback: () => Unit,
) extends ModuleDef {
  make[Set[Path]].fromValue(modelDirs)
  make[PathOps].fromValue(JvmPathOps)
  make[Option[Int]].fromValue(port)
  make[QuasiIORunner[F[Throwable, _]]].fromValue(runner)
  make[() => Unit].fromValue(exitCallback)

  make[LspCompiler].from[JvmBaboonCompiler[F]]
  make[InputProvider].from[JvmInputProvider]

  make[DocumentState]
  make[WorkspaceState]
  make[PositionConverter]

  make[DiagnosticsProvider]
  make[DefinitionProvider]
  make[HoverProvider]
  make[CompletionProvider]
  make[DocumentSymbolProvider]

  make[BaboonLanguageServer]
  make[LspLauncher]
}
