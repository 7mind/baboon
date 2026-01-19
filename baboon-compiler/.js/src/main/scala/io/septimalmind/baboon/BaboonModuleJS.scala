package io.septimalmind.baboon

import distage.{ModuleDef, TagKK}
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.{BaboonInclusionResolver, BaboonInclusionResolverMapImpl, BaboonParser}
import io.septimalmind.baboon.util.{BLogger, FileContentProvider}
import izumi.functional.bio.unsafe.MaybeSuspend2
import izumi.functional.bio.{Error2, ParallelErrorAccumulatingOps2}

final class FileContentProviderFromInputs(inputs: Seq[BaboonParser.Input]) extends FileContentProvider {
  private val contentsByPath: Map[FSPath, String] =
    inputs.map(input => input.path -> input.content).toMap

  override def read(path: FSPath): Option[String] = {
    contentsByPath.get(path)
  }
}

final class FileContentProviderUnsupported extends FileContentProvider {
  override def read(path: FSPath): Option[String] = {
    throw new IllegalStateException(s"FileContentProvider is unavailable without inputs: ${path.asString}")
  }
}

class BaboonModuleJS[F[+_, +_]: Error2: MaybeSuspend2: TagKK](
  inputs: Seq[BaboonParser.Input],
  logger: BLogger,
  parOps: ParallelErrorAccumulatingOps2[F],
  compilerOptions: CompilerOptions,
) extends ModuleDef {

  include(new BaboonModuleLogicModule[F](parOps))

  make[Seq[BaboonParser.Input]].fromValue(inputs)
  make[CompilerOptions].fromValue(compilerOptions)
  make[FileContentProvider].fromValue(new FileContentProviderFromInputs(inputs))

  make[BaboonInclusionResolver[F]].from[BaboonInclusionResolverMapImpl[F]]
  make[BaboonLoaderJS[F]].from[BaboonLoaderJS.BaboonLoaderJSImpl[F]]
  make[BLogger].fromValue(logger)

}

class BaboonJsCSModule[F[+_, +_]: Error2: TagKK](compilerTarget: CompilerTarget.CSTarget) extends ModuleDef {
  include(new BaboonCommonCSModule[F]())
  make[CompilerTarget.CSTarget].fromValue(compilerTarget)
}

class BaboonJsScModule[F[+_, +_]: Error2: TagKK](compilerTarget: CompilerTarget.ScTarget) extends ModuleDef {
  include(new BaboonCommonScModule[F]())
  make[CompilerTarget.ScTarget].fromValue(compilerTarget)
}

class BaboonCodecModuleJS[F[+_, +_]: Error2: MaybeSuspend2: TagKK](
  parOps: ParallelErrorAccumulatingOps2[F]
) extends ModuleDef {
  include(new BaboonModuleLogicModule[F](parOps))
  make[FileContentProvider].fromValue(new FileContentProviderUnsupported)
}
