package io.septimalmind.baboon

import distage.{DIKey, ModuleDef}
import io.septimalmind.baboon.CompilerTarget.{CSTarget, DtTarget, JvTarget, KtTarget, PyTarget, RsTarget, ScTarget, TsTarget}
import io.septimalmind.baboon.explore.{ExploreContext, ExploreInputs}
import io.septimalmind.baboon.parser.{BaboonInclusionResolver, BaboonInclusionResolverImpl}
import io.septimalmind.baboon.typer.model.BaboonFamily
import io.septimalmind.baboon.util.BLogger
import io.septimalmind.baboon.util.{FileContentProvider, JvmFileContentProvider}
import izumi.functional.bio.unsafe.MaybeSuspend2
import izumi.functional.bio.{Error2, ParallelErrorAccumulatingOps2}
import izumi.reflect.TagKK

class SharedTranspilerJvmModule[F[+_, +_]: TagKK](implicit @annotation.unused evidence: Error2[F]) extends ModuleDef {
  make[BaboonCompiler[F]].from[BaboonCompiler.BaboonCompilerImpl[F]]
  make[LockfileManager[F]].from[LockfileManagerImpl[F]]
}

class BaboonJvmCSModule[F[+_, +_]: Error2: TagKK](target: CSTarget) extends ModuleDef {
  include(new SharedTranspilerJvmModule[F]())
  include(new BaboonCommonCSModule[F]())
  make[CSTarget].fromValue(target)
}

class BaboonJvmScModule[F[+_, +_]: Error2: TagKK](target: ScTarget) extends ModuleDef {
  include(new SharedTranspilerJvmModule[F]())
  include(new BaboonCommonScModule[F]())
  make[ScTarget].fromValue(target)
}

class BaboonJvmPyModule[F[+_, +_]: Error2: TagKK](target: PyTarget) extends ModuleDef {
  include(new SharedTranspilerJvmModule[F]())
  include(new BaboonCommonPyModule[F]())
  make[PyTarget].fromValue(target)
}

class BaboonJvmRsModule[F[+_, +_]: Error2: TagKK](target: RsTarget) extends ModuleDef {
  include(new SharedTranspilerJvmModule[F]())
  include(new BaboonCommonRsModule[F]())
  make[RsTarget].fromValue(target)
}

class BaboonJvmTsModule[F[+_, +_]: Error2: TagKK](target: TsTarget) extends ModuleDef {
  include(new SharedTranspilerJvmModule[F]())
  include(new BaboonCommonTsModule[F]())
  make[TsTarget].fromValue(target)
}

class BaboonJvmKtModule[F[+_, +_]: Error2: TagKK](target: KtTarget) extends ModuleDef {
  include(new SharedTranspilerJvmModule[F]())
  include(new BaboonCommonKtModule[F]())
  make[KtTarget].fromValue(target)
}

class BaboonJvmJvModule[F[+_, +_]: Error2: TagKK](target: JvTarget) extends ModuleDef {
  include(new SharedTranspilerJvmModule[F]())
  include(new BaboonCommonJvModule[F]())
  make[JvTarget].fromValue(target)
}

class BaboonJvmDtModule[F[+_, +_]: Error2: TagKK](target: DtTarget) extends ModuleDef {
  include(new SharedTranspilerJvmModule[F]())
  include(new BaboonCommonDtModule[F]())
  make[DtTarget].fromValue(target)
}

class BaboonModuleJvm[F[+_, +_]: Error2: MaybeSuspend2: TagKK](
  options: CompilerOptions,
  parallelAccumulatingOps2: ParallelErrorAccumulatingOps2[F],
) extends ModuleDef {
  include(new BaboonModuleLogicModule[F](parallelAccumulatingOps2))

  make[CompilerOptions].fromValue(options)

//  make[Seq[Path]].named("inputs").fromValue(inputs)

  make[BLogger].from[BLogger.BLoggerImpl].tagged(BaboonModeAxis.Compiler)
  make[BLogger].from[BLogger.BLoggerErrImpl].tagged(BaboonModeAxis.Lsp)
  make[BLogger].fromValue(BLogger.Noop).tagged(BaboonModeAxis.Explorer)

  make[FileContentProvider].from[JvmFileContentProvider]

  make[BaboonLoader[F]].from[BaboonLoader.BaboonLoaderImpl[F]]
  make[BaboonInclusionResolver[F]].from[BaboonInclusionResolverImpl[F]]

  makeSubcontext[ExploreContext[F]]
    .localDependencies(List(DIKey[BaboonFamily], DIKey[ExploreInputs]))
    .withSubmodule(new ModuleDef {
      make[ExploreContext[F]]
    })

}
