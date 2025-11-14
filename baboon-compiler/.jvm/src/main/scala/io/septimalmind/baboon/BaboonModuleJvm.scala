package io.septimalmind.baboon

import distage.ModuleDef
import io.septimalmind.baboon.CompilerTarget.{CSTarget, ScTarget}
import io.septimalmind.baboon.parser.{BaboonInclusionResolver, BaboonInclusionResolverImpl}
import io.septimalmind.baboon.util.BLogger
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

class BaboonModuleJvm[F[+_, +_]: Error2: MaybeSuspend2: TagKK](
  options: CompilerOptions,
  parallelAccumulatingOps2: ParallelErrorAccumulatingOps2[F],
) extends ModuleDef {
  include(new BaboonModuleLogicModule[F](parallelAccumulatingOps2))

  make[CompilerOptions].fromValue(options)

//  make[Seq[Path]].named("inputs").fromValue(inputs)

  make[BLogger].from[BLogger.BLoggerImpl]

  make[BaboonLoader[F]].from[BaboonLoader.BaboonLoaderImpl[F]]
  make[BaboonInclusionResolver[F]].from[BaboonInclusionResolverImpl[F]]

}
