package io.septimalmind.baboon

import distage.{ModuleDef, TagKK}
import io.septimalmind.baboon.CompilerTargetJS.{CSTarget, ScTarget}
import io.septimalmind.baboon.parser.{BaboonInclusionResolver, BaboonInclusionResolverMapImpl, BaboonParser}
import io.septimalmind.baboon.util.BLogger
import izumi.functional.bio.unsafe.MaybeSuspend2
import izumi.functional.bio.{Error2, ParallelErrorAccumulatingOps2}

class BaboonModuleJS[F[+_, +_]: Error2: MaybeSuspend2: TagKK](
  inputs: Seq[BaboonParser.Input],
  logger: BLogger,
  parOps: ParallelErrorAccumulatingOps2[F],
) extends ModuleDef {

  include(new BaboonModuleLogicModule[F](parOps))

  make[Seq[BaboonParser.Input]].fromValue(inputs)

  make[BaboonInclusionResolver[F]].from[BaboonInclusionResolverMapImpl[F]]
  make[BaboonLoaderJS[F]].from[BaboonLoaderJS.BaboonLoaderJSImpl[F]]
  make[BLogger].fromValue(logger)

}

class BaboonJsCSModule[F[+_, +_]: Error2: TagKK](target: CSTarget) extends ModuleDef {
  include(new BaboonCommonCSModule[F]())
  make[CSTarget].fromValue(target)
}

class BaboonJsScModule[F[+_, +_]: Error2: TagKK](target: ScTarget) extends ModuleDef {
  include(new BaboonCommonScModule[F]())
  make[ScTarget].fromValue(target)
}
