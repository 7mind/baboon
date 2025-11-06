package io.septimalmind.baboon

import distage.{DIKey, Module, ModuleDef, TagKK}
import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.translator.BaboonAbstractTranslator
import io.septimalmind.baboon.translator.csharp.*
import io.septimalmind.baboon.translator.scl.*
import io.septimalmind.baboon.typer.*
import io.septimalmind.baboon.util.BLogger
import io.septimalmind.baboon.validator.BaboonValidator
import izumi.functional.bio.{Error2, ParallelErrorAccumulatingOps2}
import izumi.functional.bio.unsafe.MaybeSuspend2
import izumi.functional.quasi.QuasiIO

class BaboonSharedModuleJS[F[+_, +_]: Error2: MaybeSuspend2: TagKK] extends ModuleDef {
  addImplicit[Error2[F]].exposed
  addImplicit[MaybeSuspend2[F]]
}

class BaboonModuleJS[F[+_, +_]: TagKK](
  targets: Seq[CompilerTargetJS],
  logger: BLogger,
  parOps: ParallelErrorAccumulatingOps2[F],
)(implicit val F: Error2[F], val MS: MaybeSuspend2[F])
    extends ModuleDef {

  include(new BaboonSharedModuleJS[F])

  make[BaboonParser[F]].from[BaboonParser.BaboonParserImpl[F]]

  make[BaboonComparator[F]].from[BaboonComparator.BaboonComparatorImpl[F]]
  make[BaboonRules[F]].from[BaboonRules.BaboonRulesImpl[F]]
  make[BaboonEnquiries].from[BaboonEnquiries.BaboonEnquiriesImpl]
  make[TypeInfo].from[TypeInfo.TypeInfoImpl]
  make[ScopeSupport[F]].from[ScopeSupport.ScopeSupportImpl[F]]

  make[BaboonFamilyManager[F]].from[BaboonFamilyManager.BaboonFamilyManagerImpl[F]]
  make[BaboonValidator[F]].from[BaboonValidator.BaboonValidatorImpl[F]]
  make[BaboonLoaderJS[F]].from[BaboonLoaderJS.BaboonLoaderJSImpl[F]]

  make[BLogger].fromValue(logger)

  make[Error2[F]].fromValue(F)
  make[MaybeSuspend2[F]].fromValue(MS)
  make[ParallelErrorAccumulatingOps2[F]].fromValue(parOps)
}

class BaboonCSModuleJS[F[+_, +_]: Error2: MaybeSuspend2: TagKK](target: CompilerTargetJS.CSTarget)(implicit val Q: QuasiIO[F[Throwable, _]]) extends ModuleDef {
  include(new BaboonSharedModuleJS[F])

  make[CompilerTargetJS.CSTarget].fromValue(target)

  makeSubcontext[CSDefnTranslator[F]]
    .localDependencies(List(DIKey[typer.model.Domain], DIKey[typer.model.BaboonEvolution], DIKey[typer.model.BaboonLineage]))
    .withSubmodule(new ModuleDef {
      make[CSDefnTranslator[F]].from[CSDefnTranslator.CSDefnTranslatorImpl[F]]
      make[CSCodecTestsTranslator].from[CSCodecTestsTranslator.Impl]
      make[CSCodecFixtureTranslator].from[CSCodecFixtureTranslator.CSRandomMethodTranslatorImpl]
      make[CSDomainTreeTools].from[CSDomainTreeTools.CSDomainTreeToolsImpl]

      many[CSCodecTranslator]
        .add[CSJsonCodecGenerator]
        .add[CSUEBACodecGenerator]
    })

  make[CSBaboonTranslator[F]].aliased[BaboonAbstractTranslator[F]]
  makeFactory[CSConversionTranslator.Factory[F]]
  make[CSTreeTools].from[CSTreeTools.CSTreeToolsImpl]
  make[CSFileTools].from[CSFileTools.CSFileToolsImpl]

  make[CSTypeInfo]
  make[CSTypeTranslator]

  many[BaboonAbstractTranslator[F]]
    .ref[CSBaboonTranslator[F]]
}

class BaboonScModuleJS[F[+_, +_]: Error2: MaybeSuspend2: TagKK](target: CompilerTargetJS.ScTarget)(implicit val Q: QuasiIO[F[Throwable, _]]) extends ModuleDef {
  include(new BaboonSharedModuleJS[F])

  make[CompilerTargetJS.ScTarget].fromValue(target)

  makeSubcontext[ScDefnTranslator[F]]
    .localDependencies(List(DIKey[typer.model.Domain], DIKey[typer.model.BaboonEvolution]))
    .withSubmodule(new ModuleDef {
      make[ScDefnTranslator[F]].from[ScDefnTranslator.ScDefnTranslatorImpl[F]]
      make[ScCodecFixtureTranslator].from[ScCodecFixtureTranslator.ScRandomMethodTranslatorImpl]
      make[ScCodecTestsTranslator].from[ScCodecTestsTranslator.Impl]
      many[ScCodecTranslator]
        .add[ScJsonCodecGenerator]
        .add[ScUEBACodecGenerator]
    })

  make[ScFileTools].from[ScFileTools.ScFileToolsImpl]
  make[ScTreeTools].from[ScTreeTools.ScTreeToolsImpl]

  make[ScTypeTranslator]
  make[ScTypeInfo]
  makeFactory[ScConversionTranslator.Factory[F]]

  make[ScBaboonTranslator[F]].aliased[BaboonAbstractTranslator[F]]
  many[BaboonAbstractTranslator[F]]
    .ref[ScBaboonTranslator[F]]
}
