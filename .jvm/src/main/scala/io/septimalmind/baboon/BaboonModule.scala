package io.septimalmind.baboon

import distage.{DIKey, ModuleDef}
import io.septimalmind.baboon.CompilerTarget.{CSTarget, ScTarget}
import io.septimalmind.baboon.parser.{BaboonInclusionResolver, BaboonInclusionResolverImpl, BaboonParser}
import io.septimalmind.baboon.translator.BaboonAbstractTranslator
import io.septimalmind.baboon.translator.csharp.*
import io.septimalmind.baboon.translator.csharp.CSCodecFixtureTranslator.CSRandomMethodTranslatorImpl
import io.septimalmind.baboon.translator.scl.*
import io.septimalmind.baboon.typer.*
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.util.{BLogger, BaboonMetagen}
import io.septimalmind.baboon.validator.BaboonValidator
import izumi.functional.bio.unsafe.MaybeSuspend2
import izumi.functional.bio.{Applicative2, ApplicativeError2, Bifunctor2, Error2, Guarantee2, Monad2, ParallelErrorAccumulatingOps2}
import izumi.reflect.TagKK

class BaboonSharedModule[F[+_, +_]: Error2: MaybeSuspend2: TagKK] extends ModuleDef {
  // not all the definitions are required for parent locator, so it's easier to double-include
  addImplicit[Error2[F]].exposed
    .aliased[Monad2[F]].exposed
    .aliased[Applicative2[F]].exposed
    .aliased[ApplicativeError2[F]].exposed
    .aliased[Guarantee2[F]].exposed
    .aliased[Bifunctor2[F]].exposed
  addImplicit[MaybeSuspend2[F]]
}

class BaboonModule[F[+_, +_]: Error2: MaybeSuspend2: TagKK](
  options: CompilerOptions,
  parallelAccumulatingOps2: ParallelErrorAccumulatingOps2[F],
) extends ModuleDef {
  include(new BaboonSharedModule[F])
  make[CompilerOptions].fromValue(options)

//  make[Seq[Path]].named("inputs").fromValue(inputs)

  make[ParallelErrorAccumulatingOps2[F]].fromValue(parallelAccumulatingOps2).exposed

  make[BLogger].from[BLogger.BLoggerImpl]

  make[BaboonLoader[F]].from[BaboonLoader.BaboonLoaderImpl[F]]
  make[BaboonFamilyManager[F]].from[BaboonFamilyManager.BaboonFamilyManagerImpl[F]]
  make[BaboonValidator[F]].from[BaboonValidator.BaboonValidatorImpl[F]]
  make[BaboonRules[F]].from[BaboonRules.BaboonRulesImpl[F]]
  make[BaboonParser[F]].from[BaboonParser.BaboonParserImpl[F]]
  make[BaboonTyper[F]].from[BaboonTyper.BaboonTyperImpl[F]]
  make[BaboonComparator[F]].from[BaboonComparator.BaboonComparatorImpl[F]]

  make[BaboonEnquiries].from[BaboonEnquiries.BaboonEnquiriesImpl]
  make[TypeInfo].from[TypeInfo.TypeInfoImpl]

  make[ScopeSupport[F]].from[ScopeSupport.ScopeSupportImpl[F]]
  make[LockfileManager[F]].from[LockfileManagerImpl[F]]
  make[BaboonInclusionResolver[F]].from[BaboonInclusionResolverImpl[F]]

  makeFactory[BaboonTranslator.Factory[F]]
}

class SharedTranspilerModule[F[+_, +_]: Error2: TagKK]() extends ModuleDef {
  include(new BaboonSharedModule[F])
  make[BaboonCompiler[F]].from[BaboonCompiler.BaboonCompilerImpl[F]]
  make[BaboonMetagen].from[BaboonMetagen.BaboonMetagenImpl]

}
class BaboonCSModule[F[+_, +_]: Error2: TagKK](target: CSTarget) extends ModuleDef {
  include(new SharedTranspilerModule[F])

  make[CSTarget].fromValue(target)

  makeSubcontext[CSDefnTranslator[F]]
    .localDependencies(List(DIKey[Domain], DIKey[BaboonEvolution], DIKey[BaboonLineage]))
    .withSubmodule(new ModuleDef {
      make[CSDefnTranslator[F]].from[CSDefnTranslator.CSDefnTranslatorImpl[F]]
      make[CSCodecTestsTranslator].from[CSCodecTestsTranslator.Impl]
      make[CSCodecFixtureTranslator].from[CSRandomMethodTranslatorImpl]
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

class BaboonScModule[F[+_, +_]: Error2: TagKK](target: ScTarget) extends ModuleDef {
  include(new SharedTranspilerModule[F])

  make[ScTarget].fromValue(target)

  makeSubcontext[ScDefnTranslator[F]]
    .localDependencies(List(DIKey[Domain], DIKey[BaboonEvolution]))
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
