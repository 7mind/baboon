package io.septimalmind.baboon

import distage.{DIKey, ModuleDef}
import io.septimalmind.baboon.CompilerTarget.{CSTarget, ScTarget}
import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.translator.BaboonAbstractTranslator
import io.septimalmind.baboon.translator.csharp.*
import io.septimalmind.baboon.translator.csharp.CSCodecFixtureTranslator.CSRandomMethodTranslatorImpl
import io.septimalmind.baboon.translator.scl.{ScBaboonTranslator, ScDefnTranslator}
import io.septimalmind.baboon.typer.*
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.util.functional.ParallelAccumulatingOps2
import io.septimalmind.baboon.util.{BLogger, BaboonMetagen}
import io.septimalmind.baboon.validator.BaboonValidator
import izumi.functional.bio.{Applicative2, ApplicativeError2, Bifunctor2, Error2, Guarantee2, Monad2}
import izumi.reflect.TagKK

class BaboonSharedModule[F[+_, +_]: Error2: TagKK] extends ModuleDef {
  // not all the definitions are required for parent locator, so it's easier to double-include
  addImplicit[Error2[F]].exposed
    .aliased[Monad2[F]].exposed
    .aliased[Applicative2[F]].exposed
    .aliased[ApplicativeError2[F]].exposed
    .aliased[Guarantee2[F]].exposed
    .aliased[Bifunctor2[F]].exposed

}

class BaboonModule[F[+_, +_]: Error2: TagKK](
  options: CompilerOptions,
  parallelAccumulatingOps2: ParallelAccumulatingOps2[F],
) extends ModuleDef {
  include(new BaboonSharedModule[F])
  make[CompilerOptions].fromValue(options)

//  make[Seq[Path]].named("inputs").fromValue(inputs)

  make[ParallelAccumulatingOps2[F]].fromValue(parallelAccumulatingOps2).exposed

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
    .localDependencies(List(DIKey[Domain], DIKey[BaboonEvolution]))
    .withSubmodule(new ModuleDef {
      make[CSDefnTranslator[F]].from[CSDefnTranslator.CSDefnTranslatorImpl[F]]
      make[CSCodecTestsTranslator].from[CSCodecTestsTranslator.Impl]
      make[CSCodecFixtureTranslator].from[CSRandomMethodTranslatorImpl]
      make[CSDomainTreeTools].from[CSDomainTreeTools.CSDomainTreeToolsImpl]
      many[CSCodecTranslator]
        .add[CSNSJsonCodecGenerator]
        .add[CSUEBACodecGenerator]
    })

  many[BaboonAbstractTranslator[F]]
    .ref[CSBaboonTranslator[F]]

  make[CSBaboonTranslator[F]].aliased[BaboonAbstractTranslator[F]]

  make[CSTreeTools].from[CSTreeTools.CSTreeToolsImpl]
  make[CSFileTools].from[CSFileTools.CSFileToolsImpl]
  make[CSTypeTranslator]

  makeFactory[CSConversionTranslator.Factory[F]]
}

class BaboonScModule[F[+_, +_]: Error2: TagKK](target: ScTarget) extends ModuleDef {
  include(new SharedTranspilerModule[F])

  make[ScTarget].fromValue(target)

  makeSubcontext[ScDefnTranslator[F]]
    .localDependencies(List(DIKey[Domain], DIKey[BaboonEvolution]))
    .withSubmodule(new ModuleDef {
      make[ScDefnTranslator[F]].from[ScDefnTranslator.CSDefnTranslatorImpl[F]]

    })

  make[ScBaboonTranslator[F]].aliased[BaboonAbstractTranslator[F]]

}
