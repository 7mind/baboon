package io.septimalmind.baboon

import distage.DIKey
import distage.ModuleDef
import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.translator.BaboonAbstractTranslator
import io.septimalmind.baboon.translator.csharp.*
import io.septimalmind.baboon.translator.csharp.CSCodecFixtureTranslator.CSRandomMethodTranslatorImpl
import io.septimalmind.baboon.translator.python.*
import io.septimalmind.baboon.translator.python.PyDefnTranslator.PyDefnTranslatorImpl
import io.septimalmind.baboon.translator.scl.*
import io.septimalmind.baboon.typer.*
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.util.BaboonMetagen
import io.septimalmind.baboon.validator.BaboonValidator
import izumi.functional.bio.unsafe.MaybeSuspend2
import izumi.functional.bio.Applicative2
import izumi.functional.bio.ApplicativeError2
import izumi.functional.bio.Bifunctor2
import izumi.functional.bio.Error2
import izumi.functional.bio.Guarantee2
import izumi.functional.bio.Monad2
import izumi.functional.bio.ParallelErrorAccumulatingOps2
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

class BaboonModuleLogicModule[F[+_, +_]: Error2: MaybeSuspend2: TagKK](
  parallelAccumulatingOps2: ParallelErrorAccumulatingOps2[F]
) extends ModuleDef {
  include(new BaboonSharedModule[F])
  make[ParallelErrorAccumulatingOps2[F]].fromValue(parallelAccumulatingOps2).exposed

  make[BaboonFamilyManager[F]].from[BaboonFamilyManager.BaboonFamilyManagerImpl[F]]
  make[BaboonValidator[F]].from[BaboonValidator.BaboonValidatorImpl[F]]
  make[BaboonRules[F]].from[BaboonRules.BaboonRulesImpl[F]]
  make[BaboonParser[F]].from[BaboonParser.BaboonParserImpl[F]]
  make[BaboonTyper[F]].from[BaboonTyper.BaboonTyperImpl[F]]
  make[RootExtractor].from[RootExtractor.DeclaredRootExtractor]
  make[BaboonComparator[F]].from[BaboonComparator.BaboonComparatorImpl[F]]

  make[BaboonEnquiries].from[BaboonEnquiries.BaboonEnquiriesImpl]
  make[TypeInfo].from[TypeInfo.TypeInfoImpl]

  make[ScopeSupport[F]].from[ScopeSupport.ScopeSupportImpl[F]]
  make[ComponentParsers[F]].from[ComponentParsers.ComponentParsersImpl[F]]
  makeFactory[BaboonTranslator.Factory[F]]
  make[BaboonRuntimeCodec[F]].from[BaboonRuntimeCodec.BaboonRuntimeCodecImpl[F]]
}

class SharedTranspilerModule[F[+_, +_]: Error2: TagKK] extends ModuleDef {
  include(new BaboonSharedModule[F])
  make[BaboonMetagen].from[BaboonMetagen.BaboonMetagenImpl]
}

class BaboonCommonCSModule[F[+_, +_]: Error2: TagKK] extends ModuleDef {
  include(new SharedTranspilerModule[F])

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

class BaboonCommonScModule[F[+_, +_]: Error2: TagKK] extends ModuleDef {
  include(new SharedTranspilerModule[F])

  makeSubcontext[ScDefnTranslator[F]]
    .localDependencies(List(DIKey[Domain], DIKey[BaboonEvolution]))
    .withSubmodule(new ModuleDef {
      make[ScDomainTreeTools].from[ScDomainTreeTools.ScDomainTreeToolsImpl]
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
  makeFactory[ScConversionTranslator.Factory[F]]

  make[ScBaboonTranslator[F]].aliased[BaboonAbstractTranslator[F]]
  many[BaboonAbstractTranslator[F]]
    .ref[ScBaboonTranslator[F]]
}

class BaboonCommonPyModule[F[+_, +_]: Error2: TagKK] extends ModuleDef {
  include(new SharedTranspilerModule[F])

  makeSubcontext[PyDefnTranslator[F]]
    .localDependencies(List(DIKey[Domain], DIKey[BaboonEvolution]))
    .withSubmodule(new ModuleDef {
      make[PyDefnTranslator[F]].from[PyDefnTranslatorImpl[F]]
      make[PyDomainTreeTools].from[PyDomainTreeTools.PyDomainTreeToolsImpl]
      make[PyCodecFixtureTranslator].from[PyCodecFixtureTranslator.PyCodecFixtureTranslatorImpl]
      make[PyCodecTestTranslator].from[PyCodecTestTranslator.PyCodecTestTranslatorImpl]
      make[PyJsonCodecGenerator]
      many[PyCodecTranslator]
        .add[PyJsonCodecGenerator]
        .add[PyUEBACodecGenerator]
    })

  make[PyFileTools].from[PyFileTools.ScFileToolsImpl]

  make[PyTypeTranslator]
  makeFactory[PyConversionTranslator.Factory[F]]

  make[PyBaboonTranslator[F]].aliased[BaboonAbstractTranslator[F]]
  many[BaboonAbstractTranslator[F]]
    .ref[PyBaboonTranslator[F]]
}
