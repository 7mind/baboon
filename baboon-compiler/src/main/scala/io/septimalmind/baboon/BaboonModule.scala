package io.septimalmind.baboon

import distage.DIKey
import distage.ModuleDef
import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.scheme.BaboonSchemeRenderer
import io.septimalmind.baboon.translator.BaboonAbstractTranslator
import io.septimalmind.baboon.translator.csharp.*
import io.septimalmind.baboon.translator.csharp.CSCodecFixtureTranslator.CSRandomMethodTranslatorImpl
import io.septimalmind.baboon.translator.python.*
import io.septimalmind.baboon.translator.python.PyDefnTranslator.PyDefnTranslatorImpl
import io.septimalmind.baboon.translator.rust.*
import io.septimalmind.baboon.translator.scl.*
import io.septimalmind.baboon.translator.dart.*
import io.septimalmind.baboon.translator.java.*
import io.septimalmind.baboon.translator.kotlin.*
import io.septimalmind.baboon.translator.swift.*
import io.septimalmind.baboon.translator.typescript.*
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
  make[RootExtractor].from[RootExtractor.DeclaredRootExtractor].tagged(BaboonModeAxis.Compiler)
  make[RootExtractor].from[RootExtractor.DeclaredRootExtractor].tagged(BaboonModeAxis.Explorer)
  make[BaboonComparator[F]].from[BaboonComparator.BaboonComparatorImpl[F]]

  make[BaboonEnquiries].from[BaboonEnquiries.BaboonEnquiriesImpl]
  make[BaboonSchemeRenderer].from[BaboonSchemeRenderer.BaboonSchemeRendererImpl]
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
      make[CSServiceWiringTranslator].from[CSServiceWiringTranslator.Impl]
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
      make[ScServiceWiringTranslator].from[ScServiceWiringTranslator.Impl]
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
      make[PyServiceWiringTranslator].from[PyServiceWiringTranslator.Impl]
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

class BaboonCommonRsModule[F[+_, +_]: Error2: TagKK] extends ModuleDef {
  include(new SharedTranspilerModule[F])

  makeSubcontext[RsDefnTranslator[F]]
    .localDependencies(List(DIKey[Domain], DIKey[BaboonEvolution]))
    .withSubmodule(new ModuleDef {
      make[RsDefnTranslator[F]].from[RsDefnTranslator.RsDefnTranslatorImpl[F]]
      make[RsCodecFixtureTranslator].from[RsCodecFixtureTranslator.RsCodecFixtureTranslatorImpl]
      make[RsCodecTestsTranslator].from[RsCodecTestsTranslator.Impl]
      make[RsServiceWiringTranslator].from[RsServiceWiringTranslator.Impl]
      many[RsCodecTranslator]
        .add[RsJsonCodecGenerator]
        .add[RsUEBACodecGenerator]
    })

  make[RsFileTools].from[RsFileTools.RsFileToolsImpl]
  make[RsTreeTools].from[RsTreeTools.RsTreeToolsImpl]

  make[RsTypeTranslator]
  makeFactory[RsConversionTranslator.Factory[F]]

  make[RsBaboonTranslator[F]].aliased[BaboonAbstractTranslator[F]]
  many[BaboonAbstractTranslator[F]]
    .ref[RsBaboonTranslator[F]]
}

class BaboonCommonTsModule[F[+_, +_]: Error2: TagKK] extends ModuleDef {
  include(new SharedTranspilerModule[F])

  makeSubcontext[TsDefnTranslator[F]]
    .localDependencies(List(DIKey[Domain], DIKey[BaboonEvolution]))
    .withSubmodule(new ModuleDef {
      make[TsDefnTranslator[F]].from[TsDefnTranslator.TsDefnTranslatorImpl[F]]
      make[TsServiceWiringTranslator].from[TsServiceWiringTranslator.Impl]
      make[TsCodecFixtureTranslator].from[TsCodecFixtureTranslator.TsCodecFixtureTranslatorImpl]
      make[TsCodecTestsTranslator].from[TsCodecTestsTranslator.Impl]
      many[TsCodecTranslator]
        .add[TsJsonCodecGenerator]
        .add[TsUEBACodecGenerator]
    })

  make[TsFileTools].from[TsFileTools.TsFileToolsImpl]
  make[TsTreeTools].from[TsTreeTools.TsTreeToolsImpl]

  make[TsTypeTranslator]
  makeFactory[TsConversionTranslator.Factory[F]]

  make[TsBaboonTranslator[F]].aliased[BaboonAbstractTranslator[F]]
  many[BaboonAbstractTranslator[F]]
    .ref[TsBaboonTranslator[F]]
}

class BaboonCommonKtModule[F[+_, +_]: Error2: TagKK] extends ModuleDef {
  include(new SharedTranspilerModule[F])

  makeSubcontext[KtDefnTranslator[F]]
    .localDependencies(List(DIKey[Domain], DIKey[BaboonEvolution]))
    .withSubmodule(new ModuleDef {
      make[KtDomainTreeTools].from[KtDomainTreeTools.KtDomainTreeToolsImpl]
      make[KtDefnTranslator[F]].from[KtDefnTranslator.KtDefnTranslatorImpl[F]]
      make[KtCodecFixtureTranslator].from[KtCodecFixtureTranslator.Impl]
      make[KtCodecTestsTranslator].from[KtCodecTestsTranslator.Impl]
      make[KtServiceWiringTranslator].from[KtServiceWiringTranslator.Impl]
      many[KtCodecTranslator]
        .add[KtJsonCodecGenerator]
        .add[KtUEBACodecGenerator]
    })

  make[KtFileTools].from[KtFileTools.KtFileToolsImpl]
  make[KtTreeTools].from[KtTreeTools.KtTreeToolsImpl]

  make[KtTypeTranslator]
  makeFactory[KtConversionTranslator.Factory[F]]

  make[KtBaboonTranslator[F]].aliased[BaboonAbstractTranslator[F]]
  many[BaboonAbstractTranslator[F]]
    .ref[KtBaboonTranslator[F]]
}

class BaboonCommonJvModule[F[+_, +_]: Error2: TagKK] extends ModuleDef {
  include(new SharedTranspilerModule[F])

  makeSubcontext[JvDefnTranslator[F]]
    .localDependencies(List(DIKey[Domain], DIKey[BaboonEvolution]))
    .withSubmodule(new ModuleDef {
      make[JvDomainTreeTools].from[JvDomainTreeTools.JvDomainTreeToolsImpl]
      make[JvDefnTranslator[F]].from[JvDefnTranslator.JvDefnTranslatorImpl[F]]
      make[JvCodecFixtureTranslator].from[JvCodecFixtureTranslator.Impl]
      make[JvCodecTestsTranslator].from[JvCodecTestsTranslator.Impl]
      make[JvServiceWiringTranslator].from[JvServiceWiringTranslator.Impl]
      many[JvCodecTranslator]
        .add[JvJsonCodecGenerator]
        .add[JvUEBACodecGenerator]
    })

  make[JvFileTools].from[JvFileTools.JvFileToolsImpl]
  make[JvTreeTools].from[JvTreeTools.JvTreeToolsImpl]

  make[JvTypeTranslator]
  makeFactory[JvConversionTranslator.Factory[F]]

  make[JvBaboonTranslator[F]].aliased[BaboonAbstractTranslator[F]]
  many[BaboonAbstractTranslator[F]]
    .ref[JvBaboonTranslator[F]]
}

class BaboonCommonDtModule[F[+_, +_]: Error2: TagKK] extends ModuleDef {
  include(new SharedTranspilerModule[F])

  makeSubcontext[DtDefnTranslator[F]]
    .localDependencies(List(DIKey[Domain], DIKey[BaboonEvolution]))
    .withSubmodule(new ModuleDef {
      make[DtDomainTreeTools].from[DtDomainTreeTools.DtDomainTreeToolsImpl]
      make[DtDefnTranslator[F]].from[DtDefnTranslator.DtDefnTranslatorImpl[F]]
      make[DtCodecFixtureTranslator].from[DtCodecFixtureTranslator.Impl]
      make[DtCodecTestsTranslator].from[DtCodecTestsTranslator.Impl]
      make[DtServiceWiringTranslator].from[DtServiceWiringTranslator.Impl]
      many[DtCodecTranslator]
        .add[DtJsonCodecGenerator]
        .add[DtUEBACodecGenerator]
    })

  make[DtFileTools].from[DtFileTools.DtFileToolsImpl]
  make[DtTreeTools].from[DtTreeTools.DtTreeToolsImpl]

  make[DtTypeTranslator]
  makeFactory[DtConversionTranslator.Factory[F]]

  make[DtBaboonTranslator[F]].aliased[BaboonAbstractTranslator[F]]
  many[BaboonAbstractTranslator[F]]
    .ref[DtBaboonTranslator[F]]
}

class BaboonCommonSwModule[F[+_, +_]: Error2: TagKK] extends ModuleDef {
  include(new SharedTranspilerModule[F])

  makeSubcontext[SwDefnTranslator[F]]
    .localDependencies(List(DIKey[Domain], DIKey[BaboonEvolution]))
    .withSubmodule(new ModuleDef {
      make[SwDomainTreeTools].from[SwDomainTreeTools.SwDomainTreeToolsImpl]
      make[SwDefnTranslator[F]].from[SwDefnTranslator.SwDefnTranslatorImpl[F]]
      make[SwCodecFixtureTranslator].from[SwCodecFixtureTranslator.Impl]
      make[SwCodecTestsTranslator].from[SwCodecTestsTranslator.Impl]
      make[SwServiceWiringTranslator].from[SwServiceWiringTranslator.Impl]
      many[SwCodecTranslator]
        .add[SwJsonCodecGenerator]
        .add[SwUEBACodecGenerator]
    })

  make[SwFileTools].from[SwFileTools.SwFileToolsImpl]
  make[SwTreeTools].from[SwTreeTools.SwTreeToolsImpl]

  make[SwTypeTranslator]
  makeFactory[SwConversionTranslator.Factory[F]]

  make[SwBaboonTranslator[F]].aliased[BaboonAbstractTranslator[F]]
  many[BaboonAbstractTranslator[F]]
    .ref[SwBaboonTranslator[F]]
}
