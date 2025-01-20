package io.septimalmind.baboon

import distage.{DIKey, ModuleDef}
import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.translator.BaboonAbstractTranslator
import io.septimalmind.baboon.translator.csharp.*
import io.septimalmind.baboon.translator.csharp.CSCodecFixtureTranslator.CSRandomMethodTranslatorImpl
import io.septimalmind.baboon.translator.csharp.CSValue.CSPackageId
import io.septimalmind.baboon.typer.*
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Scope.NestedScope
import io.septimalmind.baboon.util.{BLogger, BaboonMetagen}
import io.septimalmind.baboon.validator.BaboonValidator
import izumi.functional.bio.{Applicative2, ApplicativeError2, Bifunctor2, Error2, Guarantee2, Monad2}
import izumi.reflect.TagKK

import java.nio.file.Path

class BaboonModule[F[+_, +_]: Error2: TagKK](
  options: CompilerOptions,
  inputs: Seq[Path],
) extends ModuleDef {
  make[CompilerOptions].fromValue(options)
  make[Seq[Path]].named("inputs").fromValue(inputs)
  addImplicit[Error2[F]]
    .aliased[Monad2[F]]
    .aliased[Applicative2[F]]
    .aliased[ApplicativeError2[F]]
    .aliased[Guarantee2[F]]
    .aliased[Bifunctor2[F]]

  make[BLogger].from[BLogger.BLoggerImpl]

  make[BaboonCompiler[F]].from[BaboonCompiler.BaboonCompilerImpl[F]]
  make[BaboonLoader[F]].from[BaboonLoader.BaboonLoaderImpl[F]]
  make[BaboonFamilyManager[F]].from[BaboonFamilyManager.BaboonFamilyManagerImpl[F]]
  make[BaboonValidator[F]].from[BaboonValidator.BaboonValidatorImpl[F]]
  make[BaboonRules[F]].from[BaboonRules.BaboonRulesImpl[F]]
  make[BaboonParser[F]].from[BaboonParser.BaboonParserImpl[F]]
  make[BaboonTyper[F]].from[BaboonTyper.BaboonTyperImpl[F]]
  make[BaboonComparator[F]].from[BaboonComparator.BaboonComparatorImpl[F]]

  make[BaboonEnquiries].from[BaboonEnquiries.BaboonEnquiriesImpl]
  make[BaboonMetagen].from[BaboonMetagen.BaboonMetagenImpl]
  make[TypeInfo].from[TypeInfo.TypeInfoImpl]

  makeSubcontext[BaboonTranslator[F]]
    .localDependencies(
      List(
        DIKey[Pkg],
        DIKey[NestedScope[ExtendedRawDefn]],
        DIKey[Map[TypeId, DomainMember]],
      )
    )
    .withSubmodule(new ModuleDef {
      make[BaboonTranslator[F]]
    })

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

  make[ScopeSupport[F]].from[ScopeSupport.ScopeSupportImpl[F]]

  many[BaboonAbstractTranslator[F]]
    .ref[CSBaboonTranslator[F]]

  make[CSBaboonTranslator[F]]

  make[CSTreeTools].from[CSTreeTools.CSTreeToolsImpl]
  make[CSFileTools].from[CSFileTools.CSFileToolsImpl]
  make[CSTypeTranslator]

  makeSubcontext[CSConversionTranslator[F]]
    .localDependencies(
      List(
        DIKey[CSPackageId],
        DIKey[Version],
        DIKey.get[Domain].named("current"),
        DIKey.get[Domain].named("source"),
        DIKey.get[BaboonRuleset],
        DIKey.get[BaboonEvolution],
      )
    )
    .withSubmodule(new ModuleDef {
      make[CSConversionTranslator[F]]
    })

}
