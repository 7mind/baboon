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

import java.nio.file.Path

class BaboonModule(options: CompilerOptions, inputs: Seq[Path]) extends ModuleDef {
  make[BLogger].from[BLogger.BLoggerImpl]
  make[BaboonCompiler].from[BaboonCompiler.BaboonCompilerImpl]
  make[BaboonLoader].from[BaboonLoader.BaboonLoaderImpl]
  make[CompilerOptions].fromValue(options)
  make[BaboonFamilyManager].from[BaboonFamilyManager.BaboonFamilyManagerImpl]
  make[BaboonValidator].from[BaboonValidator.BaboonValidatorImpl]
  make[BaboonEnquiries].from[BaboonEnquiries.BaboonEnquiriesImpl]
  make[BaboonRules].from[BaboonRules.BaboonRulesImpl]
  make[BaboonParser].from[BaboonParser.BaboonParserImpl]
  make[BaboonTyper].from[BaboonTyper.BaboonTyperImpl]
  make[BaboonComparator].from[BaboonComparator.BaboonComparatorImpl]
  make[BaboonMetagen].from[BaboonMetagen.BaboonMetagenImpl]
  make[TypeInfo].from[TypeInfo.TypeInfoImpl]

  makeSubcontext[BaboonTranslator]
    .localDependencies(
      List(
        DIKey[Pkg],
        DIKey[NestedScope[ExtendedRawDefn]],
        DIKey[Map[TypeId, DomainMember]],
      )
    )
    .withSubmodule(new ModuleDef {
      make[BaboonTranslator]
    })

  makeSubcontext[CSDefnTranslator]
    .localDependencies(List(DIKey[Domain], DIKey[BaboonEvolution]))
    .withSubmodule(new ModuleDef {
      make[CSDefnTranslator].from[CSDefnTranslator.CSDefnTranslatorImpl]
      make[CSCodecTestsTranslator].from[CSCodecTestsTranslator.Impl]
      make[CSCodecFixtureTranslator].from[CSRandomMethodTranslatorImpl]
      make[CSDomainTreeTools].from[CSDomainTreeTools.CSDomainTreeToolsImpl]
      many[CSCodecTranslator]
        .add[CSNSJsonCodecGenerator]
        .add[CSUEBACodecGenerator]
    })

  make[ScopeSupport].from[ScopeSupport.ScopeSupportImpl]

  make[Seq[Path]].named("inputs").fromValue(inputs)

  many[BaboonAbstractTranslator]
    .ref[CSBaboonTranslator]

  make[CSBaboonTranslator]

  make[CSTreeTools].from[CSTreeTools.CSTreeToolsImpl]
  make[CSFileTools].from[CSFileTools.CSFileToolsImpl]
  make[CSTypeTranslator]

  makeSubcontext[CSConversionTranslator]
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
      make[CSConversionTranslator]
    })
    .extractWith {
      (handler: CSConversionTranslator) =>
        handler
    }

}
