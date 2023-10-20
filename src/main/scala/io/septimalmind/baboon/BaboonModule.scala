package io.septimalmind.baboon

import distage.{DIKey, ModuleDef}
import io.septimalmind.baboon.BaboonCompiler.CompilerOptions
import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.translator.AbstractBaboonTranslator
import io.septimalmind.baboon.translator.csharp.CSValue.CSPackageId
import io.septimalmind.baboon.translator.csharp.{
  CSBaboonTranslator,
  CSCodecTranslator,
  CSDefnTools,
  CSDefnTranslator,
  CSNSJsonCodecGenerator,
  CSTypeTranslator,
  CSUEBACodecGenerator,
  IndividualConversionHandler
}
import io.septimalmind.baboon.typer.*
import io.septimalmind.baboon.typer.BaboonTyper.FullRawDefn
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.util.BLogger
import io.septimalmind.baboon.validator.BaboonValidator
import izumi.distage.LocalContext
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.functional.Identity

class BaboonModule(options: CompilerOptions) extends ModuleDef {
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

  make[BLogger].from[BLogger.BLoggerImpl]

  many[AbstractBaboonTranslator].ref[CSBaboonTranslator]
  make[CSBaboonTranslator]
  make[CSDefnTools].from[CSDefnTools.CSDefnToolsImpl]
  make[CSDefnTranslator].from[CSDefnTranslator.CSDefnTranslatorImpl]
  make[CSTypeTranslator]
  make[ScopeSupport].from[ScopeSupport.ScopeSupportImpl]

  make[LocalContext[Identity, BaboonTranslator]]
    .fromLocalContext(new ModuleDef {
      make[BaboonTranslator]
    }.running { (translator: BaboonTranslator) =>
      translator
    })
    .external(
      DIKey[Pkg],
      DIKey[NEList[Scope[FullRawDefn]]],
      DIKey[Map[TypeId, DomainMember]]
    )

  make[LocalContext[Identity, IndividualConversionHandler]]
    .fromLocalContext(new ModuleDef {
      make[IndividualConversionHandler]
    }.running { (handler: IndividualConversionHandler) =>
      handler
    })
    .external(
      DIKey[CSPackageId],
      DIKey[Version],
      DIKey.get[Domain],
      DIKey.get[BaboonRuleset]
    )

  many[CSCodecTranslator]
    .add[CSNSJsonCodecGenerator]
    .add[CSUEBACodecGenerator]
}
