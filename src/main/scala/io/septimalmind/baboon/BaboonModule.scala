package io.septimalmind.baboon

import distage.{DIKey, ModuleDef}
import io.septimalmind.baboon.BaboonCompiler.CompilerOptions
import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.translator.BaboonAbstractTranslator
import io.septimalmind.baboon.translator.csharp.CSValue.CSPackageId
import io.septimalmind.baboon.translator.csharp.*
import io.septimalmind.baboon.typer.*
import io.septimalmind.baboon.typer.BaboonTyper.{FullRawDefn, ScopedDefn}
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.util.BLogger
import io.septimalmind.baboon.validator.BaboonValidator
import izumi.fundamentals.collections.nonempty.NEList

import java.nio.file.Path

class BaboonModule(options: CompilerOptions,
                   inputs: Seq[Path],
                   testOutDir: Option[Path])
    extends ModuleDef {
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

  many[BaboonAbstractTranslator].ref[CSBaboonTranslator]
  make[CSBaboonTranslator]
  make[CSDefnTools].from[CSDefnTools.CSDefnToolsImpl]
  make[CSDefnTranslator].from[CSDefnTranslator.CSDefnTranslatorImpl]
  make[CSTypeTranslator]
  make[ScopeSupport].from[ScopeSupport.ScopeSupportImpl]
  make[CSCodecTestsTranslator].from[CSCodecTestsTranslator.Impl]

  make[Seq[Path]].named("inputs").fromValue(inputs)
  make[Option[Path]].named("test-output").fromValue(testOutDir)

  makeSubcontext[BaboonTranslator]
    .localDependencies(
      List(
        DIKey[Pkg],
        DIKey[ScopedDefn],
        DIKey[Map[TypeId, DomainMember]]
      )
    )
    .withSubmodule(new ModuleDef {
      make[BaboonTranslator]
    })
    .extractWith { (translator: BaboonTranslator) =>
      translator
    }
//  make[LocalContext[Identity, BaboonTranslator]]
//    .fromLocalContext(new ModuleDef {
//      make[BaboonTranslator]
//    }.running { (translator: BaboonTranslator) =>
//      translator
//    })
//    .external(
//      DIKey[Pkg],
//      DIKey[NEList[Scope[FullRawDefn]]],
//      DIKey[Map[TypeId, DomainMember]]
//    )

  makeSubcontext[IndividualConversionHandler]
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
      make[IndividualConversionHandler]
    })
    .extractWith { (handler: IndividualConversionHandler) =>
      handler
    }

//  make[LocalContext[Identity, IndividualConversionHandler]]
//    .fromLocalContext(new ModuleDef {
//      make[IndividualConversionHandler]
//    }.running { (handler: IndividualConversionHandler) =>
//      handler
//    })
//    .external(
//      DIKey[CSPackageId],
//      DIKey[Version],
//      DIKey.get[Domain].named("current"),
//      DIKey.get[Domain].named("source"),
//      DIKey.get[BaboonRuleset]
//    )

  many[CSCodecTranslator]
    .add[CSNSJsonCodecGenerator]
    .add[CSUEBACodecGenerator]
}
