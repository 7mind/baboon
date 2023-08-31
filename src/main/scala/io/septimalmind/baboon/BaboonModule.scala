package io.septimalmind.baboon

import distage.ModuleDef
import io.septimalmind.baboon.BaboonCompiler.CompilerOptions
import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.translator.AbstractBaboonTranslator
import io.septimalmind.baboon.translator.csharp.{
  CSBaboonTranslator,
  CSDefnTranslator,
  CSTypeTranslator
}
import io.septimalmind.baboon.typer.*
import io.septimalmind.baboon.validator.BaboonValidator

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

  many[AbstractBaboonTranslator].ref[CSBaboonTranslator]
  make[CSBaboonTranslator]
  make[CSDefnTranslator].from[CSDefnTranslator.CSDefnTranslatorImpl]
  make[CSTypeTranslator]
}
