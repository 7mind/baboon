# Scala.js Linking Issues

## Summary

The Baboon Scala.js port **compiles successfully** but fails during the linking phase due to JVM-specific dependencies.

## Current Status

- ✅ Scala.js plugin configured
- ✅ Cross-compilation working
- ✅ All Scala code compiles without errors
- ⚠️ Linker fails with "Referring to non-existent class" errors

## Root Cause

The linking errors occur because:

1. **izumi-fundamentals** uses JVM-specific APIs:
   - `java.time.LocalDate` and other `java.time.*` classes
   - These are used in `IzTimeOrderingSafe` and other platform utilities

2. **Transitive Dependencies**:
   - Even though our code doesn't directly use these APIs
   - They're pulled in through izumi's platform detection and utilities
   - Distage's `LocatorFormatter` triggers the initialization

## Linking Errors

```
Referring to non-existent class java.time.ZonedDateTime
Referring to non-existent class java.time.LocalDate
```

These come from:
- `izumi.fundamentals.platform.time.IzTimeOrderingSafe`
- `izumi.fundamentals.preamble$`
- Called through `LocatorFormatter` when rendering Locator.toString()

## Solutions

### Immediate Fix: Manual DI (No distage)

Instead of using distage's DI, manually construct the dependencies:

```scala
object BaboonCompilerSimple {
  def compile(inputs: Seq[Input], target: CompilerTargetJS): Seq[OutputFile] = {
    // Manually construct all dependencies
    val parser = new BaboonParserImpl
    val comparator = new BaboonComparatorImpl
    val rules = new BaboonRulesImpl
    // ... etc

    // No Locator, no DI - just direct instantiation
    val translator = target match {
      case _: CSTarget => new CSBaboonTranslator(...)
      case _: ScTarget => new ScBaboonTranslator(...)
    }

    translator.translate(model)
  }
}
```

### Medium-term Fix: Platform-specific DI Module

Create a JS-specific DI module that doesn't use izumi-fundamentals features:

```scala
class SimpleDIModuleJS extends ModuleDef {
  // Only include what's needed
  // Skip anything that might pull in platform utilities
}
```

### Long-term Fix: Fork izumi or Use Alternative

- Fork izumi-fundamentals and create a pure JS version
- Or replace distage with a simpler DI library (e.g., MacWire)
- Or use manual dependency construction

## Workaround for Testing

To test the compilation pipeline without full DI:

1. Extract the core compilation logic into standalone functions
2. Test those functions directly
3. Add the DI layer later once linking issues are resolved

## Related Files

- `src/main/scala-js/io/septimalmind/baboon/BaboonJS.scala` - Main JS entry point
- `src/main/scala-js/io/septimalmind/baboon/BaboonModuleJS.scala` - DI module (causes linking issues)
- `build.sbt` - Cross-compilation configuration

## Recommendations

**For Production Use:**
1. Implement manual DI approach for JS
2. Keep distage for JVM version (it's working great there)
3. This gives you the best of both worlds

**Effort Estimate:** 2-3 hours to implement manual DI version for JS

## Notes

- The core compiler logic IS platform-agnostic (as designed)
- The issue is purely in the dependency injection / initialization layer
- All the hard work (parsing, typing, translation) is already done and working
