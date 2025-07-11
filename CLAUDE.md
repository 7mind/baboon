# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Baboon is a Domain Modeling Language (DML) compiler with schema evolution support. It compiles `.baboon` domain model files to multiple target languages (Scala, C#) with automatic JSON and UEBA codec generation.

## Essential Commands

### Build Commands
```bash
# Compile the project
sbt compile

# Build native executable
sbt GraalVMNativeImage/packageBin

# Clean build
sbt clean compile
```

### Testing Commands
```bash
# Run all tests
sbt test

# Run specific test
sbt "testOnly *SpecificTestSpec"
```

### Running the Compiler
```bash
# Example compilation command
baboon \
  --model-dir ./src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon.lock \
  :cs \
  --output ./output/cs \
  :scala \
  --output ./output/scala
```

## High-Level Architecture

### Core Components

1. **Parser (`parser/` package)**
   - Parses `.baboon` files using FastParse
   - Main entry: `BaboonParser.scala`
   - Produces raw AST representations

2. **Type System (`typer/` package)**
   - Converts raw AST to typed AST
   - Handles type resolution, inheritance, and validation
   - Key classes: `BaboonTyper.scala`, `TypePasses.scala`

3. **Validators (`validator/` package)**
   - Ensures model consistency
   - Checks evolution compatibility
   - Validates foreign type mappings

4. **Code Generators (`translator/` package)**
   - `csharp/` - C# code generation with advanced deduplication
   - `scala/` - Scala code generation
   - Each generator produces source files, codec implementations and conversions from lower versions to higher ones

5. **Runtime Support (`src/main/resources/baboon-runtime/`)**
   - Contains runtime libraries copied to generated code
   - Separate implementations for each target language

### Domain Language Features

Baboon files support:
- **Structural inheritance**: Using `+` (union), `-` (subtraction), `^` (intersection)
- **Type categories**: DTOs, ADTs, enums, foreign types
- **Collections**: Lists, sets, dictionaries, options
- **Annotations**: `@root` (entry points); `: derived[json]`, `: derived[ueba]` (request codecs)
- **Evolution**: Automatic schema migration where possible

### Key Design Patterns

1. **Multi-stage Compilation**:
   - Parse → Raw AST → Typed AST → Validated Model → Generated Code
   - Each stage uses separate data structures for type safety

2. **Dependency Injection**:
   - Uses distage for wiring components
   - Allows easy testing and modularity

3. **Codec Generation**:
   - Generates both JSON (via Circe) and custom binary (UEBA) codecs
   - Supports automatic evolution between versions

4. **CLI Design**:
   - Multi-modal CLI with language-specific options
   - Uses decline for command parsing

### Testing Strategy

- Unit tests for individual components
- Integration tests with full compilation cycles
- Generated code tests in `test/cs-stub/` and `test/sc-stub/`
- Evolution tests validating schema migration

### Important Considerations

1. **Root Types**: Only types marked with `@root` or transitively referenced by roots are included in output
2. **Foreign Types**: Require manual codec implementation in target languages
3. **Evolution**: Not all schema changes are automatically evolvable
4. **Deduplication**: C# generator performs sophisticated deduplication to reduce code size
