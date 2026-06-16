# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Baboon is a Domain Modeling Language (DML) compiler with schema evolution support. It compiles `.baboon` domain model files to multiple target languages (Scala, C#, Python, Rust, TypeScript, Kotlin, Java, Dart, Swift) with automatic JSON and UEBA codec generation. It also supports schema-only output formats (GraphQL SDL, OpenAPI 3.1).

## Essential Commands

This project uses [mudyla](https://github.com/7mind/mudyla) for build orchestration.

### Pre-commit / pre-push verification — `mdl` targets

**Run mdl with the appropriate target before every commit and push.** The same mdl actions are invoked by `.github/workflows/baboon-build.yml`, so what passes locally is what CI runs — single source of truth, defined in `.mdl/defs/{actions,tests}.md`.

```bash
# Pre-commit (fast — build + cross-compile JVM/JS + full test matrix):
mdl :build :test

# Pre-push or after non-trivial refactor (everything CI runs):
mdl :ci

# Individual targets (see .mdl/defs/{actions,tests}.md for definitions):
mdl :build                   # GraalVM native-image build (triggers sbt +compile, JVM + JS)
mdl :test                    # full per-language test matrix
mdl :smoke                   # native-image binary portability + round-trip
mdl :test-editors            # tree-sitter editor grammar tests
mdl :test-acceptance         # cross-language serialization acceptance tests
mdl :test-service-acceptance # service-flavour RPC wiring round-trips
```

**Why this matters — historical failure modes these targets catch:**

- `sbt baboonJVM/compile` is **NOT** a CI-equivalent check. It builds only the JVM project. CI runs `sbt +compile` (cross-build for JVM + Scala.js), which has stricter `-Wconf` settings that promote inexhaustive-match warnings to errors. PR-47 (M21) shipped with green local `sbt baboonJVM/compile` but failed CI on `BaboonJS.scala` (commit `2de517b` fixed it). `mdl :build :test` triggers `sbt +compile` — the same path CI uses.
- When adding a new `TyperIssue` case class, **three** exhaustive-match sites must be updated: `lsp/features/DiagnosticsProvider.scala`, `lsp/state/WorkspaceState.scala`, and `.js/src/main/scala/io/septimalmind/baboon/BaboonJS.scala`. The `:build`/`:test` cross-build catches missed JS-side updates.
- **M29 pattern (PR-29.7):** when a single PR introduces multiple new `TyperIssue` cases, **bundle them into one touch per exhaustive-match file** rather than updating each file once per case. PR-29.7 added three new cases (`TemplateNotInstantiated`, `NotATemplate`, `TemplateBodyCarriesDerived`) as 9 case arms total across 3 files — 3 file touches, not 9. Reduces the risk of CI red from a partially-applied exhaustive-match update mid-PR. PR-29.4 (`DuplicateTypeParam`), PR-29.5 (`TemplateArityMismatch`, `TemplateInstantiationInBody`), and PR-29.7 are the canonical M29 examples of the 3-site update pattern.

**Flags & environment:**

- CI passes `--github-actions` (CI-grouped log output) — humans don't set it.
- macOS / Windows CI passes `--without-nix` (system tools, no nix shell).
- **Local Kotlin OOM workaround:** `mdl :test` runs language test actions in parallel by default. On laptops with <16 GB RAM the Kotlin compiler daemon OOMs under the parallel matrix (documented in `docs/logs/20260428-2350-m16-closeout-log.md`). Run `mdl --seq :build :test` to force serial execution. Slower (~20 min) but completes on memory-constrained machines. CI uses default parallelism.
- **sbt-git (jgit) cannot build inside a linked git worktree** (`NoWorkTreeException: Bare Repository`). When running `mdl :build` or `sbt` from a `git worktree add`-created directory (whose `.git` is a file, not a directory), sbt-git throws this error. Work around by cloning the repo to a real directory (e.g., `git clone <repo> /tmp/baboon-ci-clone`) and running the pipeline from there.
- **Swift 64KB-constant limit (D7/T23):** `baboon_runtime.swift` was split into `baboon_runtime.swift` (pure domain runtime) and `baboon_service_wiring.swift` (service-wiring runtime) to stay within the JVM 64KB bytecode-constant limit hit when the Swift runtime is embedded via `PortableResource.embedSources`. After modifying `baboon-compiler/src/main/resources/baboon-runtime/`, run `sbt clean` before `sbt compile` (the macro caches content per build).
- **Swift `@BaboonIndirect` recursive-DTO boxing (D8/T23):** Swift value types (`struct`) cannot have stored properties of their own type (infinite size). Baboon detects recursive DTO fields and wraps them in `@BaboonIndirect` (a property-wrapper that boxes the value behind a `Box<T>` class). Any new test model that includes recursive DTOs in Swift must compile with this boxing in place.
- **Scala MCP requires Either-mode serviceResult (D24/T69):** the Scala MCP dispatch runtime (`AbstractBaboonMcpServer.handle` in `baboon-runtime/scala/BaboonMcpRuntime.scala`) is synchronous and Either-shaped — `handle` matches `Right`/`Left` on the wiring's `invokeJson` result to drive MCP Channel-A/B. It is NOT generic over the configurable `serviceResult` container that `ScServiceWiringTranslator` honors. Combining a non-Either serviceResult (HKT `F[_,_]`, a custom result type, or `--service-result-no-errors=true`) with `--scala-generate-mcp-server=true` is rejected up front with `TranslationIssue.ScalaMcpRequiresEither` (an actionable compiler error) rather than emitting a delegate whose type mismatches the wiring container. The Scala MCP lane (`test-gen-scala-mcp`) therefore stays on `--service-result-type=Either`. Making Scala MCP container-generic is the async-axis (D24) rework, out of scope here.
- **MCP stub model directory isolation (D9/T24):** The MCP stub model lives at `baboon-compiler/src/test/resources/mcp-stub-ok/` — intentionally OUTSIDE the shared `baboon-compiler/src/test/resources/baboon/` model-dir. All `test-gen-*-wiring` and `test-gen-{regular,wrapped}-adt` actions use `--model-dir ./baboon-compiler/src/test/resources/baboon/` and therefore scan every subdirectory. Placing the MCP stub inside the shared dir caused cross-namespace fixture-reference failures in 8 wiring/test lanes (D9). The dedicated `test-gen-<lang>-mcp` actions point at `--model-dir ./baboon-compiler/src/test/resources/mcp-stub-ok/`. A minimal recursive-only `Tree` model (`baboon-compiler/src/test/resources/baboon/recursive-ok/recursive.baboon`) preserves recursive-DTO coverage in the shared matrix without the cross-namespace hazard.

**Resource files:** After modifying any file under `baboon-compiler/src/main/resources/baboon-runtime/`, run `sbt clean` before `sbt compile`. The `PortableResource.embedSources` macro caches resource contents per build, so incremental compile won't pick up resource changes.

### Build Commands
```bash
# Format code
mdl :fmt

# Build the compiler native executable
mdl :build

# Run the full test suite
mdl :build :test

# Run specific test suites independently:
# - Regular ADT tests
mdl :build :test-gen-regular-adt :test-cs-regular :test-scala-regular :test-rust-regular :test-typescript-regular :test-kotlin-regular :test-kotlin-kmp-regular :test-java-regular :test-dart-regular :test-swift-regular
# - Wrapped ADT tests
mdl :build :test-gen-wrapped-adt :test-cs-wrapped :test-scala-wrapped :test-rust-wrapped :test-typescript-wrapped :test-kotlin-wrapped :test-kotlin-kmp-wrapped :test-java-wrapped :test-dart-wrapped :test-swift-wrapped
# - Manual/compatibility tests
mdl :build :test-gen-manual :test-gen-compat-scala :test-gen-compat-cs :test-gen-compat-rust :test-gen-compat-typescript :test-gen-compat-kotlin :test-gen-compat-kotlin-kmp :test-gen-compat-java :test-gen-compat-dart :test-gen-compat-swift :test-manual-cs :test-manual-scala :test-manual-rust :test-manual-typescript :test-manual-kotlin :test-manual-kotlin-kmp :test-manual-java :test-manual-dart :test-manual-swift

# Run complete build pipeline (format, build, test)
mdl :full-build

# Create distribution packages
mdl :build :mkdist
```

### Direct SBT Commands (for development)
```bash
# Compile the project
sbt compile

# Build native executable
sbt GraalVMNativeImage/packageBin

# Clean build
sbt clean compile

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
  --output ./output/scala \
  :rust \
  --output ./output/rust \
  :typescript \
  --output ./output/ts \
  :kotlin \
  --output ./output/kotlin \
  :java \
  --output ./output/java \
  :dart \
  --output ./output/dart \
  :swift \
  --output ./output/swift
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
   - `python/` - Python code generation
   - `rust/` - Rust code generation with native types, serde derive, and custom UEBA binary codecs
   - `typescript/` - TypeScript code generation with function-based JSON/UEBA codecs
   - `kotlin/` - Kotlin code generation with Jackson JSON codecs and UEBA binary codecs
   - `java/` - Java code generation with Jackson JSON codecs and UEBA binary codecs
   - `dart/` - Dart code generation with dart:convert JSON codecs and UEBA binary codecs
   - `swift/` - Swift code generation with JSONSerialization JSON codecs and UEBA binary codecs
   - `graphql/` - GraphQL SDL schema generation (type definitions only, no codecs)
   - `openapi/` - OpenAPI 3.1 JSON Schema generation (component schemas only, no codecs)
   - Each generator produces source files, codec implementations and conversions from lower versions to higher ones

5. **Runtime Support (`src/main/resources/baboon-runtime/`)**
   - Contains runtime libraries copied to generated code
   - Separate implementations for each target language

### Domain Language Features

Baboon files support:
- **Structural inheritance**: Using `+` (union), `-` (subtraction), `^` (intersection)
- **Type categories**: DTOs, ADTs, enums, foreign types, type aliases
- **Collections**: Lists, sets, dictionaries, options
- **Annotations**: `@root` (entry points); `: derived[json]`, `: derived[ueba]` (request codecs)
- **Evolution**: Automatic schema migration where possible
- **Extracted contracts** (`has mirror` / `has contract`): Inside a templated `data`, `adt`, or `id` body, a `has mirror B` clause synthesises a standalone sibling contract `B` that enumerates the host's parameter-free fields — `B` has no relationship to the host's instantiated types. A `has contract B` clause does the same but additionally wires every instantiation of the host to implement `B` (implicit `is B`). `B` enumerates the host's parameter-free fields collected from own fields and structural-inheritance arms. Multiple `has` clauses are permitted on one host.

### Key Design Patterns

1. **Multi-stage Compilation**:
   - Parse → Raw AST → Typed AST → Validated Model → Generated Code
   - Each stage uses separate data structures for type safety

2. **Dependency Injection**:
   - Uses distage for wiring components
   - Allows easy testing and modularity

3. **Codec Generation**:
   - Generates both JSON and custom binary (UEBA) codecs
   - JSON: Circe (Scala), Newtonsoft.Json (C#), serde (Rust), Jackson (Kotlin, Java), dart:convert (Dart), JSONSerialization (Swift), custom (Python, TypeScript)
   - Supports automatic evolution between versions

4. **CLI Design**:
   - Multi-modal CLI with language-specific options
   - Uses decline for command parsing

### Testing Strategy

- Unit tests for individual components
- Integration tests with full compilation cycles
- Generated code tests in `test/cs-stub/`, `test/sc-stub/`, `test/py-stub/`, `test/rs-stub/`, `test/ts-stub/`, `test/kt-stub/`, `test/jv-stub/`, `test/dt-stub/`, `test/sw-stub/`, `test/gql-stub/`, and `test/oas-stub/`
- Cross-platform compatibility tests in `test/conv-test-{cs,sc,py,rs,ts,kt,jv,dt,sw}/` (verifies JSON/UEBA interop across all languages)
- Evolution tests validating schema migration
- MCP server round-trip tests in `test/<lang>-stub-mcp-overlay/` (one per backend; runs against `mcp-stub-ok` model with `--<lang>-generate-mcp-server=true`)

**MCP server generation** (off by default): each of the 9 backends supports `--<lang>-generate-mcp-server=true` (where `<lang>` is `cs`, `scala`, `py`, `rs`, `ts`, `kt`, `jv`, `dt`, `sw`). When the flag is absent, the compiler output is byte-identical to the pre-MCP baseline — no extra files are written. The MCP test harness uses `baboon-compiler/src/test/resources/mcp-stub-ok/mcp_stub.baboon` (isolated from the shared model-dir — see D9 note above) with overlay directories `test/<lang>-stub-mcp-overlay/` layered on top of the standard stub project. Harness actions: `test-gen-<lang>-mcp` + `test-<lang>-mcp`. All 9 MCP lanes are included in `mdl :test` and `mdl :ci`.

**Parallel Test Execution**: Test actions `test-gen-regular-adt` and `test-gen-wrapped-adt` can run in parallel. Each action:
1. Creates an isolated temporary directory under `target/` (`test-regular/` or `test-wrapped/`)
2. Copies stub projects (excluding generated files and build artifacts) via rsync
3. Generates code into the isolated directory
4. Subsequent test actions run in these isolated directories

### Important Considerations

1. **Root Types**: Only types marked with `@root` or transitively referenced by roots are included in output
2. **Foreign Types**: Require manual codec implementation in target languages
3. **Evolution**: Not all schema changes are automatically evolvable
4. **Deduplication**: C# generator performs sophisticated deduplication to reduce code size
