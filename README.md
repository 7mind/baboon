# baboon

[![Build Status](https://github.com/7mind/baboon/actions/workflows/baboon-build.yml/badge.svg)](https://github.com/7mind/baboon/actions/workflows/baboon-build.yml)
[![License](https://img.shields.io/github/license/7mind/baboon)](https://github.com/7mind/baboon/blob/main/LICENSE)
[![Latest Release](https://img.shields.io/github/v/release/7mind/baboon)](https://github.com/7mind/baboon/releases/latest)
[![IntelliJ Plugin](https://img.shields.io/badge/IntelliJ-Plugin-blue)](https://plugins.jetbrains.com/plugin/28203-baboon-support)
[![VSCode Extension](https://img.shields.io/badge/VSCode-Extension-blue)](https://marketplace.visualstudio.com/items?itemName=SeptimalMind.baboon-vscode)
[![Nix](https://img.shields.io/badge/Built%20with-Nix-5277C3.svg?logo=nixos&logoColor=white)](https://builtwithnix.org)
[![Nix Flake](https://img.shields.io/badge/Nix-Flake-blue.svg)](https://nixos.wiki/wiki/Flakes)
[![Playground](https://img.shields.io/badge/Try-Playground-green)](https://7mind.github.io/baboon/)

<p align="center">
  <a href="https://7mind.github.io/baboon/">
    <img src="https://img.shields.io/badge/%F0%9F%90%92%20TRY%20ME%20%E2%80%94%20Online%20Playground-blue?style=for-the-badge&logoColor=white" alt="Try Me — Online Playground" height="40"/>
  </a>
</p>

*Let the Baboon do the monkey job.*

Baboon is a minimal Data Modeling Language and compiler that provides ergonomic, declarative schemas and enforces reliable schema evolution. The compiler runs as a fast immutable multi-phase DAG transform, and is easy to understand and maintain.

Essentially, you define your data structures and Baboon generates implementations for you. Then you define new versions, Baboon generates new versions of the structures, the conversions from old structure versions to new ones and forces you to provide conversions which cannot be derived automatically. Also it comes with extremely efficient tagless binary encoding for all your structures.

The language is completely formal and platform/implementation-agnostic. Unlike e.g. OpenAPI, you can't leave some parts of a structure undefined; you still have an unsafe escape hatch in the form of foreign types.

Use cases:
  - Serializable application state with safe and automatic version upgrades
  - Efficient data transfer with custom tagless binary format
  - Quick data model design with an extremely concise and expressive language

Generates:
  - **C#**
  - **Scala**
  - **Python**
  - **Rust**
  - **TypeScript**
  - **Kotlin**
  - **Java**
  - **Dart**
  - **Swift**

Schema-only output formats (type definitions, no codecs):
  - **GraphQL SDL**
  - **OpenAPI 3.1**

## Highlights

- Automatic codec derivation for [JSON](docs/json-codecs.md) and [UEBA](docs/ueba-format.md) (Ultra-Efficient Binary Aggregate, a custom tagless binary format)
- Evolution-aware codegen: derives migrations when possible, emits stubs when manual work is required
- Set-based structural inheritance with `+`, `-`, and `^` operators
- Algebraic data types (`adt`), DTOs (`data`) and enums.
- First-class [identifier types](docs/language-features.md#identifier-types-id) (`id` keyword) with parseable canonical repr (`<Name>:<ver>#field:value:...`) and free `id`↔`data` conversion when shapes match.
- [ADT branch inheritance and subtraction](docs/language-features.md#adt-branch-inheritance-and-subtraction) (`+`, `^`, `-`) for cross-ADT branch reuse.
- [Rename tracking](docs/language-features.md#rename-tracking-was) (`was`) for fields, types, and enum members — renames stay automatically convertible instead of breaking evolution.
- [Doc comments](docs/language-features.md#doc-comments) (`/** … */`, `//!`) preserved into every backend as idiomatic doc comments (and GraphQL/OpenAPI descriptions) without affecting wire formats or schema digests.
- Cross-language wire-form parity verified by per-backend conv-test matrix: byte-identical JSON and UEBA fixtures across all 9 generated backends.
- [Polymorphic `any` fields](docs/language-features.md#polymorphic-any-fields) — opaque envelope for runtime-typed payloads with byte-canonical cross-language wire format.
- [User-defined templates](docs/language-features.md#templates-generics) (`data Page[T] { ... }`, `adt Result[T, E] { ... }`, `contract Acked[T] { ... }`, `service Crud[K, V] { ... }`) instantiated through type aliases (`type IntPage = Page[i32]`); monomorphised at compile time so codegen never sees a generic — every backend emits a concrete type. Same-package cross-namespace instantiation supported (`type Y = ns.Foo[i32]`). See [docs/spec/generics.md](docs/spec/generics.md) for the full spec.
- Type aliases for convenience (`type Name = TargetType`)
- Basic form of nominal inheritance (`contract`)
- Namespaces, includes, and imports
- Collections (`opt`, `lst`, `set`, `map`) and timestamps/UID primitives
- Schema-only backends: GraphQL SDL and OpenAPI 3.1 component schemas from the same model.
- Optional [MCP server generation](docs/cli-reference.md#mcp-servers): every `service` can be exposed as a Model Context Protocol server in all 9 languages (`--<lang>-generate-mcp-server`).
- Deduplicated C# output (reuses as much code as possible to lower binary footprint)
- Interactive Explorer: REPL-style shell for browsing types, evolution, and debugging codecs.
- LSP support

# TLDR

You define your data model:

```
model acme.billing
version "1.0.0"

root adt PaymentMethod {
  data Card {
    pan: str
    holder: str
  }
  data Wallet {
    provider: str
    token: str
  }
}
```

Then you refactor and extend it:

```
model acme.billing
version "2.0.0"

data Token {
  token: str
}

root adt PaymentMethod {
  data Card {
    pan: str
    holder: str
  }
  // refactored, but same structure as before
  data Wallet {
    provider: str
    + Token
  }

  // new ADT member
  data BankTransfer { iban: str }
}
```

Baboon generates conversions (migrations) from version `1.0.0` to `2.0.0`. In this particular case all the migrations will be generated automatically.

Detailed language walkthrough with copy-paste examples: [docs/language-features.md](docs/language-features.md).

## Editor support

- [Intellij Idea Plugin](https://plugins.jetbrains.com/plugin/28203-baboon-support) (source: [baboon-intellij](https://github.com/7mind/baboon-intellij))
- [VSCode Extension](https://marketplace.visualstudio.com/items?itemName=SeptimalMind.baboon-vscode) (source: [baboon-vscode](https://github.com/7mind/baboon-vscode))
- [VSCodium Extension](https://open-vsx.org/extension/SeptimalMind/baboon-vscode)
- **LSP Server**: Baboon includes a built-in Language Server Protocol (LSP) implementation for integration with any LSP-compatible editor (Neovim, Emacs, etc.). See [docs/lsp-integration.md](docs/lsp-integration.md) for details.

## Interactive Explorer

Baboon includes an interactive explorer for browsing and debugging domain models. It provides a REPL-style shell to explore types, dependencies, evolution history, and test codecs.

```bash
baboon --model-dir ./src/models :explore
```

See [docs/explorer-mode.md](docs/explorer-mode.md) for a full command reference.

## Limitations

1. Nominal inheritance support is limited to trait model
2. Templates are monomorphised — no per-language reified generics in emitted source. Higher-kinded templates, variance annotations, where-clauses/bounds, and templates on `id` declarations or ADT-body inheritance arms are out of scope (structural arms in `data`/`contract` bodies *do* accept template instantiation, e.g. `+ Page[i32]`). Cross-package template instantiation (template declared in a different `.baboon` file) is also out of scope; same-package cross-namespace works. See [docs/spec/generics.md §6](docs/spec/generics.md) for the full deferred-items list.
3. (*) Plain comments (`//`, `/* */`) are not preserved in the transpiler output; [doc comments](docs/language-features.md#doc-comments) (`/** */`, `//!`) are
4. (*) No structural inheritance information is preserved in the transpiler output
5. (*) Only integer constants may be associated with enum members
6. No newtypes (type aliases are supported, newtypes are not)
7. (*) No inheritance-based lenses/projections/conversions

Points marked with (*) will/may be improved in the future.

## CLI

The CLI is multi-modal: global options (model inputs, lockfile, evolution metadata) followed by one or more `:target` sections, each with its own options. All targets in one invocation share a single parsed model.

- `:cs`, `:scala`, `:python`, `:rust`, `:typescript`, `:kotlin`, `:java`, `:dart`, `:swift` - Code generation for target languages
- `:graphql`, `:openapi` - Schema-only outputs (GraphQL SDL, OpenAPI 3.1 component schemas)
- `:explore` - Launch [Interactive Explorer](docs/explorer-mode.md)
- `:lsp` - Start [LSP Server](docs/lsp-integration.md)
- `:scheme` - Emit a cleaned-up single `.baboon` file for a domain version

**Full option reference (global and per-target): [docs/cli-reference.md](docs/cli-reference.md)**, or run `baboon --help`.

See build configuration in [.mdl/defs/actions.md](.mdl/defs/actions.md) and test configuration in [.mdl/defs/tests.md](.mdl/defs/tests.md).

## Notes

1. All the types which are not transitively referenced by `root` types will be eliminated from the compiler output.
2. Usages in structural inheritance are not considered references, so structural parents which are not directly referenced as fields and not marked as `root`s will be eliminated

## Foreign types

Be careful about foreign types. It is your responsibility to wire codecs correctly.

For every foreign type:

1) Create a custom codec
2) Override the generated dummy codec with `BaboonCodecs#Register`
3) Override the generated dummy codec using the setter on `${Foreign_Type_Name}_UEBACodec#Instance`
4) Override the generated dummy codec using the setter on `${Foreign_Type_Name}_JsonCodec#Instance`

Make sure your foreign types are NOT primitive types or other generated types. It's a funny idea, but it will explode in runtime.

Foreign types may hold any position in generics but it's up to you to ensure correctness.

A foreign block may declare a wire-level Baboon equivalent with `rt = <baboon type>` (e.g. `rt = i32`). Schema-only backends (GraphQL, OpenAPI, MCP input schemas) render the foreign as that type, and codec machinery delegates through it where the backend supports it. See [Foreign types in the language guide](docs/language-features.md#foreign-types) for the full entry syntax.

## Development

### Build commands

This project uses [mudyla](https://github.com/7mind/mudyla) for build orchestration.

#### Common Commands

```bash
# Format code
direnv exec . mdl :fmt

# Build the compiler
direnv exec . mdl :build

# Run complete test suite
direnv exec . mdl :build :test

# Run full build pipeline (format, build, test)
direnv exec . mdl :full-build

# Run specific test suites
direnv exec . mdl :build :test-gen-regular-adt :test-cs-regular :test-scala-regular :test-rust-regular :test-typescript-regular :test-kotlin-regular :test-java-regular :test-dart-regular :test-swift-regular
direnv exec . mdl :build :test-gen-wrapped-adt :test-cs-wrapped :test-scala-wrapped :test-rust-wrapped :test-typescript-wrapped :test-kotlin-wrapped :test-java-wrapped :test-dart-wrapped :test-swift-wrapped
direnv exec . mdl :build :test-gen-manual :test-gen-compat-scala :test-gen-compat-cs :test-gen-compat-rust :test-gen-compat-typescript :test-gen-compat-kotlin :test-gen-compat-java :test-gen-compat-dart :test-gen-compat-swift :test-manual-cs :test-manual-scala :test-manual-rust :test-manual-typescript :test-manual-kotlin :test-manual-java :test-manual-dart :test-manual-swift

# Create distribution packages
direnv exec . mdl :build :mkdist

# Build with custom distribution paths
direnv exec . mdl --mkdist-source=./custom/path --mkdist-target=./output :build :mkdist
```

#### Setting up the environment

```bash
# Enter the nix development shell
nix develop

# Or use direnv for automatic shell activation
direnv allow
```

#### Quick testing

```bash
sbt baboonJVM/GraalVMNativeImage/packageBin
./baboon --help
```
