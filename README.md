# baboon

[![Build Status](https://github.com/7mind/baboon/actions/workflows/baboon-build.yml/badge.svg)](https://github.com/7mind/baboon/actions/workflows/baboon-build.yml)
[![License](https://img.shields.io/github/license/7mind/baboon)](https://github.com/7mind/baboon/blob/main/LICENSE)
[![Latest Release](https://img.shields.io/github/v/release/7mind/baboon)](https://github.com/7mind/baboon/releases/latest)
[![IntelliJ Plugin](https://img.shields.io/badge/IntelliJ-Plugin-blue)](https://plugins.jetbrains.com/plugin/28203-baboon-support)
[![VSCode Extension](https://img.shields.io/badge/VSCode-Extension-blue)](https://marketplace.visualstudio.com/items?itemName=SeptimalMind.baboon-vscode)
[![Nix](https://img.shields.io/badge/Built%20with-Nix-5277C3.svg?logo=nixos&logoColor=white)](https://builtwithnix.org)
[![Nix Flake](https://img.shields.io/badge/Nix-Flake-blue.svg)](https://nixos.wiki/wiki/Flakes)

*Let the Baboon do the monkey job.*

Baboon is a minimal Data Modeling Language and compiler that provides ergonomic, declarative schemas and enforces reliable schema evolution. The compiler runs as a fast immutable multi-phase DAG transform, and is easy to understand and maintain.

Essentially, you define your data structures and Baboon generates implementations for you. Then you define new versions, Baboon generates new versions of the structures, the conversions from old structure versions to new ones and forces your to provide conversions which cannot be derived automatically. Also it comes with extremely efficient tagless binary encoding for all your structures.

Currently generates **C#** and **Scala**, more backends are on the way.

## Highlights

- Automatic codec derivation for [JSON](docs/json-codecs.md) and [UEBA](docs/ueba-format.md) (Ultra-Efficient Binary Aggregate, a custom tagless binary format) 
- Evolution-aware codegen: derives migrations when possible, emits stubs when manual work is required
- Set-based structural inheritance with `+`, `-`, and `^` operators
- Algebraic data types (`adt`), DTOs (`data`) and enums.
- Basic form of nominal inheritance (`contract`)
- Namespaces, includes, and imports
- Collections (`opt`, `lst`, `set`, `map`) and timestamps/UID primitives
- Deduplicated C# output (reuses as much code as possible to lower binary footprint)

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

## Limitations

1. No templates
2. Only Enums, DTOs and ADTs
3. Nominal inheritance support is limited to trait model
4. Generic/type constructor support is limited to builtin collections
5. (*) This is a DML, not an IDL, service/interface definitions support is extremely limited at the moment
7. (*) Comments are not preserved in the transpiler output
8. (*) No structural inheritance information is preserved in the transpiler output
9. (*) Only integer constants may be associated with enum members
10. (*) No newtypes/type aliases
11. (*) No inheritance-based lenses/projections/conversions

Points marked with (*) will/may be improved in the future.

## CLI

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
direnv exec . mdl :build :test-gen-regular-adt :test-cs-regular :test-scala-regular
direnv exec . mdl :build :test-gen-wrapped-adt :test-cs-wrapped :test-scala-wrapped
direnv exec . mdl :build :test-gen-manual :test-gen-compat-scala :test-gen-compat-cs :test-manual-cs :test-manual-scala

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
