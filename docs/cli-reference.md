# Baboon CLI Reference

Complete reference for the `baboon` command line. The authoritative source is
[`CLIOptions.scala`](../baboon-compiler/.jvm/src/main/scala/io/septimalmind/baboon/CLIOptions.scala)
and [`Baboon.scala`](../baboon-compiler/.jvm/src/main/scala/io/septimalmind/baboon/Baboon.scala);
`baboon --help` prints a condensed version of this document.

## Invocation model

The CLI is multi-modal: one set of global options followed by one or more
*modality* sections. Each modality starts with a `:name` token and consumes its
own options until the next `:name` or the end of the line. All code-generation
modalities in a single invocation share one parsed and type-checked model, so
generating several languages at once is cheaper than separate runs.

```bash
baboon [global options] :target [target options] [:target [target options] ...]
```

```bash
baboon \
  --model-dir ./models \
  --lock-file ./target/baboon.lock \
  :cs     --output ./out/cs \
  :scala  --output ./out/scala --service-result-hkt \
  :openapi --output ./out/oas
```

Option syntax follows [case-app](https://github.com/alexarchambault/case-app)
conventions: kebab-case names, `--flag value` or `--flag=value`, boolean flags
may be passed bare (`--debug`) or with an explicit value (`--debug=false`).
Repeatable options (`--model`, `--model-dir`, `--pragma`, `--ext-allow-cleanup`)
are passed multiple times.

## Global options

| Option | Default | Description |
|---|---|---|
| `--model <file>` | — | A single `*.baboon` file to process. Repeatable; can be combined with `--model-dir`. |
| `--model-dir <dir>` | — | A directory to read `*.baboon` files from, recursively. Repeatable. |
| `--lock-file <file>` | none | Version-signature lockfile. Created on first run; on later runs the compiler verifies that every *non-latest* version's signature still matches and fails with `LockedVersionModified` if a frozen version was edited. The latest version of each domain is exempt (it is still in flux). |
| `--meta-write-evolution-json <file>` | none | Write evolution metadata (version lineage and per-version type identifiers for every domain) as a JSON file. |
| `--emit-only <pkgs>` | all | Comma-separated list of domain (package) names, e.g. `my.domain,other.pkg`. Every input is still parsed, typed and validated, but only the listed domains emit code. |
| `--debug` | `false` | Additional debug output (written file paths, etc.). |
| `--help`, `-h` | — | Print help and exit. |

## Modalities

| Modality | Purpose |
|---|---|
| `:cs` | Generate C# code |
| `:scala` | Generate Scala code |
| `:python` | Generate Python code |
| `:rust` | Generate Rust code |
| `:typescript` | Generate TypeScript code |
| `:kotlin` | Generate Kotlin code (JVM or KMP) |
| `:java` | Generate Java 21 code |
| `:dart` | Generate Dart 3+ code |
| `:swift` | Generate Swift 5.9+ code |
| `:graphql` | Generate GraphQL SDL schemas (type definitions only, no codecs) |
| `:openapi` | Generate OpenAPI 3.1 component schemas (no codecs) |
| `:lsp` | Start the LSP server ([docs](lsp-integration.md)) |
| `:explore` | Start the interactive explorer ([docs](explorer-mode.md)) |
| `:scheme` | Emit a cleaned-up single `.baboon` file for one domain version |

## Common transpiler options

Accepted by every code-generation modality (`:cs` … `:swift`, `:graphql`,
`:openapi`).

### Output layout

| Option | Default | Description |
|---|---|---|
| `--output <dir>` | required | Directory for generated code. |
| `--fixture-output <dir>` | none | Directory for generated random-value fixtures. When unset but `--test-output` is set, fixtures are emitted into the test directory. |
| `--test-output <dir>` | none | Directory for generated codec round-trip tests (implies fixture generation). |
| `--runtime <with\|only\|without>` | `with` | `with` emits both the shared runtime support code and the type definitions, `only` emits just the runtime, `without` emits just the definitions (use when several invocations share one runtime copy). |
| `--ext-allow-cleanup <ext>` | see below | File extensions the compiler is allowed to delete when erasing the output directory before generation. The directory is erased only if every file in it is a dot-file or matches the allowed set (default: `meta`, `cs`, `json`, `scala`, `py`, `pyc`, `rs`, `ts`, `kt`, `java`, `dart`, `swift`, `toml`, `graphql`); any other file aborts compilation with `CantCleanupTarget`. Repeatable; passing the option replaces the default set. |

### Conversions and versioning

| Option | Default | Description |
|---|---|---|
| `--disable-conversions` | `false` | Do not generate version-to-version conversion code. |
| `--omit-most-recent-version-suffix-from-paths` | `true` (C#) | The latest version of each domain is emitted without the version segment in file paths. Parsed by all targets but currently honored by the **C# backend only**. |
| `--omit-most-recent-version-suffix-from-namespaces` | `true` (C#) | Same, for namespaces. Currently **C# only**. |
| `--enable-deprecated-encoders` | `false` | Also generate *encoders* for non-latest (deprecated) versions. By default old versions get decoders only (you can read old data and write the latest version). Available on `:cs`, `:scala`, `:python`, `:kotlin`, `:java`. |

### Codecs

Not available on `:graphql` / `:openapi` (schema-only targets).

| Option | Default | Description |
|---|---|---|
| `--generate-json-codecs` | `true` | Generate JSON codecs. |
| `--generate-ueba-codecs` | `true` | Generate UEBA binary codecs. |
| `--generate-json-codecs-by-default` | `false` | Generate JSON codecs even for types without `derived[json]`. |
| `--generate-ueba-codecs-by-default` | `false` | Generate UEBA codecs even for types without `derived[ueba]`. |
| `--codec-test-iterations <n>` | `500` | Number of iterations each generated codec round-trip test performs. |
| `--generate-domain-facade` | `true` | Generate a per-domain `Domain<DomainId>Facade` class whose parameterless constructor auto-registers the codecs of all known versions. |
| `--<prefix>-wrapped-adt-branch-codecs` | `false` | Every ADT branch codec embeds the ADT discriminator metadata and expects it when decoding, so a branch value can be decoded without knowing the enclosing ADT. `<prefix>` is the per-language flag prefix (see table below). |
| `--<prefix>-write-evolution-dict` | `false` (`true` for Python) | Emit evolution metadata (version/type dictionaries) as a language-native structure for runtime introspection. |

### Service generation

Ignored by `:graphql` / `:openapi`. The same settings can be expressed as
[pragmas](language-features.md#services) in `.baboon` files; CLI flags and
`--pragma` values override in-file pragmas.

| Option | Default | Description |
|---|---|---|
| `--service-result-no-errors` | per language | Service methods return the success type directly instead of a result wrapper. Defaults: `false` for Scala, Rust, Kotlin (they wrap), `true` for C#, Python, TypeScript, Java, Dart, Swift. |
| `--service-result-type <type>` | per language | Wrapper type for service results (e.g. `scala.util.Either`, `Result`, `anyhow::Result`). |
| `--service-result-pattern <pattern>` | per language | Type-argument pattern for the wrapper; `$success` and `$error` are substituted (e.g. `[$error, $success]`, `<$success, $error>`). |
| `--service-context-mode <none\|abstract\|type>` | `none` | Inject a context parameter into every service method: `abstract` adds a generic type parameter to the service, `type` uses a concrete type name. |
| `--service-context-type <name>` | `Ctx` | Context type name (a type parameter name in `abstract` mode, a fully-qualified type in `type` mode). |
| `--service-context-parameter-name <name>` | `ctx` | Name of the injected context parameter. |
| `--pragma <key=value>` | — | Set any pragma (repeatable), e.g. `--pragma "scala.service.result.hkt=true"`. Overrides in-file pragmas. |

HKT variants (Scala and Kotlin only):

| Option | Default | Description |
|---|---|---|
| `--service-result-hkt` | `false` | Wrap service results in a higher-kinded type parameter added to the service trait/interface. |
| `--service-result-hkt-name <name>` | `F` | HKT type-parameter name. |
| `--service-result-hkt-signature <sig>` | `[+_, +_]` | HKT type-parameter signature (Kotlin would use e.g. `<*, *>`). |

### MCP servers

Each language target can additionally emit a [Model Context Protocol](https://modelcontextprotocol.io)
server per `service` definition (a `<Service>McpServer` class plus a shared
`BaboonMcpRuntime`), exposing service methods as MCP tools with JSON schemas.
Off by default; when the flag is absent the output is byte-identical to the
non-MCP baseline.

| Flag | Target |
|---|---|
| `--cs-generate-mcp-server` | `:cs` |
| `--scala-generate-mcp-server` | `:scala` |
| `--py-generate-mcp-server` | `:python` |
| `--rs-generate-mcp-server` | `:rust` |
| `--ts-generate-mcp-server` | `:typescript` |
| `--kt-generate-mcp-server` | `:kotlin` |
| `--jv-generate-mcp-server` | `:java` |
| `--dt-generate-mcp-server` | `:dart` |
| `--sw-generate-mcp-server` | `:swift` |

## Per-language options

Flag prefixes: `cs` (C#), `sc`/`scala` (Scala), `py` (Python), `rs` (Rust),
`ts` (TypeScript), `kt` (Kotlin), `jv` (Java), `dt` (Dart), `sw` (Swift).

### `:cs` — C#

| Option | Default | Description |
|---|---|---|
| `--cs-obsolete-errors` | `false` | Mark deprecated versions with `[Obsolete(..., error: true)]` (compile errors) instead of warnings. |
| `--cs-exclude-global-usings` | `false` | Do not emit `using System;`, `using System.Collections.Generic;`, `using System.Linq;` — for projects built with `ImplicitUsings` enabled. |
| `--cs-wrapped-adt-branch-codecs` | `false` | See [Codecs](#codecs). |
| `--cs-write-evolution-dict` | `false` | Emit a `BaboonMeta` evolution dictionary. |
| `--generate-index-writers` | `true` | Generate UEBA index writers (C#-specific bulk-encoding support). |
| `--deduplicate` | `true` | Deduplicate identical generated code across versions to shrink output size. |
| `--enable-deprecated-encoders` | `false` | See [Conversions and versioning](#conversions-and-versioning). |
| `--cs-async-services` | `false` | Service signatures and RPC client methods return `Task<T>`. |
| `--cs-generate-mcp-server` | `false` | See [MCP servers](#mcp-servers). |

Service result default: plain `Out` (no error wrapping).

### `:scala` — Scala

| Option | Default | Description |
|---|---|---|
| `--sc-write-evolution-dict` | `false` | Emit a `BaboonMetadata` evolution dictionary. |
| `--sc-wrapped-adt-branch-codecs` | `false` | See [Codecs](#codecs). |
| `--enable-deprecated-encoders` | `false` | See above. |
| `--service-result-hkt*` | — | See [Service generation](#service-generation). |
| `--scala-generate-mcp-server` | `false` | See [MCP servers](#mcp-servers). |

Service result default: `scala.util.Either[$error, $success]`.

### `:python` — Python

| Option | Default | Description |
|---|---|---|
| `--py-write-evolution-dict` | **`true`** | Emit an evolution dictionary (on by default for Python, unlike other backends). |
| `--py-wrapped-adt-branch-codecs` | `false` | See [Codecs](#codecs). |
| `--enable-deprecated-encoders` | `false` | See above. |
| `--py-async-services` | `false` | Service signatures and RPC client methods become `async def` coroutines. |
| `--py-generate-mcp-server` | `false` | See [MCP servers](#mcp-servers). |

Service result default: plain `Out`.

### `:rust` — Rust

| Option | Default | Description |
|---|---|---|
| `--rs-write-evolution-dict` | `false` | Emit an evolution dictionary. |
| `--rs-wrapped-adt-branch-codecs` | `false` | See [Codecs](#codecs). |
| `--rs-async-services` | `false` | Generate `async` service interfaces. |
| `--rs-crate-prefix <prefix>` | `crate` | Path prefix used for intra-crate references in generated code (set when the generated code is embedded as a module rather than a crate root). |
| `--rs-reexport-mode <all\|none\|selective>` | `selective` | How generated modules re-export their contents. |
| `--rs-edition <edition>` | `2024` | Rust edition written into the generated `Cargo.toml`. |
| `--rs-generate-mcp-server` | `false` | See [MCP servers](#mcp-servers). |

Service result default: `Result<$success, $error>`.

### `:typescript` — TypeScript

| Option | Default | Description |
|---|---|---|
| `--ts-write-evolution-dict` | `false` | Emit an evolution dictionary. |
| `--ts-wrapped-adt-branch-codecs` | `false` | See [Codecs](#codecs). |
| `--ts-import-suffix <suffix>` | empty | Suffix appended to relative import paths (e.g. `.js` for Node16/NodeNext module resolution). |
| `--ts-async-services` | `false` | Service interfaces return `Promise<T>`. |
| `--ts-bare-service-symbols` | `false` | Emit bare service symbols (`Service`, `Client`, `invokeJson`, …) instead of service-name-prefixed ones, relying on the per-service directory/barrel for namespacing. |
| `--ts-maps-as-records` | `false` | Use `Record<string, V>` instead of `Map<string, V>` for string-keyed maps. |
| `--ts-timestamps-as-strings` | `false` | Use ISO-8601 strings instead of the `BaboonDateTimeUtc` wrapper for timestamps. |
| `--ts-timestamps-as-dates` | `false` | Use `Date` objects instead of the `BaboonDateTimeUtc` wrapper. |
| `--ts-enum-lowercase-values` | `false` | Emit and accept lowercase enum wire values (pre-PR-35 TS form). Backward-compat escape hatch only: other backends do not honor it, so it **breaks cross-language wire compatibility**. |
| `--ts-generate-mcp-server` | `false` | See [MCP servers](#mcp-servers). |

Service result default: plain `Out`.

### `:kotlin` — Kotlin

| Option | Default | Description |
|---|---|---|
| `--kt-write-evolution-dict` | `false` | Emit an evolution dictionary. |
| `--kt-wrapped-adt-branch-codecs` | `false` | See [Codecs](#codecs). |
| `--enable-deprecated-encoders` | `false` | See above. |
| `--service-result-hkt*` | — | See [Service generation](#service-generation). |
| `--kt-multiplatform` | `false` | Generate Kotlin Multiplatform-compatible code instead of JVM-only. |
| `--kt-generate-mcp-server` | `false` | See [MCP servers](#mcp-servers). |

Service result default: `Either<$error, $success>`.

### `:java` — Java

| Option | Default | Description |
|---|---|---|
| `--jv-write-evolution-dict` | `false` | Emit an evolution dictionary. |
| `--jv-wrapped-adt-branch-codecs` | `false` | See [Codecs](#codecs). |
| `--enable-deprecated-encoders` | `false` | See above. |
| `--jv-async-services` | `false` | RPC client methods return `CompletableFuture<T>`. |
| `--jv-generate-mcp-server` | `false` | See [MCP servers](#mcp-servers). |

Service result default: plain `Out`.

### `:dart` — Dart

| Option | Default | Description |
|---|---|---|
| `--dt-write-evolution-dict` | `false` | Emit an evolution dictionary. |
| `--dt-wrapped-adt-branch-codecs` | `false` | See [Codecs](#codecs). |
| `--dt-generate-mcp-server` | `false` | See [MCP servers](#mcp-servers). |

Service result default: plain `Out`.

### `:swift` — Swift

| Option | Default | Description |
|---|---|---|
| `--sw-write-evolution-dict` | `false` | Emit an evolution dictionary. |
| `--sw-wrapped-adt-branch-codecs` | `false` | See [Codecs](#codecs). |
| `--sw-async-services` | `false` | Service signatures and RPC client methods use `async`/`await`. |
| `--sw-generate-mcp-server` | `false` | See [MCP servers](#mcp-servers). |

Service result default: plain `Out`.

### `:graphql` — GraphQL SDL, `:openapi` — OpenAPI 3.1

Schema-only targets: they accept the [output layout](#output-layout) options
and `--pragma`, and ignore codec/service options (no codecs are generated).
Doc comments from the model are emitted as SDL descriptions / `description`
fields.

## Other modalities

### `:scheme`

Renders one domain version back into a single, cleaned-up `.baboon` file
(definitions toposorted, includes inlined, ADT-owned and service-inline types
nested, dependency comments added).

| Option | Description |
|---|---|
| `--domain <name>` | Domain name, e.g. `my.domain.name`. |
| `--version <version>` | Version string, e.g. `1.0.0`. |
| `--target <file>` | Output file path. |

```bash
baboon --model-dir ./models :scheme --domain=my.pkg --version=1.0.0 --target=./cleaned.baboon
```

### `:lsp`

Starts the Language Server. By default it talks LSP over stdio; pass a port to
use TCP instead.

| Option | Description |
|---|---|
| `--port <port>` | TCP port to listen on (space-separated form; default: stdio). |

### `:explore`

Starts the [interactive explorer](explorer-mode.md) REPL on the models given by
the global `--model`/`--model-dir` options. No modality-specific options.

## Examples

```bash
# Generate all 9 languages plus both schema formats in one run
baboon --model-dir ./models --lock-file ./target/baboon.lock \
  :cs --output ./out/cs \
  :scala --output ./out/scala \
  :python --output ./out/py \
  :rust --output ./out/rs \
  :typescript --output ./out/ts \
  :kotlin --output ./out/kt \
  :java --output ./out/jv \
  :dart --output ./out/dt \
  :swift --output ./out/sw \
  :graphql --output ./out/gql \
  :openapi --output ./out/oas

# Codec tests with fixtures, 1000 iterations
baboon --model-dir ./models \
  :scala --output ./out/src --test-output ./out/test --codec-test-iterations 1000

# ZIO-style Scala services + plain TypeScript services
baboon --model-dir ./models \
  :scala --output ./out/scala \
    --service-result-hkt --service-result-hkt-name F \
    --service-result-pattern '[$error, $success]' \
  :typescript --output ./out/ts --ts-import-suffix .js

# MCP server generation (TypeScript)
baboon --model-dir ./models \
  :typescript --output ./out/ts --ts-generate-mcp-server=true

# Only emit one domain out of a multi-domain model tree
baboon --model-dir ./models --emit-only my.billing :cs --output ./out/cs
```
