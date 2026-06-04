# MCP Generators — Grounding Note

Verified against the current worktree source at commit `e61a5068992d1ecd58079de2fda648e12bd53c55`
(branch `implement/T1`, 2026-06-04).

All `path:line` citations are verified by direct file read.

---

## 1. Per-language `*Options` 4-site mechanism

### Site 1 — `CompilerOptions.scala` (shared cross-platform, `*Options` case classes + `CompilerTarget` sealed trait)

`baboon-compiler/src/main/scala/io/septimalmind/baboon/CompilerOptions.scala`

| Symbol | Lines |
|---|---|
| `sealed trait CompilerTarget` | 7 |
| `CompilerTarget.CSTarget` | 15–21 |
| `CompilerTarget.ScTarget` | 23–28 |
| `CompilerTarget.PyTarget` | 29–34 |
| `CompilerTarget.RsTarget` | 35–40 |
| `CompilerTarget.TsTarget` | 43–48 |
| `CompilerTarget.KtTarget` | 50–55 |
| `CompilerTarget.JvTarget` | 57–62 |
| `CompilerTarget.DtTarget` | 64–69 |
| `CompilerTarget.SwTarget` | 71–76 |
| `CompilerTarget.GqlTarget` | 78–83 |
| `CompilerTarget.OasTarget` | 85–90 |
| `final case class CSOptions(...)` | 205–224 |
| `final case class ScOptions(...)` | 191–203 |
| `final case class PyOptions(...)` | 176–189 |
| `final case class RsOptions(...)` | 226–241 |
| `final case class TsOptions(...)` | 243–273 |
| `final case class KtOptions(...)` | 275–288 |
| `final case class JvOptions(...)` | 290–303 |
| `final case class DtOptions(...)` | 305–316 |
| `final case class SwOptions(...)` | 318–330 |
| `final case class GqlOptions(...)` | 332–334 |
| `final case class OasOptions(...)` | 336–338 |
| `ServiceContextConfig` case class | 162–166 |
| `ServiceResultConfig` case class | 98–103 |
| `CompilerOptions` final case class | 390–398 |

Note: `GqlOptions` and `OasOptions` carry only `pragmas: Map[String, String]`.
No `serviceResult`/`serviceContext` fields — GQL/OAS skip service wiring entirely.
**This is where a new `McpOptions` case class would be added when implementing the MCP backend.**

### Site 2 — `.jvm/src/main/scala/…/CLIOptions.scala` (JVM CLI option case classes)

`baboon-compiler/.jvm/src/main/scala/io/septimalmind/baboon/CLIOptions.scala`

| Symbol | Lines |
|---|---|
| `GenericTranspilerCLIOptions` case class | 5–26 |
| `SharedCLIOptions` trait | 28–38 |
| `CsCLIOptions` | 46–91 |
| `ScCLIOptions` | 93–135 |
| `PyCLIOptions` | 137–174 |
| `RsCLIOptions` | 176–217 |
| `TsCLIOptions` | 219–266 |
| `KtCLIOptions` | 268–312 |
| `JvCLIOptions` | 314–351 |
| `DtCLIOptions` | 353–386 |
| `GqlCLIOptions` | 388–407 |
| `OasCLIOptions` | 409–428 |
| `SwCLIOptions` | 439–474 |
| `CLIOptions` (global options) | 476–489 |

`GqlCLIOptions` and `OasCLIOptions` implement `SharedCLIOptions` stubs (all service-related fields are `Option[Boolean]`/`Option[String]` marked "Unused, present for CLI compatibility"). A new `McpCLIOptions` would follow the same pattern — no service wiring fields needed.

### Site 3 — `.jvm/src/main/scala/…/Baboon.scala` (JVM target construction / `processTarget` dispatch)

`baboon-compiler/.jvm/src/main/scala/io/septimalmind/baboon/Baboon.scala`

| Symbol | Lines |
|---|---|
| `sharedArgNames` set (shared arg names excluded from per-language help) | 34–62 |
| `formatLanguageHelp` helper | 74–92 |
| `helpText` string (all language sections) | 94–185 |
| `main` — multi-modal parse loop | 187–598 |
| `"cs"` match arm → `CompilerTarget.CSTarget(...)` construction | 287–317 |
| `"scala"` match arm → `CompilerTarget.ScTarget(...)` | 318–343 |
| `"python"` match arm → `CompilerTarget.PyTarget(...)` | 344–368 |
| `"rust"` match arm → `CompilerTarget.RsTarget(...)` | 369–395 |
| `"typescript"` match arm → `CompilerTarget.TsTarget(...)` | 396–431 |
| `"kotlin"` match arm → `CompilerTarget.KtTarget(...)` | 432–456 |
| `"java"` match arm → `CompilerTarget.JvTarget(...)` | 457–481 |
| `"dart"` match arm → `CompilerTarget.DtTarget(...)` | 482–504 |
| `"swift"` match arm → `CompilerTarget.SwTarget(...)` | 505–528 |
| `"graphql"` match arm → `CompilerTarget.GqlTarget(...)` | 529–542 |
| `"openapi"` match arm → `CompilerTarget.OasTarget(...)` | 543–556 |
| `processTarget` (module dispatch by `CompilerTarget` sub-type) | 669–720 |
| `mkServiceContext` helper | 642–648 |
| `mkServiceResult` helper | 650–667 |

**Adding a new `:mcp` modality requires: (a) a new `"mcp"` match arm near line 543, (b) extending `processTarget` match, (c) adding a `BaboonJvmMcpModule` import/creation.**

### Site 4 — `.js/src/main/scala/…/BaboonJS.scala` (JS/playground `createTargets` dispatch)

`baboon-compiler/.js/src/main/scala/io/septimalmind/baboon/BaboonJS.scala`

| Symbol | Lines |
|---|---|
| `JSCSOptions` native trait | 248–271 |
| `JSScOptions` native trait | 273–293 |
| `JSCommonLangOptions` native trait (covers py/rs/ts/kt/jv/dt/sw) | 295–315 |
| `JSGenericOptions` native trait | 317–326 |
| `JSCompilerTarget` native trait | 328–341 |
| `createTargets` function (language dispatch) | 415–643 |
| `"cs"` match arm | 419–447 |
| `"scala"` match arm | 448–475 |
| `"python"` match arm | 476–496 |
| `"rust"` match arm | 497–519 |
| `"typescript"` match arm | 520–545 |
| `"kotlin"` match arm | 546–566 |
| `"java"` match arm | 567–587 |
| `"dart"` match arm | 588–606 |
| `"swift"` match arm | 607–626 |
| `"graphql"` match arm (no options, just `GqlOptions(pragmas = Map.empty)`) | 627–633 |
| `"openapi"` match arm (no options, just `OasOptions(pragmas = Map.empty)`) | 634–640 |
| `processTarget` JS variant (exhaustive match) | 1055–1076 |
| `toCompilerTargets` (maps `CompilerTargetJS` → `CompilerTarget`) | 776–790 |

The JS playground does NOT expose per-language options for graphql/openapi. A new MCP target would need: a new match arm in `createTargets`, a new `CompilerTargetJS.McpTarget` sealed case, and extension of `processTarget` / `toCompilerTargets`.

### Site 5 — Playground `options.ts` / `compiler.ts`

`baboon-playground/src/compiler.ts`

| Symbol | Lines |
|---|---|
| `ALL_LANGUAGES` const array (all 11 targets including graphql/openapi) | 149–161 |
| `TARGET_LANGUAGES` exported const | 165 |
| `LANGUAGE_DISPLAY_NAMES` record | 167–179 |
| `LANGUAGE_TO_MONACO` record | 181–193 |
| `EXTENSION_TO_LANGUAGE` record (no openapi entry — detected by filename) | 195–206 |
| `detectLanguageByPath` (special-cases `openapi.json`) | 208–218 |
| `SCHEMA_ONLY_LANGUAGES` set (`"graphql"`, `"openapi"`) | 392 |

`baboon-playground/src/options.ts`

| Symbol | Lines |
|---|---|
| `SCHEMA_ONLY_LANGUAGES` (graphql, openapi — no codec/evolution options) | 8 |
| `CODEC_LANGUAGES` (all minus schema-only) | 9 |
| `LanguageOptions` interface (codec + service options, NOT extended for graphql/openapi) | 23–58 |
| `SERVICE_RESULT_DEFAULTS` per-language map (includes graphql/openapi stubs at lines 88–89) | 78–90 |
| `DEFAULT_LANGUAGE_OPTIONS` exported const | 92–… |

**A new MCP target would need: `ALL_LANGUAGES` array extended (compiler.ts:149), `LANGUAGE_DISPLAY_NAMES`/`LANGUAGE_TO_MONACO` records extended, and the MCP target added to `SCHEMA_ONLY_LANGUAGES` (options.ts:8) if it has no codec options, or to the codec path otherwise.**

---

## 2. Transport-abstract service-wiring runtime contract

All runtimes follow the same two-tier pattern:
- **Tier 1** (`IBaboonJsonService<R>` / `IBaboonUebaService<R>`): context-free, for simple no-context invocation.
- **Tier 2** (`IBaboonJsonServiceCtx<Ctx, R>` / `IBaboonUebaServiceCtx<Ctx, R>`): per-invocation context parameter, for the `service.context=abstract` mode.

### C# — `baboon-compiler/src/main/resources/baboon-runtime/cs/BaboonServiceWiring.cs`

| Symbol | Line |
|---|---|
| `IBaboonJsonService<out R>` interface | 67 |
| `IBaboonUebaService<out R>` interface | 73 |
| `IBaboonJsonServiceCtx<Ctx, out R>` interface | 157 |
| `IBaboonUebaServiceCtx<Ctx, out R>` interface | 163 |
| `Invoke` signature on `IBaboonJsonServiceCtx` | 160: `R Invoke(BaboonMethodId method, string data, Ctx ctx, BaboonCodecContext codecCtx)` |
| `Invoke` signature on `IBaboonUebaServiceCtx` | 166: `R Invoke(BaboonMethodId method, byte[] data, Ctx ctx, BaboonCodecContext codecCtx)` |
| `JsonMuxerCtx<Ctx, R>` class | 171 |
| `UebaMuxerCtx<Ctx, R>` class | 207 |

### Dart — `baboon-compiler/src/main/resources/baboon-runtime/dart/baboon_runtime.dart`

| Symbol | Line |
|---|---|
| `IBaboonJsonService<R>` abstract interface | 936 |
| `IBaboonUebaService<R>` abstract interface | 941 |
| `IBaboonJsonServiceCtx<Ctx, R>` abstract interface | 1008 |
| `IBaboonUebaServiceCtx<Ctx, R>` abstract interface | 1013 |
| `invoke` on `IBaboonJsonServiceCtx` | 1010: `R invoke(BaboonMethodId method, String data, Ctx ctx, BaboonCodecContext codecCtx)` |
| `invoke` on `IBaboonUebaServiceCtx` | 1015: `R invoke(BaboonMethodId method, Uint8List data, Ctx ctx, BaboonCodecContext codecCtx)` |
| `JsonMuxerCtx<Ctx, R>` class | 1017 |
| `UebaMuxerCtx<Ctx, R>` class | 1044 |

### Java — `baboon-compiler/src/main/resources/baboon-runtime/java/`

| File | Symbol | Line |
|---|---|---|
| `IBaboonJsonService.java` | `IBaboonJsonService<R>` interface | 22 |
| `IBaboonUebaService.java` | `IBaboonUebaService<R>` interface | 14 |
| `IBaboonJsonServiceCtx.java` | `IBaboonJsonServiceCtx<Ctx, R>` interface | 15 |
| `IBaboonJsonServiceCtx.java` | `invoke` signature | 17: `R invoke(BaboonMethodId method, String data, Ctx ctx, BaboonCodecContext codecCtx) throws Exception` |
| `IBaboonUebaServiceCtx.java` | `IBaboonUebaServiceCtx<Ctx, R>` interface | 13 |
| `IBaboonUebaServiceCtx.java` | `invoke` signature | 15: `R invoke(BaboonMethodId method, byte[] data, Ctx ctx, BaboonCodecContext codecCtx) throws Exception` |
| `JsonMuxerCtx.java` | `JsonMuxerCtx<Ctx, R>` class | 19 |
| `UebaMuxerCtx.java` | `UebaMuxerCtx<Ctx, R>` class | 19 |

### Kotlin — `baboon-compiler/src/main/resources/baboon-runtime/kotlin/BaboonServiceWiring.kt`

| Symbol | Line |
|---|---|
| `IBaboonJsonService<R>` interface | 26 |
| `IBaboonUebaService<R>` interface | 31 |
| `IBaboonJsonServiceCtx<Ctx, R>` interface | 91 |
| `IBaboonUebaServiceCtx<Ctx, R>` interface | 96 |
| `invoke` on `IBaboonJsonServiceCtx` | 93: `fun invoke(method: BaboonMethodId, data: String, ctx: Ctx, codecCtx: BaboonCodecContext): R` |
| `invoke` on `IBaboonUebaServiceCtx` | 98: `fun invoke(method: BaboonMethodId, data: ByteArray, ctx: Ctx, codecCtx: BaboonCodecContext): R` |
| `JsonMuxerCtx<Ctx, R>` | 101 |
| `UebaMuxerCtx<Ctx, R>` | 124 |

(Kotlin-KMP runtime at `kotlin-kmp/BaboonServiceWiring.kt` is identical at the same line numbers.)

### Python — `baboon-compiler/src/main/resources/baboon-runtime/python/baboon_service_wiring.py`

| Symbol | Line |
|---|---|
| `IBaboonJsonServiceCtx` Protocol | 193 |
| `IBaboonUebaServiceCtx` Protocol | 200 |
| `invoke` on `IBaboonJsonServiceCtx` | 196: `def invoke(self, method: BaboonMethodId, data: str, ctx: Ctx, codec_ctx) -> R` |
| `invoke` on `IBaboonUebaServiceCtx` | 203: `def invoke(self, method: BaboonMethodId, data: bytes, ctx: Ctx, codec_ctx) -> R` |
| `JsonMuxerCtx` class | 207 |
| `UebaMuxerCtx` class | 238 |

### Rust — `baboon-compiler/src/main/resources/baboon-runtime/rust/baboon_service_wiring.rs`

| Symbol | Line |
|---|---|
| `IBaboonJsonService<R>` trait | 53 |
| `IBaboonUebaService<R>` trait | 58 |
| `IBaboonJsonServiceCtx<Ctx, R>` trait | 151 |
| `IBaboonUebaServiceCtx<Ctx, R>` trait | 156 |
| `fn invoke` on `IBaboonJsonServiceCtx` | 153: `fn invoke(&self, method: &BaboonMethodId, data: &str, ctx: Ctx, codec_ctx: &BaboonCodecContext) -> R` |
| `fn invoke` on `IBaboonUebaServiceCtx` | 158: `fn invoke(&self, method: &BaboonMethodId, data: &[u8], ctx: Ctx, codec_ctx: &BaboonCodecContext) -> R` |
| `JsonMuxerCtx<Ctx, R>` struct | 160 |
| `UebaMuxerCtx<Ctx, R>` struct | 199 |

### Scala — `baboon-compiler/src/main/resources/baboon-runtime/scala/BaboonServiceWiring.scala`

| Symbol | Line |
|---|---|
| `IBaboonJsonService[R]` trait | 28 |
| `IBaboonUebaService[R]` trait | 33 |
| `IBaboonJsonServiceCtx[Ctx, R]` trait | 89 |
| `IBaboonUebaServiceCtx[Ctx, R]` trait | 94 |
| `invoke` on `IBaboonJsonServiceCtx` | 91: `def invoke(method: BaboonMethodId, data: String, ctx: Ctx, codecCtx: BaboonCodecContext): R` |
| `invoke` on `IBaboonUebaServiceCtx` | 96: `def invoke(method: BaboonMethodId, data: Array[Byte], ctx: Ctx, codecCtx: BaboonCodecContext): R` |
| `JsonMuxerCtx[Ctx, R]` class | 99 |
| `UebaMuxerCtx[Ctx, R]` class | 120 |

### Swift — `baboon-compiler/src/main/resources/baboon-runtime/swift/baboon_service_wiring.swift`

| Symbol | Line |
|---|---|
| `IBaboonJsonServiceCtx` protocol (associated types `Ctx`, `R`) | 171 |
| `IBaboonUebaServiceCtx` protocol | 178 |
| `invoke` on `IBaboonJsonServiceCtx` | 175: `func invoke(_ method: BaboonMethodId, _ data: String, _ ctx: Ctx, _ codecCtx: BaboonCodecContext) throws -> R` |
| `invoke` on `IBaboonUebaServiceCtx` | 182: `func invoke(_ method: BaboonMethodId, _ data: Data, _ ctx: Ctx, _ codecCtx: BaboonCodecContext) throws -> R` |

Note: Swift uses associated-type protocols instead of generic type parameters (language constraint). `IBaboonJsonService<R>` without context is at lines 43/49.

### TypeScript — `baboon-compiler/src/main/resources/baboon-runtime/typescript/BaboonSharedRuntime.ts`

| Symbol | Line |
|---|---|
| `IBaboonJsonService<R>` interface | 690 |
| `IBaboonUebaService<R>` interface | 695 |
| `IBaboonJsonServiceCtx<Ctx, R>` interface | 752 |
| `IBaboonUebaServiceCtx<Ctx, R>` interface | 757 |
| `invoke` on `IBaboonJsonServiceCtx` | 754: `invoke(method: BaboonMethodId, data: string, ctx: Ctx, codecCtx: BaboonCodecContext): R` |
| `invoke` on `IBaboonUebaServiceCtx` | 759: `invoke(method: BaboonMethodId, data: Uint8Array, ctx: Ctx, codecCtx: BaboonCodecContext): R` |
| `JsonMuxerCtx<Ctx, R>` class | 763 |
| `UebaMuxerCtx<Ctx, R>` class | 785 |

### `ServiceContextResolver` (shared Scala codegen helper)

`baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/ServiceContextResolver.scala`

| Symbol | Line |
|---|---|
| `ResolvedServiceContext` sealed trait | 6 |
| `NoContext` case object | 8 |
| `AbstractContext(typeName, parameterName)` case class | 9 |
| `ConcreteContext(typeName, parameterName)` case class | 10 |
| `ServiceContextResolver.resolve(...)` method | 47 |

Languages supported by `ServiceContextResolver`: `cs`, `dart`, `java`, `kotlin`, `python`, `rust`, `scala`, `swift`, `typescript` (line 14).

### Per-language `*ServiceWiringTranslator.scala` files

All located under `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/`:

| File path | Note |
|---|---|
| `csharp/CSServiceWiringTranslator.scala` | C# |
| `dart/DtServiceWiringTranslator.scala` | Dart |
| `java/JvServiceWiringTranslator.scala` | Java |
| `kotlin/KtServiceWiringTranslator.scala` | Kotlin |
| `python/PyServiceWiringTranslator.scala` | Python |
| `rust/RsServiceWiringTranslator.scala` | Rust |
| `scl/ScServiceWiringTranslator.scala` | Scala (note: `scl/` not `scala/`) |
| `swift/SwServiceWiringTranslator.scala` | Swift |
| `typescript/TsServiceWiringTranslator.scala` | TypeScript |

No service wiring translators exist for `graphql/` or `openapi/` — consistent with their schema-only status.

---

## 3. OpenAPI schema emitter surface

### Entry point

`baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/openapi/OasBaboonTranslator.scala`

| Symbol | Line |
|---|---|
| `OasBaboonTranslator[F]` class (extends `BaboonAbstractTranslator[F]`) | 69–72 |
| `translate(family: BaboonFamily)` override | 74 |
| `translateDomain(domain: Domain)` private | 91 |
| `renderTypedef(...)` private | 141 |
| `renderDto(...)` private | 155 |
| `renderEnum(...)` private | 199 |
| `renderAdt(...)` private | 207 |
| `renderForeignSchema(...)` private | 250 |
| `isOptional(ref: TypeRef)` private | 256 |

Output filename pattern: `$pkgStr/v$versionStr/openapi.json` (line 136).

### Supported type mappings (from doc-comment lines 13–68)

| Baboon type | OpenAPI/JSON Schema output |
|---|---|
| `Typedef.Dto` | `object` with `properties` and `required` |
| `Typedef.Enum` | `string` with `enum` keyword |
| `Typedef.Adt` | `oneOf` referencing each branch schema |
| `Typedef.Foreign` with `rt` | Resolved to underlying Baboon type |
| `Typedef.Foreign` without `rt` | Opaque `{"type": "object", "description": "Foreign type: …"}` |
| `Typedef.Service` / `Typedef.Contract` (`NonDataTypedef`) | **Skipped** |
| Type aliases | Transparent (resolved by typer before codegen) |

### Cannot render / limitations

- Service and contract types are skipped (line 107: `case _: Typedef.NonDataTypedef => None`).
- `paths` is always empty — output is a component-schema library only.
- No discriminator `mapping` for ADTs.
- `f128` → `string/decimal` (JSON cannot carry 128-bit precision).
- `tsu` and `tso` both → `date-time` (UTC vs offset distinction lost).
- ADT branch schemas are emitted inline alongside their parent ADT — no cross-document `$ref` support.

### Companion type translator

`baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/openapi/OasTypeTranslator.scala`

Provides: `renderOasDescription`, `foreignTypeResolution`, `resolveTypeRef`, `typeRefSchema`, `schemaName`, `escapeJson`, `sanitize`.

---

## 4. Wiring-overlay test harness layout

### Overlay directories under `test/`

| Language | Overlay directory | Used by |
|---|---|---|
| C# — Either | `test/cs-stub-either-overlay/` | `test-gen-cs-wiring-either` |
| C# — Result | `test/cs-stub-result-overlay/` | `test-gen-cs-wiring-result` |
| C# — Outcome | `test/cs-stub-outcome-overlay/` | `test-gen-cs-wiring-outcome` |
| Scala — Either | `test/sc-stub-either-overlay/` | `test-gen-sc-wiring-either` |
| Scala — Result | `test/sc-stub-result-overlay/` | `test-gen-sc-wiring-result` |
| Scala — Outcome | `test/sc-stub-outcome-overlay/` | `test-gen-sc-wiring-outcome` |
| Scala — HKT | `test/sc-stub-hkt-overlay/` | `test-gen-sc-wiring-hkt` |
| TypeScript — Either | `test/ts-stub-either-overlay/` | `test-gen-ts-wiring-either` |
| TypeScript — Result | `test/ts-stub-result-overlay/` | `test-gen-ts-wiring-result` |
| TypeScript — Outcome | `test/ts-stub-outcome-overlay/` | `test-gen-ts-wiring-outcome` |
| Rust — Either | `test/rs-stub-either-overlay/` | `test-gen-rs-wiring-either` |
| Rust — Result | `test/rs-stub-result-overlay/` | `test-gen-rs-wiring-result` |
| Rust — Outcome | `test/rs-stub-outcome-overlay/` | `test-gen-rs-wiring-outcome` |
| Python — Either | `test/py-stub-either-overlay/` | `test-gen-py-wiring-either` |
| Python — Result | `test/py-stub-result-overlay/` | `test-gen-py-wiring-result` |
| Python — Outcome | `test/py-stub-outcome-overlay/` | `test-gen-py-wiring-outcome` |
| Kotlin | `test/kt-stub-wiring-overlay/` | `test-gen-kt-wiring` |
| Java | `test/jv-stub-wiring-overlay/` | `test-gen-jv-wiring` |
| Dart | `test/dt-stub-wiring-overlay/` | `test-gen-dt-wiring` |
| Swift | `test/sw-stub-wiring-overlay/` | `test-gen-sw-wiring`, `test-gen-sw-wiring-async`, `test-gen-sw-wiring-errors` |

Note: Kotlin, Java, Dart, and Swift use a **single** wiring overlay (no-errors service-result mode by default, matching the service-acceptance harness). C#, Scala, TypeScript, Rust, and Python each have **three** overlays (Either/Result/Outcome) plus Python adds an async overlay.

**No wiring overlays exist yet for GraphQL or OpenAPI** — they are schema-only backends.

### `test-gen-*-wiring` / `test-*-wiring` action pairs in `.mdl/defs/tests.md`

(All line numbers in `.mdl/defs/tests.md`)

| Action pair | Lines |
|---|---|
| `test-gen-cs-wiring-either` / `test-cs-wiring-either` | 1094 / 1131 |
| `test-gen-cs-wiring-result` / `test-cs-wiring-result` | 1145 / 1182 |
| `test-gen-cs-wiring-outcome` / `test-cs-wiring-outcome` | 1196 / 1233 |
| `test-gen-sc-wiring-either` / `test-sc-wiring-either` | 1247 / 1282 |
| `test-gen-sc-wiring-result` / `test-sc-wiring-result` | 1295 / 1330 |
| `test-gen-sc-wiring-outcome` / `test-sc-wiring-outcome` | 1343 / 1378 |
| `test-gen-sc-wiring-hkt` / `test-sc-wiring-hkt` | 1391 / 1429 |
| `test-gen-ts-wiring-either` / `test-ts-wiring-either` | 1442 / 1478 |
| `test-gen-ts-wiring-result` / `test-ts-wiring-result` | 1492 / 1528 |
| `test-gen-ts-wiring-outcome` / `test-ts-wiring-outcome` | 1542 / 1578 |
| `test-gen-rs-wiring-either` / `test-rs-wiring-either` | 1592 / 1628 |
| `test-gen-rs-wiring-result` / `test-rs-wiring-result` | 1641 / 1678 |
| `test-gen-rs-wiring-outcome` / `test-rs-wiring-outcome` | 1691 / 1728 |
| `test-gen-py-wiring-either` / `test-py-wiring-either` | 1741 / 1776 |
| `test-gen-py-wiring-result` / `test-py-wiring-result` | 1792 / 1827 |
| `test-gen-py-wiring-outcome` / `test-py-wiring-outcome` | 1843 / 1878 |
| `test-py-wiring-async` (run only, no separate gen) | 1894 |
| `test-gen-kt-wiring` / `test-kt-wiring` | 1931 / 1966 |
| `test-gen-jv-wiring` / `test-jv-wiring` | 1979 / 2013 |
| `test-gen-dt-wiring` / `test-dt-wiring` | 2026 / 2073 |
| `test-gen-sw-wiring` / `test-sw-wiring` | 2087 / 2133 |
| `test-gen-sw-wiring-async` / `test-sw-wiring-async` | 2154 / 2185 |
| `test-gen-sw-wiring-errors` / `test-sw-wiring-errors` | 2208 / 2243 |
| `test-gen-cs-wiring-async` | 2489 |

### `:test` aggregator action in `.mdl/defs/tests.md`

Lines 2342–2405. Depends on all wiring test actions listed above (lines 2380–2402). A new `test-gen-mcp-wiring` / `test-mcp-wiring` action pair would need to be added to `tests.md` and a corresponding `dep action.test-mcp-wiring` line added to the `:test` aggregator.

---

## 5. Discrepancies vs. plan grounding

The following items either differ from expectations stated in the task description or represent novel findings:

1. **No `McpOptions` or `McpTarget` exists.** The task description asks to ground the "4-site flags" mechanism generically. GQL and OAS are the most recent additions (confirmed present). MCP does not yet exist in any source file — it is a planned addition.

2. **`GqlCLIOptions` / `OasCLIOptions` implement `SharedCLIOptions` with stub service fields.** The task description implies these exist. Confirmed — they are at `CLIOptions.scala:388-407` and `409-428`. The stub fields (`serviceResultNoErrors`, etc.) are marked "Unused, present for CLI compatibility" in the help text.

3. **JS-side `createTargets` uses a simplified approach for graphql/openapi.** Neither `JSCSOptions` nor `JSCommonLangOptions` covers graphql/openapi — those targets are created with `GqlOptions(pragmas = Map.empty)` / `OasOptions(pragmas = Map.empty)` directly (BaboonJS.scala:627–640). No separate JS native trait exists for them. A new MCP target would follow the same simplified pattern unless it needs options.

4. **`ServiceContextResolver` covers 9 languages (cs, dart, java, kotlin, python, rust, scala, swift, typescript).** GraphQL and OpenAPI are intentionally absent (line 14 of `ServiceContextResolver.scala`). MCP would need to be added if it supports the `service.context` pragma.

5. **Swift uses associated-type protocols, not generic type parameters, for `IBaboonJsonServiceCtx` / `IBaboonUebaServiceCtx`.** Swift's `baboon_service_wiring.swift:171-182` defines these as protocols with `associatedtype Ctx` and `associatedtype R`, unlike all other languages which use generic type parameters directly. This is a known language constraint but means the MCP runtime contract (if Swift-sourced) would need the same pattern.

6. **`scl/` (not `scala/`) subdirectory for Scala service wiring translator.** The `ScServiceWiringTranslator.scala` is under `translator/scl/`, not `translator/scala/`. This is an internal naming quirk. The full path is: `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/scl/ScServiceWiringTranslator.scala`.

7. **Playground `options.ts` does NOT have a `SERVICE_RESULT_DEFAULTS` entry for a future `mcp` language.** Currently the map at lines 78–90 covers exactly the 11 languages in `ALL_LANGUAGES`. Extending to MCP requires adding an entry here.

8. **`test/py-stub-either-overlay/` etc. directories confirmed present; wiring overlay naming is NOT `{lang}-stub-*-wiring-*-overlay/` but `{lang}-stub-{variant}-overlay/` (for cs/sc/ts/rs/py) and `{lang}-stub-wiring-overlay/` (for kt/jv/dt/sw).** The asymmetry is intentional: kt/jv/dt/sw use a single no-errors mode.
