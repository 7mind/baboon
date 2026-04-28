# Triage Plan: Baboon Upstream Defects (2026-04-28)

Source: `baboon-upstream-defects-clean.md` at the repo root.

## Summary of triage outcome

Of the 14 defects: **11 REAL** (S01, S02, K01, K02, K03, K04, T01, T02, W01, W02, W03, W04 — counted as 12; see below), **2 MISDIAGNOSED/STALE** (S04, C01), **1 UNVERIFIED** (K05). Specifically: BAB-S04's claim that the codec-default flags are "absent from `--help`" is wrong — they carry `@HelpMessage` annotations in `CLIOptions.scala`. BAB-C01's claim that `--omit-most-recent-version-suffix-*` flags were removed is wrong — both flags are still present in `GenericTranspilerCLIOptions`. BAB-K05 ("one redundant conversion call") cannot be located without the user's reproducer; UNVERIFIED. The remaining 11 are reproducible in `main` with concrete file:line evidence.

---

## Triage table

| ID | Verdict | Evidence (file:line) | Proposed fix |
|---|---|---|---|
| **BAB-S01** | REAL | `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/scl/ScDefnTranslator.scala:80-91` — wiring uses `trans.toScPkg(domain.id, domain.version, evo)` (root pkg) regardless of `defn.id.owner`; `getOutputPath` (line 405) puts file at `<fbase>/<ns>.<File>_Wiring.scala` so file path is ns-aware while emitted `package` declaration is root-only. `mainOutput` (line 75) correctly uses `srcRef.pkg`. | In `doTranslate` (ScDefnTranslator.scala:80-91), replace `val pkg = trans.toScPkg(domain.id, ...)` with `val pkg = srcRef.pkg` (or compute pkg from `defn.id.owner` matching ns). Same change-shape as `mainOutput`. |
| **BAB-S02** | REAL | `ScDefnTranslator.scala:274-290` emits enum members as `m.name.capitalize` (case objects). `ScUEBACodecGenerator.scala:300-305` references `case $name.${m.name}` (raw name, not capitalized). The two diverge for any non-Pascal-case source identifier. | Centralize enum member identifier transformation. Either: (a) add a helper `def enumMemberIdent(m: EnumMember): String = m.name.capitalize` in a shared place (e.g. `ScDomainTreeTools`) and use it from both `ScDefnTranslator.genEnum*` and `ScUEBACodecGenerator.genEnumBodies`; or (b) fix `ScUEBACodecGenerator.scala:303-304` to emit `$name.${m.name.capitalize}`. The `parse(...)` companion path in `ScJsonCodecGenerator.scala:244-255` already routes through the capitalized companion case-arms, so JSON is consistent once UEBA is aligned. |
| **BAB-S04** | MISDIAGNOSED | `baboon-compiler/.jvm/src/main/scala/io/septimalmind/baboon/CLIOptions.scala:100-107` (ScCLIOptions) — both `generateUebaCodecsByDefault` and `generateJsonCodecsByDefault` carry `@HelpMessage` annotations (line 104, 106), as do all 9 backend variants. caseapp renders these in `--help`. | No code change. Reclassify as "cannot reproduce against current source"; possibly a stale CLI binary in the user's PATH. |
| **BAB-T01** | REAL (3 sub-issues) | (a) `TsBaboonTranslator.scala:166,169,176,179,185,213` emit `import {...}` only — never `import type`. (b) ADT branch encoders' `match` flow: ADT branch encoders in `TsUEBACodecGenerator.scala`/`TsJsonCodecGenerator.scala` need verifying for explicit returns. (c) Runtime: `BaboonSharedRuntime.ts:106-110` — `readByte(): number { const v = this.buf[this.pos]; this.pos += 1; return v; }` — under `noUncheckedIndexedAccess`, `this.buf[this.pos]` is `number \| undefined`, but the return is annotated `number`. | (a) Partition imports into "type-only" vs "value-bearing" and emit `import type` for the first set. (b) Append a `default: throw new Error(\`unhandled ADT branch …\`)` arm to ADT switches. (c) `BaboonSharedRuntime.ts` `readByte()` — bounds-check at the start, throw a `BaboonCodecError` on underflow. |
| **BAB-T02** | REAL (same root cause as T01a) | `TsDefnTranslator.scala:386,434` emit `export interface ${name.name}` for record/contract types; consumers of `BaboonSharedRuntime.ts` and per-domain modules then `import { Foo }` at value position, which Rolldown/esbuild resolve as `MISSING_EXPORT`. | Pair with T01a: when emitting an interface-only export, also emit a runtime stub (`export const ${name} = Object.freeze({})`) — OR emit `export type { Foo }` and require consumers `import type`. Lower-friction: runtime-stub. |
| **BAB-K01** | REAL | `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/kotlin/KtDefnTranslator.scala:81-92` — wiring uses `trans.toKtPkg(domain.id, domain.version, evo)` (root pkg). `getOutputPath` at line 444-453 puts file at `<fbase>/<ns>/<File>Wiring.kt` (ns-aware path). Service trait uses `srcRef.pkg` at line 76 (ns-aware). Same client wiring at lines 94-105 has the same bug. | Replace both `trans.toKtPkg(domain.id, domain.version, evo)` calls with the ns-aware `srcRef.pkg`. |
| **BAB-K02** | REAL | `KtUEBACodecGenerator.scala:114` emits `override fun encode(ctx, writer, value: $name)`. Parent in `BaboonCodecs.kt:77` declares `fun encode(ctx, writer, instance: T)`. Param name diverges. JSON-side also has parallel mismatch. | Harmonize parameter names. Safer to rename codegen output to `instance` at `KtUEBACodecGenerator.scala:114` and the corresponding JSON codec line. |
| **BAB-K03** | REAL | `KtUEBACodecGenerator.scala:201-205` (ADT branch encode body) emits `is $fqBranch -> { val $castedName = value as $fqBranch; … }`. Inside `is` arm, Kotlin smart-casts `value`; the `as` is provably redundant. | Drop the `value as $fqBranch` cast. Bind `val $castedName = value` (or use `value` directly). |
| **BAB-K04** | REAL | `KtUEBACodecGenerator.scala:518` — `mkEncoder(c.args.head, q"$ref!!", wref)` applies `!!` to a non-null receiver. JSON-side `KtJsonCodecGenerator.scala:382` is on `Map[String, JsonElement?]` index (legitimately nullable, leave alone). | When receiver is statically non-null, drop the `!!`. Use `q"$ref"` instead of `q"$ref!!"`. |
| **BAB-K05** | UNVERIFIED | "one site" — not locatable without the user's exact reproducer. | Defer until reporter supplies a sample model + the exact warning location. |
| **BAB-W01** | REAL | `SwJsonCodecGenerator.scala:175` — `q"$escaped: try ${mkDecoder(f.name.name, f.tpe, q"jsonObj")}"` prepends `try ` at the field-assignment level. Inner `mkDecoder` for `Typedef.User` emits `q"try $targetTpe.instance.decode(ctx, $ref)"` (line 300, 354). Composition yields `field: try try Codec.decode(...)`. Comment at line 397-404 dismisses this — but Swift emits a *warning* per site. | Drop the outer `try ` from the decFields template (let inner `mkDecoder` carry the `try`). Audit `mkDecoder` to emit `try` only on throwing branches. |
| **BAB-W02** | REAL | `SwJsonCodecGenerator.scala:367` — passes `v!` into `decodeElement`; for `str` the inner emits `v! as! String`. | Change `q"v!"` to `q"v"` (the inner `decodeElement` then emits `v as! String`, sound — closure body proved `v != nil`). |
| **BAB-W03** | REAL (same root cause as W01) | Three classes, all rooted in `SwJsonCodecGenerator.scala:175` always-prepend-`try`. | Same fix as W01 — track a `mayThrow` boolean from `mkDecoder`/`decodeElement` returns; emit `try ` only when `mayThrow == true`. |
| **BAB-W04** | REAL (high severity) | `baboon-compiler/src/main/resources/baboon-runtime/swift/baboon_runtime.swift:506-518` (`readString`) — `subdata(in:)` slice without bounds-check (traps on overrun) + `String(data:encoding:.utf8)!` force-unwrap. Same file:521-526 (`readBytes`), :604-609 (`readUuid`). | Make reader entry points `throws`. Pre-check `pos + length <= data.count` and `throw BaboonCodecError.truncated`. Replace force-unwrap with `guard let s = String(data: bytes, encoding: .utf8) else { throw BaboonCodecError.invalidUtf8 }`. Add `BaboonCodecError`. **API-shape change** — every Swift codec call site needs `try`. |
| **BAB-C01** | MISDIAGNOSED | `CLIOptions.scala:21-23` — `omitMostRecentVersionSuffixFromPaths` and `omitMostRecentVersionSuffixFromNamespaces` are still present in `GenericTranspilerCLIOptions`. | No code change. Reporter may have been on a transient pre-release. Close as "flags still present". |

---

## PR breakdown

PRs are arranged so backends are independent and can be parallelised. Within a backend, fixes share files and should be sequenced.

### PR-27 — Scala backend (BAB-S01, BAB-S02)
- **Scope:** `ScDefnTranslator.scala` (S01 wiring pkg, S02 enum case-object), `ScUEBACodecGenerator.scala` (S02 enum codec arms).
- **Dependencies:** none.
- **Success criteria:** `mdl :test-scala-regular` green; integration test with a service inside an `ns` block compiles cleanly; an enum with `cafe`/`bar_pub` members compiles and round-trips.

### PR-28 — Kotlin backend (BAB-K01, BAB-K02, BAB-K03, BAB-K04; defer K05)
- **Scope:** `KtDefnTranslator.scala` (K01), `KtUEBACodecGenerator.scala` (K02/K03/K04). Parallel `KtJsonCodecGenerator.scala` if K02 has a JSON-side mismatch.
- **Dependencies:** none.
- **Success criteria:** `mdl :test-kotlin-regular` green; generated stub set compiles `-Werror` with no `Unnecessary non-null assertion`, no `No cast needed`, no parameter-name supertype warnings.

### PR-29 — TypeScript backend (BAB-T01, BAB-T02)
- **Scope:** `TsBaboonTranslator.scala` (import emission split), `TsDefnTranslator.scala` (export type-or-stub), `TsUEBACodecGenerator.scala`/`TsJsonCodecGenerator.scala` (default branches), `BaboonSharedRuntime.ts` (`readByte`/`readBytes` truncation).
- **Dependencies:** T01 and T02 share files — sequence within the PR.
- **Success criteria:** `mdl :test-typescript-regular` green; generated stubs compile clean against strict tsconfig (`verbatimModuleSyntax`, `noImplicitReturns`, `noUncheckedIndexedAccess`); Rolldown bundle of a sample consumer succeeds.

### PR-30 — Swift backend (BAB-W01, BAB-W02, BAB-W03, BAB-W04)
- **Scope:** `SwJsonCodecGenerator.scala` (W01/W02/W03), `baboon_runtime.swift` runtime (W04 + new error type), codec call sites that gain `throws`.
- **Dependencies:** W01 and W03 share fix surface; W04 ripples into every reader caller — land last in this PR or split.
- **Success criteria:** `mdl :test-swift-regular` green; generated Swift compiles with `-warnings-as-errors`; corrupt-UEBA harness throws cleanly instead of trapping.

### PR-31 — Documentation/closure (BAB-S04, BAB-C01, BAB-K05)
- **Scope:** No code changes. Update `baboon-upstream-defects-clean.md` to reclassify S04 (closed: cannot reproduce) and C01 (closed: flags still present). Reach out to reporter for K05 reproducer.
- **Success criteria:** triage doc updated.

---

## Cross-cutting notes

- **Test impact (S01, K01):** No existing `conv-test/*` model defines an ns-scoped `service`. Add a new ns-scoped service to `test/conv-test/` and wire into `test/conv-test-sc/` and `test/conv-test-kt/` so the regression is locked in.
- **Test impact (S02):** Add an enum with non-Pascal-case members (`cafe`, `bar_pub`) to `test/conv-test/pkg01.baboon`.
- **Generated code regen:** `test/conv-test-*/` outputs need regeneration per language (`mdl :test-<lang>-runtime`). Watch `test/conv-test-sc/`, `test/conv-test-kt/`, `test/conv-test-ts/`, `test/conv-test-sw/`.
- **Runtime `.ts` change risk:** T01/T02 alter the public surface of `BaboonSharedRuntime.ts`. Document the breaking change.
- **Runtime `.swift` change risk (W04):** `throws` is a public-API break — coordinate the bump.
- **Independence:** PR-27 through PR-31 touch disjoint generators and runtimes. They can be developed concurrently.

---

## Verification commands per PR

- **PR-27 (Scala):** `mdl :test-scala-regular`, `mdl :test-scala-runtime`, `sbt baboon-compiler/test`
- **PR-28 (Kotlin):** `mdl :test-kotlin-regular`, `mdl :test-kotlin-runtime`, `./gradlew compileKotlin -Pkotlin.werror=true` in `test/conv-test-kt/`
- **PR-29 (TypeScript):** `mdl :test-typescript-regular`, `npm test` in `test/conv-test-ts/`, `npx tsc --noEmit --verbatimModuleSyntax --noImplicitReturns --noUncheckedIndexedAccess`
- **PR-30 (Swift):** `mdl :test-swift-regular`, `swift build -Xswiftc -warnings-as-errors` in `test/conv-test-sw/`
- **PR-31:** none — manual review

---

## Critical Files for Implementation

- `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/scl/ScDefnTranslator.scala`
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/scl/ScUEBACodecGenerator.scala`
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/kotlin/KtDefnTranslator.scala`
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/kotlin/KtUEBACodecGenerator.scala`
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/typescript/TsBaboonTranslator.scala`
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/typescript/TsDefnTranslator.scala`
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/swift/SwJsonCodecGenerator.scala`
- `baboon-compiler/src/main/resources/baboon-runtime/typescript/BaboonSharedRuntime.ts`
- `baboon-compiler/src/main/resources/baboon-runtime/swift/baboon_runtime.swift`
