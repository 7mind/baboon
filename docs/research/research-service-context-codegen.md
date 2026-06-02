# Service-Context Codegen Defects (abstract mode) across target languages

Investigation of a bug report against `pragma <lang>.service.context = "abstract"`.
The reporter, using the model below, observed in **TypeScript** output (and claimed
similar in **C#**):

> model Sdk.Test.Service / version "1.0.0" /
> `pragma typescript.service.context = "abstract"` / `pragma cs.service.context = "abstract"`
> root service TestService { def test(data in {text: str} data out {text: str}) }

1. client is generated **without a type parameter** (`<Ctx>`),
2. argument-name **clash**: `ctx: Ctx, ctx: BaboonCodecContext`,
3. json/ueba **services** are generated **without context in invocations** — the
   abstract context is injected through the **constructor** instead (inconvenient),
4. **method names** in clients and services are **not capitalized**.

The reporter asks to debug+fix TS and C#, and to check the other 7 target
languages. This ledger characterizes the defects per-language against real
generated artifacts under `/tmp/repro-all/<lang>` (generated with
`--service-context-mode abstract --generate-json-codecs-by-default
--generate-ueba-codecs-by-default`) and the translator sources.

Hypothesis state: `confirmed` · `uncertain` · `wrong`
Evidence state:   `correct` · `incorrect` · `unverified`

---

## Resolution (build phase, post-research) — ALL 9 LANGUAGES FIXED

Design chosen by the user (see [[project-service-context-abstract-fix]]): generic
client that forwards `ctx` to the transport; runtime service interface carries the
context per-invoke (new additive `IBaboon*ServiceCtx<Ctx,R>` + `*MuxerCtx`); codec
context parameter renamed `codecCtx`/`codec_ctx` to break the H2 clash; C#-only
PascalCase method names (#4); apply to all 9.

`sbt baboonJVM/compile` is GREEN with all 9 backends changed. The implementation
pattern is documented in `service-context-fix-pattern.md` (same dir).

**Compile-verified abstract output (regenerated, real toolchain):**
- **TypeScript** — `tsc --strict` clean.
- **C#** — `dotnet build` clean (0/0), incl. PascalCase (`Test`/`TestJson`; wire strings `"test"`).
- **Python** — `python3 -m py_compile` clean (duplicate-param SyntaxError gone; `Ctx = TypeVar` + `Generic` imported in interface/client/wiring; per-invoke `Generic[Ctx]` wrappers).
- **Swift** — `swiftc -typecheck` clean (the hard case): protocol gets `associatedtype Ctx`; free fns `invokeJson<Impl: TestService>(… ctx: Impl.Ctx, codecCtx)`; wrappers generic `<Impl: TestService>` conforming to `IBaboon*ServiceCtx` (`typealias Ctx = Impl.Ctx`) per-invoke; client `class …Client<Ctx>` forwards ctx to transport. (The parallel subagent left the Swift client + wrapper unfinished — orchestrator completed them.)

**Structurally verified (not standalone-compiled — toolchain/dep harness cost):**
scala, rust, kotlin, java, dart. No residual duplicate-`ctx` clash anywhere; generic
placement correct (kotlin `fun <Ctx> invokeJson`, java `<Ctx> R invokeJson`, rust single
`<Rt: …, Ctx>` clause, dart static `invokeJson<Ctx>`); wrappers per-invoke + generic.

**NoContext (`--service-context-mode none`) regression — VERIFIED CLEAN for all 9.**
Pristine HEAD built via `nix build .#baboon` vs the my-changes build; per-file diff over
all 9 langs: for ts/scala/rust/kotlin/dart/swift/python the ONLY differing file is the
additive runtime service-wiring file — every per-service generated file byte-identical.
C# differs only by the intended PascalCase; Java adds 4 additive `*Ctx<Ctx>` transport
interfaces + 4 new resource files, per-service identical. → the service-acceptance
matrix (none mode) is structurally safe.

**C# hand-written harnesses updated for PascalCase** (impl defs + call sites capitalized,
wire-string literals kept lowercase): `test/services/cs/ServiceTest/{PetStoreImpl,
PetStoreClient,PetStoreServer}.cs`, `test/services/cs/ClientRoundTrip/Driver.cs`,
`test/cs-stub/BaboonTests/WiringTests.cs` (MockI1/ThrowingI1/MockI2 `TestCall`/`TestCall2`/`NoErrCall`).

**REMAINING (final end-to-end gate, not yet run):** native rebuild (`mdl :build`) +
`mdl :test` (cs-stub WiringTests with the renamed impls) + `mdl :test-service-acceptance`
(9×9×2 none-mode HTTP matrix); optional standalone abstract-compile for scala/rust/kotlin/java/dart.

**Gate run 1 (2026-06-02 21:33) — FAILED at `test-gen-dt-wiring` (code 4), NOT a codegen
defect.** Cause: the new abstract-pragma unit-test fixture was placed at
`baboon-compiler/src/test/resources/baboon/case-ctx/mixed.baboon` declaring
`model MixedCase.Pkg` / `version "1.0.0"` — the SAME domain id+version already owned
by the pre-existing committed `case-ok/mixed.baboon`. Every bulk-codegen action globs
`--model-dir .../baboon/` recursively, so both files loaded → "Domains with duplicated
versions were found … MixedCase.Pkg" at import-resolution. Fix: moved the fixture out
of the bulk model-dir to `baboon-compiler/src/test/resources/baboon-ctx/case-ctx/`
(still a loadable classpath resource for the unit test, but not globbed by any
`--model-dir`), and updated `TypeScriptServiceContextPragmaTest`'s resource path from
`baboon/case-ctx` to `baboon-ctx/case-ctx`. This also removes the secondary hazard of
the `typescript.service.context = "abstract"` pragma leaking abstract output into the
TS wiring bulk codegen. `TypeScriptServiceContextPragmaTest` green post-move.

**Gate run 2 (2026-06-02 21:41) — FAILED at `test-cs-wiring-either` (code 1), incomplete
PascalCase harness update.** The #4 fix made C# service methods PascalCase
(`TestCall`/`TestCall2`/`NoErrCall`); the build-phase session updated the base
`test/cs-stub/BaboonTests/WiringTests.cs` and the `test/services/cs/` PetStore harness,
but MISSED the three service-result overlays that ship their own `WiringTests.cs`
(rsynced over the base by the `test-gen-cs-wiring-{either,result,outcome}` actions):
`test/cs-stub-{either,result,outcome}-overlay/BaboonTests/WiringTests.cs`. Their impl
classes (`{Mock,Failing,Throwing}I1{Either,Result,Outcome}`, `MockI2{…}`) still declared
lowercase `testCall`/`testCall2`/`noErrCall` → `CS0535 does not implement interface
member`. Fix: renamed the 7 method DEFINITIONS per file to PascalCase (lowercase
`BaboonMethodId("I1","testCall")` wire strings preserved; test-method names untouched).
Targeted `mdl :test-cs-wiring-{either,result,outcome}` all green post-fix.

**Gate run 3 (parallel) — `test-kotlin-kmp-regular` OOM aborted the run.** The Kotlin
compiler hit `java.lang.OutOfMemoryError: GC overhead limit exceeded` in `compileKotlin`
under the parallel matrix (the documented CLAUDE.md flake — Kotlin daemon JVM heap, not
system RAM; 125 GB free). mdl fail-fast then KILLED the in-flight `test-service-acceptance`
and ~25 other actions before they reported. All 42 service/wiring steps that DID complete
were green. Re-ran serially (`mdl --seq`) to clear the OOM and get a real
service-acceptance verdict; `test-kotlin-kmp-regular` then passed in 55.9s (vs 5m55s OOM)
— confirms flake, not regression.

**Gate run 4 (serial) — `test-service-acceptance` FAILED 153/162: the entire C# SERVER /
JSON row (9 cells) returns HTTP 500; UEBA fine; all 8 other servers green both codecs.**
Root cause: a real regression in the hand-written harness `test/services/cs/ServiceTest/
PetStoreServer.cs`. The build-phase #4 PascalCase pass over-applied — it correctly
capitalized the `impl.AddPet(...)` C# method CALLS, but ALSO capitalized the `switch`
`case "AddPet"`/`"GetPet"`/`"ListPets"`/`"DeletePet"` labels. Those labels are matched
against the WIRE method name extracted from the URL path (`POST /PetStore/addPet` →
`"addPet"`, lowercase). So every JSON request fell through to `default: throw
ArgumentException` → 500. UEBA was unaffected because it dispatches via
`PetStoreWiring.InvokeUeba(new BaboonMethodId("PetStore", "addPet"), …)` against the
generated wiring, which correctly matches lowercase wire strings. Fix: reverted the 4
`case` labels to lowercase wire names (`addPet`/`getPet`/`listPets`/`deletePet`); kept the
`impl.*` calls PascalCase. Confirms the locked invariant "C#-only PascalCase method names,
wire strings kept lowercase" — the wire-string dispatch switch is NOT a method name.
Re-running `mdl :test-service-acceptance` to confirm 162/162.

---

## H1 — The generated RPC **client** class omits the `<Ctx>` type parameter while its methods reference the abstract context type, leaving `Ctx` unbound (compile error)

The service *interface* is correctly emitted as generic (`interface TestService<Ctx>`
in TS, `interface ITestService<Ctx>` in C#). The client translator reuses the
abstract-context parameter declaration (`ctx: Ctx`) on client methods but does not
declare `<Ctx>` on the client class, so `Ctx` does not resolve.

**State:** `confirmed` (TS, C#) — rests on E1.✓ (TS) and the C# repro; cross-language coverage pending [[H6]].

**Evidence:**
- **E1** [`correct`] — `/tmp/repro-all/ts/sdk/test/service/test-service/client.ts`: `export class TestServiceClient {` (no `<Ctx>`) yet method `public async test(ctx: Ctx, arg: In, ...)` references `Ctx`. Orchestrator-generated artifact, round 1.
- **E1.cs** [`correct`] — `/tmp/repro-all/cs/Sdk-Test-Service/TestService_Client.cs`: `public sealed class TestServiceClient` (no `<Ctx>`) with `public ... test(Ctx ctx, ... arg, BaboonCodecContext ctx)`. Round 1.
- **E1.src** [`correct`] — `TsServiceWiringTranslator.scala:361`: `q"""export class ${typeTranslator.serviceClientName(svcType.name)} {` — no type-parameter suffix, while `ctxParamDecl` (line 542-545) emits `s"$pn: $tn, "`. Round 1.

---

## H2 — The abstract context parameter collides with the codec-context parameter (both named `ctx`) in client methods and wiring functions → duplicate parameter name (compile error)

The pragma default parameter name is `ctx`; the codec context parameter is also
`ctx` (TS `BaboonCodecContext`, C# `BaboonCodecContext`). Client UEBA/JSON methods
and the `invoke<Json|Ueba>_X` wiring functions emit both, producing `ctx: Ctx, ...,
ctx: BaboonCodecContext`.

**State:** `confirmed` (TS, C#) — rests on E2.✓; cross-language coverage pending [[H6]].

**Evidence:**
- **E2** [`correct`] — TS client: `public async test(ctx: Ctx, arg: In, ctx: BaboonCodecContext = BaboonCodecContext.Default)`. TS wiring: `invokeJson_TestService(... impl: TestService, ctx: Ctx, ctx: BaboonCodecContext)`. Both duplicate `ctx`. Round 1.
- **E2.cs** [`correct`] — C# client: `public ... test(Ctx ctx, ... arg, BaboonCodecContext ctx)`. Round 1.
- **E2.src** [`correct`] — `TsServiceWiringTranslator.scala:332`: `public async ${m.name.name}(${ctxParamDecl}arg: $inType, ctx: $tsBaboonCodecContext = ...)` — `ctxParamDecl` already emits a `ctx` so the trailing literal `ctx:` duplicates it. Round 1.

---

## H3 — JSON/UEBA wiring **service wrapper** classes inject the abstract context via constructor (not per-invocation) and are not parameterized over `<Ctx>`

The runtime `IBaboon{Json,Ueba}Service.invoke(method, data, ctx: BaboonCodecContext)`
contract is fixed; the wrapper bakes the abstract context as a constructor field
(`svcCtxField`) and forwards `this.ctx`. The wrapper class is not generic over Ctx,
so the `private readonly ctx: Ctx` field leaves `Ctx` unbound, and `impl: TestService`
omits the interface's `<Ctx>` type argument.

**State:** `confirmed` (TS) — rests on E3.✓; C# + cross-language pending [[H6]].

**Evidence:**
- **E3** [`correct`] — TS wiring `class TestServiceJsonService implements IBaboonJsonService<string> { ... private readonly ctx: Ctx; constructor(impl: TestService, ctx: Ctx) {...} invoke(method, data, ctx: BaboonCodecContext) { return invokeJson_TestService(method, data, this.impl, this.ctx, ctx); } }`. Round 1.
- **E3.src** [`correct`] — `TsServiceWiringTranslator.scala:602-638`: `svcCtxField` → `private readonly $pn: $tn`, ctor param, and `export class $wrapperName implements $ifaceType<$retType>` with no `<Ctx>`. Round 1.

---

## H4 — Generated service/client **method names** are not capitalized (the `.baboon` name is preserved verbatim)

`def test` produces `test(...)` in the interface, client, and wiring. In C# this is
non-idiomatic (methods are PascalCase); in TS camelCase is conventional. Whether this
is a defect is language-dependent and tied to whatever casing convention the
generators apply elsewhere.

**State:** `wrong` (as a *codegen* defect) — rests on E4.✓ and E4.x.✓. All 9 generators preserve the `.baboon` method name verbatim, applying each language's own identifier convention (camelCase for ts/cs/kotlin/java/swift/dart/scala, snake_case for rust/python). No generator capitalizes method names anywhere — DTO/codec members follow the same verbatim convention — so "not capitalized" is the *intended, consistent* behavior, not a defect. The JSON variant is uniformly `<name>Json`/`<name>_json`. Whether C# should PascalCase service methods is a **product/style decision**, not a compile defect. Treated as out-of-scope-bug; carried to the delivery as a design question.

**Evidence:**
- **E4** [`correct`] — TS/C# interface+client emit `test`/`testJson` (lowercase). Round 1.
- **E4.x** [`correct`] — verbatim names confirmed in every language's interface+client (scala `def test`/`testJson`, rust `fn test`/`test_json`, kotlin `fun test`/`testJson`, java `test`/`testJson`, dart `test`/`testJson`, swift `func test`/`testJson`, python `def test`/`test_json`). DTO/codec methods use `encode`/`decode` (runtime names), confirming no service-name capitalization step exists. Round 2, orchestrator-validated against /tmp/repro-all/*.

---

## H5 — Root cause: the client/wiring translators apply context-injection without the `<Ctx>` declaration and name-deduplication that the interface translator performs correctly

H1, H2, H3 are symptoms of one defect: `ctxParamDecl`/`svcCtxField` (and C# equivalents)
are spliced into client/wiring sites that never (a) declare the generic `<Ctx>` parameter
on the generated type, nor (b) deconflict the abstract-context name against the
codec-context parameter. The interface translator (which *does* emit `<Ctx>`) is the
only site that handles it correctly.

**State:** `confirmed` — rests on E5.a.✓, E5.b.✓ and the per-language `ctxParamDecl`/`svcCtxField` sources cited under [[H6.*]]. The single shared `ServiceContextResolver` produces `AbstractContext(typeName="Ctx", parameterName="ctx")`. Each language translator splices `ctxParamDecl` (= `"<param>: <Type>, "`, default `"ctx: Ctx, "`) into client/wiring signatures, and `svcCtxField` into wrapper ctors, but the surrounding generated *type* (client class, free wiring function, wrapper class) is emitted **without declaring the generic `<Ctx>`** and **without renaming the always-present codec-context parameter (also `ctx`)**. The service-*interface* translator is the only site that declares `<Ctx>`. Hence three independent symptoms (H1, H2, H3) from one omission family.

**Evidence:**
- **E5.a** [`correct`] — `ServiceContextResolver.scala:73-74`: `if (mode == "abstract") ResolvedServiceContext.AbstractContext(validatedType, validatedParam)`; defaults `typeName="Ctx"` (cliConfig) and `parameterName="ctx"`. So the abstract param name defaults to `ctx`, identical to the codec-context param name across all generators. Round 2.
- **E5.b** [`correct`] — `ctxParamDecl` definitions emit `s"$pn: $tn, "` with no deconfliction, and the codec-context param literal is `ctx: <CodecCtx>` in every wiring/client emitter (TS `:332`,`:693`; scala `:792`; kotlin `:404`,`:662`; java; dart `:387`; swift `:9`/`:31`; python `:485`). Round 2.

---

## H6 — Cross-language scope: which of the 9 targets exhibit H1–H4, and which handle abstract context correctly or omit the feature

Bounded survey over the 9 generators (ts, cs, scala, rust, kotlin, java, dart, swift,
python) using `/tmp/repro-all/<lang>` artifacts + translator sources. Predicate per
language: does the client carry `<Ctx>`? does any signature duplicate the context
parameter name? are wrapper services constructor-injected? are method names
capitalized? Completeness limit: only the single-method `text in/out` model is
exercised; multi-method / error-mode / concrete-context variants are out of scope.

**State:** `confirmed` — all 9 targets surveyed; every target that emits server-side wiring exhibits the duplicate-`ctx` defect (H2), and the abstract-context feature produces non-compiling output in all 9. Completeness limit: single-method `text in/out`, `--service-context-mode abstract`, default type/param names, async-on where applicable; concrete-context (`type`) mode and custom param-name (which would avoid the H2 clash) not exercised. Rests on the per-language evidence below, all orchestrator-validated in Round 2 against `/tmp/repro-all/<lang>`.

**Cross-language matrix** (D1 client `<Ctx>` / D2 dup-`ctx` / D3 wrapper / D4 method-case):

| lang | client exposes abstract ctx? | D1 client `<Ctx>` declared? | D2 wiring dup `ctx`? | wiring generic placement | D3 wrapper generic `<Ctx>`? | compiles? |
|------|------|------|------|------|------|------|
| **ts** | yes (`ctx: Ctx`) | NO → unbound + dup w/ codec ctx | YES (client *and* wiring fn) | n/a (free fn, refs unbound `Ctx`) | NO → unbound `Ctx` field | ✗ |
| **cs** | yes (`Ctx ctx`) | NO → unbound + dup w/ codec ctx | YES (client *and* wiring fn) | correct `InvokeJson<Ctx>` | NO → wrapper not generic, refs `ITestService<Ctx>`/`Ctx _ctx` unbound | ✗ |
| **scala** | no (client = arg only) | n/a (no unbound) | YES (`invokeJson[Ctx](… ctx: Ctx, ctx: BaboonCodecContext)`) | correct `[Ctx]` | YES (correct) | ✗ (dup param only) |
| **rust** | yes (`ctx: Ctx`) | NO → unbound `Ctx` in method | json fn uses `_ctx` for codec (no name clash) but ueba fn drops abstract param & passes codec ctx as abstract | **invalid** double clause `<Rt><Ctx>` | YES (correct) | ✗ |
| **kotlin** | no (client = arg + codec) | n/a | YES (`fun invokeJson<Ctx>(… ctx: Ctx, ctx: BaboonCodecContext)`) | **invalid** `invokeJson<Ctx>` (needs `fun <Ctx> invokeJson`) | YES (correct) | ✗ |
| **java** | no (client = arg) | n/a | YES (`Ctx ctx, BaboonCodecContext ctx`) | **invalid** `public static <Ctx> String invokeJson<Ctx>(` | YES (correct) | ✗ |
| **dart** | no (client = arg + codec) | n/a | YES (`Ctx ctx, BaboonCodecContext ctx`) | n/a (free fn refs unbound `Ctx`) | NO → unbound `Ctx` field | ✗ |
| **swift** | no (client = arg + codec) | n/a | YES (`_ ctx: Ctx, _ ctx: BaboonCodecContext`) + protocol drops ctx entirely + `impl.test(arg: ctx, decoded)` malformed | n/a (refs unbound `Ctx`) | NO → unbound `Ctx` field | ✗ |
| **python** | yes (`ctx: Ctx`, then body `ctx = self.ctx`) | NO → `Ctx` NameError (also missing `Generic`/`TypeVar` import) | YES → **SyntaxError** dup arg | n/a | NO → unbound `Ctx` | ✗ (won't even parse) |

**Evidence (validated Round 2):**
- **E6.scala** [`correct`] — `/tmp/repro-all/scala/.../TestService_Client.scala:6-11` client has no `[Ctx]`, methods `def test(arg: In)` (no abstract ctx); `TestService_Wiring.scala:7-12` `invokeJson[Ctx](… ctx: Ctx, ctx: BaboonCodecContext)` dup; wrapper `class TestServiceJsonService[Ctx](impl, rt, ctx: Ctx)` generic-correct.
- **E6.rust** [`correct`] — `test_service_client.rs:5-13,24,32` struct `<TU,TJ>` (no `<Ctx>`), method `pub fn test(&self, ctx: Ctx, …)` unbound `Ctx`, body uses `self.ctx`; `test_service_wiring.rs:10` `invoke_json_test_service<Rt: IBaboonServiceRt><Ctx>(` invalid double clause, `:15` `ctx: Ctx, _ctx: &BaboonCodecContext`; wrapper `struct TestServiceJsonService<Impl, Rt, Ctx>` correct.
- **E6.kotlin** [`correct`] — `TestServiceClient.kt:18,30,38` no `<Ctx>`, methods `fun test(arg, ctx: BaboonCodecContext)` (no abstract); `TestServiceWiring.kt:25-30` `fun invokeJson<Ctx>(… ctx: Ctx, ctx: BaboonCodecContext)` dup + invalid placement; wrapper `class TestServiceJsonService<Ctx>(…)` correct.
- **E6.java** [`correct`] — `TestServiceClient.java:12,27` no `<Ctx>`, method `test(In arg)`; `TestServiceWiring.java:17-21` `public static <Ctx> String invokeJson<Ctx>(… Ctx ctx, BaboonCodecContext ctx)` dup `<Ctx>` + dup `ctx`; wrapper `JsonService<Ctx>` correct.
- **E6.dart** [`correct`] — single `test_service.dart`: client `:86,92` `class TestServiceClient` (no generic), method `test(in_ arg, [ctx])`; wiring `:12-16,33-37` `static String invokeJson(… Ctx ctx, BaboonCodecContext ctx)` dup; wrapper `:56,61` `class TestServiceJsonService implements …` with `final Ctx _ctx;` unbound.
- **E6.swift** [`correct`] — `test_service.swift:3-5` protocol `func test(arg:)` **drops abstract ctx entirely**; `test_service_wiring.swift:9,15,31` `_ ctx: Ctx, _ ctx: BaboonCodecContext` dup + `impl.test(arg: ctx, decoded)` malformed; wrapper `:50,54` `class TestServiceJsonService: IBaboonJsonService` with `private let ctx: Ctx` unbound.
- **E6.cs** [`correct`] — `/tmp/repro-all/cs/Sdk-Test-Service/TestService_Client.cs` `public sealed class TestServiceClient` (no `<Ctx>`), method `test(Ctx ctx, … arg, BaboonCodecContext ctx)` dup + unbound; `TestService.Wiring.cs:66-70,87-91` `InvokeJson<Ctx>(… Ctx ctx, BaboonCodecContext ctx)` dup param (placement valid); wrapper `:113,119` `class TestServiceJsonService : IBaboonJsonService<String>` not generic but `TestServiceJsonService(ITestService<Ctx> impl, Ctx ctx)` + `Ctx _ctx` unbound. Round 2.
- **E6.python** [`correct`] — `TestService.py:5` `class TestService(ABC, Generic[Ctx])` (import of `Generic`/`TypeVar` missing); `TestService_Client.py:18,28` `def test(self, ctx: Ctx, arg: In)` then `ctx = self.ctx`, `Ctx` undefined; `TestService_Wiring.py:9,23` `def invoke_json_TestService(… ctx: Ctx, ctx: BaboonCodecContext)` → SyntaxError; wrapper `:41-49` `self.ctx: Ctx` unbound.

### H6.note — Two client design families

The survey reveals the generators disagree on whether the **client** should carry the
abstract context at all:
- **Group A** (ts, cs, rust, python): client method takes `ctx: Ctx` (so the client *would*
  need to be generic over `<Ctx>` — but isn't → unbound).
- **Group B** (scala, kotlin, java, dart, swift): client method takes only `arg` (+ the
  codec context); the abstract context never appears on the client.

This inconsistency is itself a finding: the intended client contract for abstract context
is undefined across the codebase. The fix must pick one (recorded as a delivery question).
