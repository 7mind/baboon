# Service-context abstract-mode fix — implementation pattern (for per-language replication)

TypeScript and C# are DONE and VERIFIED. Replicate the SAME pattern to the other
languages. Read the worked examples before editing:

- TS: `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/typescript/TsServiceWiringTranslator.scala`,
  `.../typescript/TsTypes.scala`, runtime `baboon-compiler/src/main/resources/baboon-runtime/typescript/BaboonSharedRuntime.ts`.
- C#: `.../csharp/CSServiceWiringTranslator.scala`, `.../csharp/CSTypes.scala`,
  runtime `baboon-compiler/src/main/resources/baboon-runtime/cs/BaboonServiceWiring.cs`.

## The bug (abstract / `type` service-context modes)

Only the service *interface* declares the context generic (`interface Foo<Ctx>` with
`method(ctx: Ctx, arg)`). The CLIENT, the free `invoke*` wiring functions, and the
JSON/UEBA wrapper classes splice `ctx: Ctx` WITHOUT declaring the generic on the
generated type and WITHOUT renaming the codec-context parameter (also named `ctx` by
default) → unbound `Ctx` + duplicate-parameter errors. `ServiceContextResolver`
defaults the abstract param name to `ctx`, identical to the codec-context param.

## The five changes (all gated on context-active; `none` mode MUST stay byte-identical)

1. **Codec-context parameter rename.** Wherever a generated signature carries BOTH the
   service-context param (`ctxParamDecl`) AND the codec-context param, rename the
   codec-context param to `codecCtx` (or `baboonCodecCtx` if the service-context param
   is itself named `codecCtx`). Update every body reference to the codec context
   (the `encode(ctx, …)` / `decode(ctx, …)` calls in the codec-expr helpers) to use
   the new name. In `none` mode keep the name `ctx` so output is unchanged.

2. **Generic client + forward ctx to transport.** The client class/struct becomes
   generic over the abstract context (`Client<Ctx>` / `Client[Ctx]` etc.; for
   `type`/concrete mode the type is concrete, no generic). The client method keeps the
   leading `ctx: Ctx` (already present), and the abstract `ctx` is FORWARDED as the
   leading argument of the transport callback — so the transport delegate/closure type
   gains a leading `Ctx` parameter (`(ctx, service, method, data) => …`). The codec
   param on the client method is the renamed `codecCtx`.

3. **Per-invoke wrapper implementing a context-aware runtime interface.** Add ADDITIVE
   runtime types next to the existing `IBaboon{Json,Ueba}Service` / muxers — do NOT
   change the existing ones (the service-acceptance matrix uses `none` mode and depends
   on them byte-for-byte). New types:
   `IBaboonJsonServiceCtx<Ctx,R>` / `IBaboonUebaServiceCtx<Ctx,R>` with
   `Invoke(method, data, ctx: Ctx, codecCtx)`, plus `{Json,Ueba}MuxerCtx<Ctx,R>`.
   The generated wrapper, when context is active, is generic over `<Ctx>`, implements
   the `*Ctx` interface, takes the service context PER-INVOKE (not in the constructor),
   and forwards `ctx` + `codecCtx` to the free `invoke*` function. `rt` (errors mode)
   stays a constructor field. In `none` mode the wrapper is unchanged (implements the
   context-free interface, `Invoke(method, data, ctx)`).

4. **Declare the generic on the free `invoke*` functions and reference the interface
   with its type arg.** The `invoke{Json,Ueba}_X` functions must declare `<Ctx>` in the
   language-correct position and reference the service interface as `Foo<Ctx>`. (TS/C#
   already place this correctly via a `genericParam`/`ctxTypeParamDecl` helper; some
   languages emit it in the WRONG position — see per-language extras.)

5. **Dispatcher (if the language emits one).** Thread the abstract ctx per-invoke and
   rename the codec param, same as the wiring functions.

Helper shape used in TS/C# (mirror it): `ctxTypeParamDecl` (= `<Ctx>` only for abstract),
`ctxTypeArg` (appended to interface refs), `codecCtxName`, `svcCtxTypeName`
(abstract tn OR concrete tn), `svcCtxArgName` (param name). The existing `ctxParamDecl`
/ `ctxArgPass` already emit the service-context param/arg — keep them.

## Method-name casing

Do NOT change method-name casing for these languages — keep verbatim (the existing
convention). PascalCase is applied to **C# only** and is already done.

## Per-language extras (bugs beyond the shared pattern, from the research ledger)

- **rust**: client struct lacks `<Ctx>` and references unbound `Ctx`; the free fns emit
  an INVALID double generic clause `fn invoke_json_x<Rt: …><Ctx>(` — must be a single
  `<Rt: …, Ctx>` clause; the ueba fn dropped the abstract-ctx param and passed the
  codec ctx as the abstract ctx. Wrapper structs were already generic+correct.
- **kotlin**: `fun invokeJson<Ctx>(` is invalid — Kotlin needs `fun <Ctx> invokeJson(`.
  Wrappers were already generic+correct; client lacks the abstract ctx entirely (add
  per decision: generic client + forward ctx).
- **java**: `public static <Ctx> R invokeJson<Ctx>(` has a DUPLICATE `<Ctx>` — drop the
  one after the name. Wrappers were already generic+correct.
- **dart**: single service file; the wrapper classes are NOT generic but reference
  `Ctx` — make them generic; client lacks abstract ctx (add per decision).
- **swift**: the PROTOCOL drops the abstract ctx entirely (`func test(arg:)`), and the
  wiring emits a malformed `impl.test(arg: ctx, decoded)`. Swift protocols can't carry a
  free generic param; use a generic on the wrapper/client classes and add the `ctx`
  param to the protocol method (`func test(ctx: Ctx, arg:)` requires the protocol to be
  used generically — if Swift can't express a protocol method over an unconstrained
  external `Ctx`, use `associatedtype Ctx` on the protocol, or make the context a
  generic on a base; pick the approach that compiles with `swiftc`). Verify with swiftc
  if available.
- **python**: the interface uses `Generic[Ctx]` but the module never imports `Generic`
  / defines the `Ctx = TypeVar('Ctx')` — add the import + TypeVar; client/wrapper must
  also be `Generic[Ctx]` and define/much import the TypeVar; the duplicate `ctx` param
  is a hard SyntaxError, so the codec rename is critical.
- **scala**: wiring fns + wrapper were already generic `[Ctx]` and correct EXCEPT the
  duplicate `ctx` param (codec rename fixes it); client lacks abstract ctx (add per
  decision). Scala generic position is `def invokeJson[Ctx](`.

## Verification

Do NOT run sbt/compile yourself (a central build runs after all languages merge).
After editing, the `none`-mode output for your language MUST be byte-identical to before
(all your changes gated on context-active). Report the exact list of files+hunks you changed.
