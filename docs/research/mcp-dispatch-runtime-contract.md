# MCP Dispatch Runtime Contract (v1) — language-agnostic specification

Status: design note backing locked decision `decisions:M1` (this session).
Author: opus-4.8[1m]. Session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b.
Date: 2026-06-04. Base commit: `d3e0557dbd41dfaa66e6b45975cf096c668498db`.

This note fixes the **runtime dispatch contract** that every one of the 9
per-language Baboon MCP server generators (scala, cs, python, rust, typescript,
kotlin, java, dart, swift) MUST target **identically**. It is the runtime-shape
sibling of the wire contract in `decisions:K4`
(`docs/research/mcp-protocol-contract.md`): K4 fixes *what goes on the wire*;
this note fixes *the shape of the generated code that produces those bytes and
the additive runtime types it leans on*, so that the per-language generator
tasks (T8/T10/T12–T18) implement a single, non-divergent design.

The guiding constraint, per Q2 and K4 §1.4: the generated MCP server is
**transport-abstract**. It emits the JSON-RPC method state machine + tool
dispatch ONLY. Bytes-in/bytes-out (stdio framing, Streamable-HTTP request
bodies) is an **injected adapter** the generated surface never contains — exactly
as the existing abstract-context service contract supplies `Ctx` per invocation
rather than baking an I/O loop into the generated wrapper.

It is grounded in the verified per-language service-wiring runtimes (see the T1
grounding note `docs/research/mcp-generators-grounding.md` §2 for line refs) —
specifically the `IBaboon{Json,Ueba}ServiceCtx<Ctx,R>` family, the
`{Json,Ueba}MuxerCtx<Ctx,R>` dispatch, `BaboonMethodId(ServiceName, MethodName)`,
and the `BaboonWiringError` taxonomy
(`NoMatchingService`/`NoMatchingMethod`/`DecoderFailed`/`EncoderFailed`/
`CallFailed`/`DuplicateService`).

All signatures below are read against the actual runtime source (C#:
`BaboonServiceWiring.cs`; TS: `BaboonSharedRuntime.ts`; Swift:
`baboon_service_wiring.swift`; Python: `baboon_service_wiring.py`; Rust:
`baboon_service_wiring.rs`), not hypothesised.

---

## 0. Design principle — mirror the abstract-context service contract

The service-wiring contract pairs, per language:

- a per-invocation interface `IBaboon{Json,Ueba}ServiceCtx<Ctx, R>` whose single
  method is `invoke(method: BaboonMethodId, data, ctx: Ctx, codecCtx: BaboonCodecContext): R`;
- a cross-domain dispatcher `{Json,Ueba}MuxerCtx<Ctx, R>` keyed by
  `method.ServiceName`, fail-fast on `DuplicateService`/`NoMatchingService`;
- generic over a caller-supplied context `Ctx` and a return shape `R` (the muxer
  stays `R`-parametric to support sync `R = string` / async `R = Task<string>`
  emission without changing the runtime).

The MCP dispatch contract is the **same shape, one tier up**: it accepts a parsed
JSON-RPC *request value* instead of `(method, data)`, threads the SAME `Ctx`
through, returns a JSON-RPC *response value* instead of a raw payload, and
**delegates `tools/call` straight into the existing `JsonMuxerCtx.invoke`**. The
MCP runtime adds NO new dispatch mechanism — it is a protocol shell over the
established muxer. This is what makes it feel like a natural sibling rather than
a parallel invention, and is the single most important consistency property
across the 9 backends.

```
 transport adapter (NOT generated)                 generated MCP surface (this note)
 ┌─────────────────────────────┐   JsonRpcRequest  ┌───────────────────────────────┐
 │ stdio / Streamable-HTTP loop │ ───────────────▶ │ <Service>McpServer.handle      │
 │ frames bytes, parses JSON,   │                  │   initialize / tools/list /    │
 │ supplies Ctx per request     │ ◀─────────────── │   tools/call  ─┐               │
 └─────────────────────────────┘   JsonRpcResponse └────────────────┼──────────────┘
                                                                     │ tools/call only
                                                          existing   ▼
                                                   JsonMuxerCtx<Ctx, R>.invoke(
                                                     BaboonMethodId, data, ctx, codecCtx)
                                                          (already shipped, §2 grounding)
```

---

## 1. The runtime type family (additive, analogous to `IBaboon*ServiceCtx`)

These types are **additive runtime helpers** shipped as resource files alongside
the existing service-wiring runtime (§6). They are emitted ONLY when the `:mcp`
target is selected; the service-wiring runtime is unchanged, byte-for-byte. Names
use the canonical (C#/Scala/TS) generic-parameter spelling; per-language spelling
is in §7.

### 1.1 JSON-RPC value types — `JsonRpcRequest` / `JsonRpcResponse`

A transport-neutral, codec-neutral representation of one JSON-RPC 2.0 message,
already parsed from bytes by the adapter. These carry the request *as structured
fields*, NOT as raw bytes, so the dispatch shell never touches the transport.

```
// canonical (cs/scala/ts spelling)
final case class JsonRpcRequest(
  id: JsonRpcId | null,        // string | number | absent(notification); see 1.2
  method: String,             // "initialize" | "tools/list" | "tools/call" | other
  params: JsonValue | null    // raw JSON params subtree, decoded per-method inside handle
)

final case class JsonRpcResponse(
  id: JsonRpcId | null,        // echoes request id; null on parse-error path (K4 §3.1)
  result: JsonValue | null,    // present on success (mutually exclusive with error)
  error:  JsonRpcError | null  // present on Channel-A protocol error (K4 §3.1)
)

final case class JsonRpcError(code: Int, message: String, data: JsonValue | null)
```

- `JsonRpcId` is the union `string | number` (JSON-RPC permits both). For a
  notification (`notifications/initialized`), `id` is absent and `handle`
  returns **no** response (§3 — `null`/`Option.none`/`nil`).
- `params` and `result`/`error.data` are the **language's existing JSON value
  type** — the same node type the per-language JSON codecs already produce
  (Circe `Json`, Newtonsoft `JToken`, `serde_json::Value`, Jackson `JsonNode`,
  Python `dict`/`Any`, Dart `Object?`, Swift `Any`/`JSONValue`, TS `unknown`).
  The MCP runtime does **not** introduce a new JSON model; it reuses what the
  codecs already speak. The transport adapter is responsible for turning bytes
  ↔ this value type; `handle` only ever sees the value type.

### 1.2 The dispatch interface — `IBaboonMcpServer<Ctx>`

The single generated entrypoint, analogous to `IBaboonJsonServiceCtx<Ctx, R>`.
**It is NOT `R`-parametric** the way the muxer is: an MCP response is always a
`JsonRpcResponse` value (the protocol fixes the envelope), so the only free type
parameter is the caller's `Ctx` — identical to the `Ctx` the service-wiring
contract threads. (Async emission, if ever needed, wraps the whole return as
`Future<Option<JsonRpcResponse>>` at the per-language level, exactly as the muxer
wraps `R`; v1 is sync.)

```
// canonical spelling
interface IBaboonMcpServer<Ctx> {
  // returns None for an accepted notification (no response), Some(resp) otherwise
  handle(request: JsonRpcRequest, session: McpSession, ctx: Ctx,
         codecCtx: BaboonCodecContext): Option<JsonRpcResponse>
}
```

- `handle` is the per-request dispatch entrypoint mandated by K4 §1.4
  (`handle(jsonRpcRequest, ctx) -> jsonRpcResponse`), refined here with the
  `codecCtx` argument the underlying muxer requires, the `session` latch (§3.1),
  and an `Option` return for the notification case.
- It is **synchronous** and **side-effect-free w.r.t. I/O**: it reads the
  request value, possibly calls `JsonMuxerCtx.invoke`, and returns a value. No
  sockets, no stdin/stdout, no `while(true)` loop appears anywhere in the
  generated code. That loop lives entirely in the hand-written transport adapter
  the integrator supplies (§3).

### 1.3 The concrete generated server — `<Service>McpServer<Ctx>`

For each Baboon `service` the generator emits a concrete class implementing
`IBaboonMcpServer<Ctx>`, constructed from the **already-existing** generated
service wiring. It holds:

1. a `JsonMuxerCtx<Ctx, String>` (the existing dispatcher — reused verbatim, NOT
   re-implemented), built from the generated `<Svc>` service wrapper exactly as
   the service-acceptance harness builds it today;
2. an immutable **tool registry** (§5) computed at generation time;
3. fixed `serverInfo` (`name`/`version`) and the constant protocol version
   `"2025-06-18"` from K4 §2.2 — baked in as generated constants.

`handle` is a `switch` on `request.method`:

| `request.method` | action |
|---|---|
| `"initialize"` | validate `params.protocolVersion` present (else `-32602`); return the fixed `initialize` result (K4 §2.2); set `session.initialized = true`. |
| `"notifications/initialized"` | accept, return `None` (no response). |
| `"tools/list"` | require `session.initialized` (else `-32600`, K4 §2.1); return `{ tools: [...] }` from the registry (§5), in declaration order, schema from K4/K3. |
| `"tools/call"` | require `session.initialized` (else `-32600`); look up `params.name` in the registry → `BaboonMethodId`; serialize `params.arguments` to a JSON string; call `muxer.invoke(methodId, argsJson, ctx, codecCtx)`; wrap the returned JSON string as `{content:[{type:"text",text:...}], isError:false}`. Map `BaboonWiringError` to Channel A / B per K4 §3. |
| anything else | `-32601` Method not found (K4 §3.1). |

The generated code's only dependency surface is: the JSON value type, the JSON
codecs (via the muxer), `BaboonMethodId`, `BaboonWiringError`, and the new
runtime types in §1.1–§1.2. No transport, no new serialization framework.

---

## 2. How `ctx` threads through (transport → dispatch → service method)

The `Ctx` type parameter is the SAME one the service-wiring abstract-context
contract uses (`IBaboonJsonServiceCtx<Ctx, R>`), so MCP introduces no new context
concept. The thread is:

1. **Transport adapter** (hand-written, per-integrator) accepts a connection /
   request, constructs the per-request `Ctx` (e.g. auth principal, request-id,
   tracing span — whatever the abstract context models), parses the bytes into a
   `JsonRpcRequest`, and calls `server.handle(request, session, ctx, codecCtx)`.
2. **`handle`** does NOT inspect or transform `ctx`. For `tools/call` it passes
   `ctx` straight into `muxer.invoke(methodId, argsJson, ctx, codecCtx)`.
   For `initialize`/`tools/list` `ctx` is unused (those are pure protocol
   metadata). This mirrors the service-wiring `invoke(method, data, ctx,
   codecCtx)` exactly — `ctx` is opaque to the dispatch layer.
3. **`JsonMuxerCtx.invoke`** routes by `method.ServiceName` and calls the
   per-service `IBaboonJsonServiceCtx.invoke(method, data, ctx, codecCtx)`, which
   the existing generator already wires to the user's service implementation.
   `ctx` arrives at the user's method body unchanged.

Net: `ctx` is constructed by the transport (outside the generated surface),
flows through `handle` untouched, and reaches the service method via the
unmodified muxer. When `service.context = none`, the generator targets the
context-free `JsonMuxer<R>` and `handle` carries a `Unit`/`void`/`()` `Ctx` (or
an `IBaboonMcpServer` non-generic overload) — the SAME none/abstract/type
trichotomy `ServiceContextResolver` already drives for service wiring, extended
with `mcp` (note: `ServiceContextResolver` currently lists 9 languages and would
gain MCP awareness in T5/T6, per the T1 grounding §2 discrepancy #4).

---

## 3. The transport-injection seam (what is NOT generated)

### 3.1 What the generated surface excludes

The generated MCP code contains **none** of:

- a read/parse loop over stdin or an HTTP body;
- any framing (newline-delimited JSON, `Content-Length` headers, SSE/Streamable
  chunking);
- any socket/file-descriptor/HTTP-server type;
- connection lifecycle / session management beyond the in-`handle` protocol
  state described in K4 §2.1.

The "initialized" precondition (K4 §2.1: reject `tools/*` before a successful
`initialize` with `-32600`) is **per-connection state**, and a connection is a
transport concept. The contract therefore places the latch in a tiny generated
value `McpSession` that the **adapter owns and creates per connection**, rather
than as ambient mutable state inside `handle` or the server object (which would
violate the no-global-state convention and break concurrent connections sharing
one immutable server instance):

```
// the only mutable runtime value; one field
final class McpSession { var initialized: Boolean = false }
```

`handle(request, session, ctx, codecCtx)` reads `session.initialized` to enforce
the pre-`initialize` gate and sets it on a successful `initialize`. The server
object itself stays immutable and shareable across connections. This keeps
`handle` a pure function of `(request, session, ctx)` and leaves connection
identity entirely to the adapter.

### 3.2 What the adapter (hand-written) provides

A v1 integrator writes a thin adapter — NOT generated, NOT part of the contract
surface, but documented as the integration recipe — that:

1. owns the I/O loop (stdio: read line / `Content-Length` frame; HTTP: one
   request body per POST);
2. parses raw bytes → `JsonRpcRequest` using the language's JSON value type
   (`-32700` with `id:null` on parse failure, per K4 §3.1 — raised by the
   adapter, never reaching `handle`);
3. creates/looks-up the per-connection `McpSession` and the per-request `Ctx`;
4. calls `server.handle(request, session, ctx, codecCtx)`;
5. on `Some(response)` serializes it back to bytes and writes it; on `None`
   (notification) writes nothing.

This is the precise analogue of how the service-acceptance harness today wraps
the muxer behind a real HTTP server: the muxer is generated, the HTTP plumbing is
harness code. MCP keeps the same split.

---

## 4. The dispatch entrypoint signature (normative summary)

```
// canonical
interface IBaboonMcpServer<Ctx> {
  handle(
    request:  JsonRpcRequest,
    session:  McpSession,            // per-connection initialized-latch, adapter-owned
    ctx:      Ctx,                   // SAME Ctx as IBaboonJsonServiceCtx<Ctx, R>
    codecCtx: BaboonCodecContext
  ): Option<JsonRpcResponse>          // None == accepted notification, no reply
}
```

- Generic over `Ctx` only (the response shape is fixed by the protocol).
- Synchronous; returns a value; performs no I/O.
- `tools/call` is the only arm that touches `ctx`/`codecCtx`/the muxer.

---

## 5. The tool registry shape

The registry is the generation-time-fixed table that backs both `tools/list` and
`tools/call`. It is an **immutable, ordered** structure built once when the
server object is constructed (or as a static/`lazy` generated constant), keyed by
the K4 §1.2 tool name. One entry per Baboon method bound to this server.

```
// canonical entry
final case class McpToolEntry(
  name:        String,         // K4 §1.2: <serviceName>_<methodName>, verbatim casing
  method:      BaboonMethodId, // reverse map target (K4 §1.3 — NO runtime '_' split)
  description: String | null,  // method doc-comment, omitted if absent (K4 §2.3)
  inputSchema: JsonValue       // self-contained JSON Schema object, from the K3 shared emitter
)
```

- **Where it lives:** a generated immutable list `McpToolEntry[]` plus a derived
  `name -> McpToolEntry` lookup map, both fields of the concrete
  `<Service>McpServer`. Order = model declaration order (service order, then
  method order) so `tools/list` is deterministic (K4 §2.3, §4).
- **`inputSchema` source:** the value is produced by the **dedicated shared MCP
  inputSchema emitter** decided in `decisions:K3` (a self-contained
  `{"type":"object", ...}` with a local `$defs` closure, NOT the OpenAPI
  cross-document emitter), validated for well-formedness at the source per
  `decisions:K1`. It is embedded into the generated code as a constant JSON value
  (string-parsed at construction or emitted as a literal node) — the runtime does
  not compute schemas, it carries the precomputed ones.
- **Codec-backed invoke lives at lookup-then-dispatch:** `tools/call` does
  `registry.lookup(params.name)` → `entry.method`, then
  `muxer.invoke(entry.method, argsJson, ctx, codecCtx)`. The codecs are reached
  exclusively through the existing muxer; the registry holds no codec logic
  itself, only the `BaboonMethodId` to dispatch on. This keeps decode/encode in
  the one place it already lives (the generated service wrapper), avoiding a
  second divergent codec path.
- **Collision handling** (K4 §1.2 rule 4) is a **generation-time** check over
  the candidate `name`s; the runtime registry is built only from an
  already-validated, collision-free set. No runtime de-duplication exists.

---

## 6. Where the runtime helpers live (resource-file convention)

The additive runtime types (§1.1 `JsonRpcRequest`/`JsonRpcResponse`/
`JsonRpcError`, §1.2 `IBaboonMcpServer`, §3.1 `McpSession`, §5 `McpToolEntry`)
are shipped as **static runtime resource files**, exactly like the existing
service-wiring runtime, under:

```
baboon-compiler/src/main/resources/baboon-runtime/<lang>/
```

following the established per-language file conventions verified in the T1
grounding §2:

| lang | proposed MCP runtime resource file (sibling of the service-wiring file) |
|---|---|
| cs | `BaboonMcpRuntime.cs` (alongside `BaboonServiceWiring.cs`, namespace `Baboon.Runtime.Shared`) |
| scala | `BaboonMcpRuntime.scala` (alongside `BaboonServiceWiring.scala`) |
| typescript | `BaboonMcpRuntime.ts`, or appended to `BaboonSharedRuntime.ts` like the service-wiring types |
| kotlin (+ kotlin-kmp) | `BaboonMcpRuntime.kt` (alongside `BaboonServiceWiring.kt`; KMP copy identical) |
| java | `IBaboonMcpServer.java`, `JsonRpcRequest.java`, `JsonRpcResponse.java`, `McpToolEntry.java`, `McpSession.java` (Java's one-type-per-file convention, as the service-wiring runtime already splits) |
| python | `baboon_mcp_runtime.py` (alongside `baboon_service_wiring.py`) |
| rust | `baboon_mcp_runtime.rs` (alongside `baboon_service_wiring.rs`) |
| dart | append to `baboon_runtime.dart` (where the service-wiring types already live) or a sibling `baboon_mcp_runtime.dart` |
| swift | `baboon_mcp_runtime.swift` (alongside `baboon_service_wiring.swift`) |

These resource files are **static** (no per-model templating) — the only
generated, per-model code is the concrete `<Service>McpServer` and its registry
literals, emitted by the per-language `*McpTranslator` (sibling of the existing
`*ServiceWiringTranslator`, T1 grounding §2). Reminder from CLAUDE.md: after
editing any `baboon-runtime/` resource, `sbt clean` before `sbt compile` (the
`PortableResource.embedSources` macro caches resource contents).

---

## 7. Per-language feasibility sketch (all 9)

Each sketch shows the `IBaboonMcpServer` dispatch type + how it reuses the
existing `*MuxerCtx<Ctx, R>` in that language's idiom, proving the contract maps
cleanly **before** T8/T10/T12–T18 implement it. Signatures mirror the verified
service-wiring style for that language.

### scala — generic trait, `Option` return (mirrors `IBaboonJsonServiceCtx[Ctx, R]`)

```scala
trait IBaboonMcpServer[Ctx] {
  def handle(request: JsonRpcRequest, session: McpSession, ctx: Ctx,
             codecCtx: BaboonCodecContext): Option[JsonRpcResponse]
}
final class GreeterMcpServer[Ctx](muxer: JsonMuxerCtx[Ctx, String], ...) extends IBaboonMcpServer[Ctx] { ... }
```
Feasible: identical generic-param + trait shape as `BaboonServiceWiring.scala`'s
`IBaboonJsonServiceCtx[Ctx, R]`. `Option` is idiomatic for the notification case.

### cs — generic interface, `Ctx, R` muxer reuse

```csharp
public interface IBaboonMcpServer<Ctx> {
    JsonRpcResponse? Handle(JsonRpcRequest request, McpSession session, Ctx ctx, BaboonCodecContext codecCtx);
}
public sealed class GreeterMcpServer<Ctx> : IBaboonMcpServer<Ctx> {
    private readonly JsonMuxerCtx<Ctx, string> _muxer;   // existing runtime, reused
    ...
}
```
Feasible: same `<Ctx, R>`-family generics as `BaboonServiceWiring.cs`.
**PascalCase**: the C# *method* is `Handle`, the *property* `InputSchema`, etc.
— per K4 §1.2 rule 2, this PascalCasing is internal symbol casing ONLY; the wire
`tool.name` stays verbatim (`<serviceName>_<methodName>`), and the JSON-RPC
method strings (`"tools/list"` …) and result keys (`"protocolVersion"`,
`"inputSchema"`) are emitted as literal lowercase strings, NOT cased by the C#
convention. `JsonRpcResponse?` nullable models the no-reply case.

### python — `Generic[Ctx]` + `TypeVar`, Protocol (mirrors `Protocol[Ctx, R]`)

```python
Ctx = TypeVar("Ctx")  # already declared in baboon_service_wiring.py

@runtime_checkable
class IBaboonMcpServer(Protocol[Ctx]):
    def handle(self, request: "JsonRpcRequest", session: "McpSession",
               ctx: Ctx, codec_ctx) -> "Optional[JsonRpcResponse]": ...
```
Feasible, **and the known Python hard case is handled**: the MCP runtime file
re-declares/imports `Ctx = TypeVar("Ctx")` and imports `Generic`, `Protocol`,
`Optional`, `runtime_checkable` from `typing` exactly as
`baboon_service_wiring.py` already does (verified: `Ctx = TypeVar("Ctx")` at
py:189, `Protocol[Ctx, R]` at py:193, `Generic[Ctx, R]` at py:206). The concrete
`GreeterMcpServer(Generic[Ctx])` holds a `JsonMuxerCtx[Ctx, str]`.

### rust — generic trait, `Box<dyn>` storage, `Option` return (generic placement)

```rust
pub trait IBaboonMcpServer<Ctx> {
    fn handle(&self, request: &JsonRpcRequest, session: &mut McpSession,
              ctx: Ctx, codec_ctx: &BaboonCodecContext) -> Option<JsonRpcResponse>;
}
pub struct GreeterMcpServer<Ctx> {
    muxer: JsonMuxerCtx<Ctx, String>,        // existing runtime, reused
    tools: Vec<McpToolEntry>,
    by_name: std::collections::BTreeMap<String, usize>,
}
```
Feasible, **generic-placement hard case handled**: `Ctx` is a trait/`struct`
type parameter exactly as `IBaboonJsonServiceCtx<Ctx, R>` and `JsonMuxerCtx<Ctx,
R>` place it (rs:151, rs:161). The muxer already returns `Result<R,
BaboonWiringError>`; the MCP layer turns that `Result` into Channel-A/B per K4
§3 and yields `Option<JsonRpcResponse>`. `session: &mut McpSession` keeps the
latch adapter-owned without a global.

### typescript — generic interface, `R`-free (response shape fixed)

```typescript
export interface IBaboonMcpServer<Ctx> {
  handle(request: JsonRpcRequest, session: McpSession, ctx: Ctx,
         codecCtx: BaboonCodecContext): JsonRpcResponse | undefined;
}
export class GreeterMcpServer<Ctx> implements IBaboonMcpServer<Ctx> {
  constructor(private readonly muxer: JsonMuxerCtx<Ctx, string>, ...) {}
}
```
Feasible: same generic-interface idiom as `BaboonSharedRuntime.ts`'s
`IBaboonJsonServiceCtx<Ctx, R>`. Note the service-wiring `R` defaults to
`Promise<string>` (ts:752) for async; v1 MCP is sync so `<Ctx, string>` is the
concrete muxer instantiation. `| undefined` models the no-reply notification.

### kotlin (+ kotlin-kmp) — generic interface (mirrors `IBaboonJsonServiceCtx<Ctx, R>`)

```kotlin
interface IBaboonMcpServer<Ctx> {
    fun handle(request: JsonRpcRequest, session: McpSession, ctx: Ctx,
               codecCtx: BaboonCodecContext): JsonRpcResponse?
}
class GreeterMcpServer<Ctx>(private val muxer: JsonMuxerCtx<Ctx, String>, ...) : IBaboonMcpServer<Ctx>
```
Feasible: same `<Ctx, R>` generics as `BaboonServiceWiring.kt` (kt:91). KMP copy
is byte-identical (as the service-wiring KMP runtime already is). `?` nullable
for no-reply.

### java — generic interface, `Optional` return, one-type-per-file

```java
public interface IBaboonMcpServer<Ctx> {
    Optional<JsonRpcResponse> handle(JsonRpcRequest request, McpSession session,
                                     Ctx ctx, BaboonCodecContext codecCtx) throws Exception;
}
```
Feasible: same generic + `throws Exception` shape as `IBaboonJsonServiceCtx.java`
(jv:15). Java's one-public-type-per-file rule means each runtime type is its own
`.java` resource (§6), matching how the service-wiring runtime is already split.

### dart — generic class interface, nullable return

```dart
abstract interface class IBaboonMcpServer<Ctx> {
  JsonRpcResponse? handle(JsonRpcRequest request, McpSession session,
                          Ctx ctx, BaboonCodecContext codecCtx);
}
```
Feasible: same `<Ctx, R>` generic interface idiom as `baboon_runtime.dart`'s
`IBaboonJsonServiceCtx<Ctx, R>` (dart:1008). `JsonRpcResponse?` for no-reply.

### swift — associated-type protocol + type-eraser (the known hard case)

Swift cannot put generic params on a protocol, so — exactly as the service-wiring
runtime does — `IBaboonMcpServer` uses an `associatedtype Ctx`, paired with an
`AnyMcpServer<Ctx>` type-eraser so callers/adapters can hold a concrete
`(Ctx)`-typed value:

```swift
public protocol IBaboonMcpServer {
    associatedtype Ctx
    func handle(_ request: JsonRpcRequest, _ session: inout McpSession,
                _ ctx: Ctx, _ codecCtx: BaboonCodecContext) throws -> JsonRpcResponse?
}

public struct AnyMcpServer<Ctx> {
    private let _handle: (JsonRpcRequest, inout McpSession, Ctx, BaboonCodecContext) throws -> JsonRpcResponse?
    public init<S: IBaboonMcpServer>(_ s: S) where S.Ctx == Ctx { self._handle = s.handle }
    public func handle(_ r: JsonRpcRequest, _ s: inout McpSession, _ ctx: Ctx,
                       _ c: BaboonCodecContext) throws -> JsonRpcResponse? { try _handle(r, &s, ctx, c) }
}
```
Feasible: this is the verbatim pattern of `IBaboonJsonServiceCtx` (`associatedtype
Ctx, R`, sw:171) + `AnyJsonServiceCtx<Ctx, R>` (sw:185). The concrete
`GreeterMcpServer<Impl: Greeter>` binds `Ctx = Impl.Ctx` and holds the existing
`JsonMuxerCtx<Ctx, String>` (sw:223). `throws` carries the same error surface the
service-wiring protocols already use; `inout McpSession` keeps the latch
caller-owned. No generic-on-protocol is required.

---

## 8. Consistency requirements across the 9 backends (additive to K4 §4)

For the same input model, the following MUST be structurally identical across all
9 backends (in addition to K4 §4's wire-level invariants):

1. The `handle` dispatch arms and their `request.method` → behaviour mapping
   (§1.3 table) — same set, same order of checks, same Channel-A/B outcome.
2. The tool-registry contents and order (§5) — derived from the same
   declaration-order traversal and the same K4 §1.2 names + K3 schemas.
3. The set of additive runtime types (§1) — same names (modulo per-language
   spelling in §7), same fields, same method signature shape.
4. The transport seam (§3) — NO backend bakes in an I/O loop; all expose the same
   `handle(request, session, ctx, codecCtx)` entrypoint and the same adapter
   contract.

Per-language symbol casing, file splitting, nullable-vs-`Option`-vs-`?`
encoding, and associated-type-vs-generic encoding are free to follow each
language's idiom (§7) because none of them is observable on the wire.

---

## 9. Provenance / cross-references

- Goal: `goals:G1` (MCP server generators across all backends).
- Milestone: `decisions:M1` (Plan: MCP server generators).
- This note backs the new locked decision created this session (the MCP dispatch
  runtime contract).
- Wire contract (sibling): `decisions:K4` / `docs/research/mcp-protocol-contract.md`.
- inputSchema strategy: `decisions:K3`; validity coverage: `decisions:K1`.
- Grounding (verified line refs): `docs/research/mcp-generators-grounding.md` §2.
- Drives: T8/T10 (reference per-lang MCP translators) and T12–T18 (replicas).
