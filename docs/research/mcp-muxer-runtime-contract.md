# MCP Muxer Runtime Contract (v1) — language-agnostic specification

Status: design note for task `tasks:T103`.
Author: opus-4.8[1m]. Session: 69f1bb6c-0332-40c1-972f-7a2dc7467581.
Date: 2026-06-16. Base commit: `c41cf493c557e588bb4ccd53e44578f4efc1ce4f`.

This note fixes the **cross-service MCP muxer contract** that every one of the 9
per-language Baboon backends (scala, cs, python, rust, typescript, kotlin, java,
dart, swift) MUST target. It is the MCP-tier sibling of the service muxer
(`JsonMuxer`/`UebaMuxer`, grounded below) and a one-tier-up consumer of the MCP
dispatch runtime contract in `docs/research/mcp-dispatch-runtime-contract.md`:
that note fixes the *per-service* `<Service>McpServer` `handle()` state machine;
this note fixes the *cross-service* `AbstractMcpMuxer<Ctx>` that **composes
several such servers behind one MCP endpoint** so a single connection can serve
the union of multiple services' tools.

The guiding constraint is the same as the dispatch contract: the muxer is
**transport-abstract**. It implements the JSON-RPC method state machine over a
*set of registered servers* only. Bytes-in/bytes-out (stdio framing,
Streamable-HTTP request bodies) remains the **injected adapter**'s job; the
muxer never touches it. The muxer is to `<Service>McpServer` exactly what
`JsonMuxer` is to a single `IBaboonJsonService`: a fan-in router keyed by a flat
name, fail-fast on duplicate registration, fail-fast on unknown lookup.

It is grounded against the actually-shipped source, NOT hypothesised:

- service muxer precedent — `baboon-runtime/java/JsonMuxer.java`
  (`LinkedHashMap<String, IBaboonJsonService<R>>` keyed by `serviceName`,
  `register()` throwing `DuplicateService`, `invoke()` throwing
  `NoMatchingService`; jv:21–48);
- the `BaboonWiringError` taxonomy — `baboon-runtime/java/BaboonWiringError.java`
  (`NoMatchingMethod`/`NoMatchingService`/`DuplicateService`/`DecoderFailed`/
  `EncoderFailed`/`CallFailed`; jv:3–10);
- the per-service MCP `handle()` state machine and its access modifiers —
  `baboon-runtime/java/AbstractBaboonMcpServer.java` (jv:21–145),
  `baboon-runtime/typescript/BaboonMcpRuntime.ts` (ts:117–173),
  `baboon-runtime/scala/BaboonMcpRuntime.scala` (sc:88–197),
  `baboon-runtime/swift/baboon_mcp_runtime.swift` (sw:127–301),
  `baboon-runtime/rust/baboon_mcp_server.rs` (rs:108–263);
- the additive MCP value types — `McpToolEntry`, `McpServerInfo`, `McpSession`,
  `JsonRpcRequest`/`JsonRpcResponse`, `McpProtocol` (java resource files of the
  same name; mirrored per language).

---

## 0. The composition problem this note solves (R112)

The per-service `<Service>McpServer` already implements the full `handle()` state
machine, but it only knows **its own** tools and routes `tools/call` only into
**its own** service wiring. To expose two or more services through one MCP
endpoint we need a router that:

1. registers several `<Service>McpServer<Ctx>` instances;
2. answers `tools/list` with the **union** of their tools;
3. routes each `tools/call` to the **one server that owns** the named tool;
4. answers `initialize`/`notifications/initialized` once, at the muxer level.

The blocking obstacle today (review R112, criticism 1): the per-service base
**hides exactly the members a sibling router needs**. In every backend with an
inheritance-based base, `serverInfo`/`tools`/`invokeJson` are `protected` and the
`name -> entry` lookup is `private`:

| backend | base | hidden member (verified) |
|---|---|---|
| java | `AbstractBaboonMcpServer` | `protected serverInfo()` jv:24, `protected tools()` jv:25, `protected invokeJson()` jv:26, `private byName()` jv:29 |
| cs | `AbstractBaboonMcpServer<Ctx>` | `protected abstract ServerInfo` cs:138, `protected abstract Tools` cs:139, `protected abstract InvokeJson` cs:140, `private ByName()` cs:142 |
| ts | `AbstractBaboonMcpServer<Ctx>` | `protected abstract readonly serverInfo` ts:118, `protected abstract readonly tools` ts:119, `protected abstract invokeJson` ts:120, `private byName()` ts:122 |
| scala | `AbstractBaboonMcpServer[Ctx]` | `protected val serverInfo` sc:110, `protected val tools` sc:111, `protected def invokeJson` sc:112, `private def byName()` sc:114 |
| swift | `IBaboonMcpServer` protocol-extension | `serverInfo`/`tools`/`invokeJson` protocol requirements + `private func byName()` sw:151 |
| python | `AbstractBaboonMcpServer` | `@abstractmethod server_info`/`tools`/`invoke_json`, `_by_name()` (name-private) py:92–111 |
| rust | `BaboonMcpServerBase` **(already public)** | `pub server_info` rs:135, `pub tools` rs:136, `pub handle_request` rs:144, `pub type McpJsonInvoke` rs:124; only `fn by_name` rs:140 is private |

A sibling muxer **cannot** read these to build a union table, and it must NOT
route `tools/call` by re-entering a server's `handle()`: `handle()` resolves the
name against only THAT server's `byName()` and returns `unknown tool` for any
cross-service name (jv:93–95). It is structurally incapable of cross-service
routing.

**Therefore the muxer composes on the PUBLIC routable-server surface introduced
in `tasks:T114`, NOT on protected members and NOT on `handle()`.** This note
specifies the muxer against that public surface; T114 is its hard dependency.

### 0.1 The T114 public routable-server surface (what the muxer depends on)

T114 promotes the three composition inputs the muxer needs to a **public,
stable accessor surface** on every `<Service>McpServer` (a public interface
`IBaboonRoutableMcpServer<Ctx>`, or public accessors on the base, per each
language's idiom — rust already satisfies this via `BaboonMcpServerBase`'s
public fields and `McpJsonInvoke`). The muxer reads ONLY:

| accessor (canonical spelling) | returns | replaces today's hidden member |
|---|---|---|
| `serverInfo(): McpServerInfo` | this server's identity | `protected serverInfo` |
| `tools(): List<McpToolEntry>` | this server's declaration-ordered tool registry | `protected tools` |
| `routeToolCall(method, argsJson, ctx, codecCtx): R` | the per-service dispatch entry that reuses Channel-A/B mapping unchanged | `protected invokeJson` |

`routeToolCall` is the **public name for the T114 routable dispatch entry**: it
is the same code path `<Service>McpServer.handle()` calls for its own
`tools/call` arm (`invokeJson` → `JsonMuxerCtx.invoke` → service impl,
dispatch-contract §1.3, §2), exposed so the muxer reuses Channel-A/Channel-B
mapping verbatim rather than reimplementing it. The muxer NEVER calls a server's
`handle()`.

---

## 1. `AbstractMcpMuxer<Ctx>` — the runtime type

The muxer is an **additive runtime helper** shipped as a resource file alongside
the existing MCP runtime (e.g. appended to `BaboonMcpRuntime.{cs,kt,scala}` /
`baboon_mcp_runtime.{py,swift,dart}` / `baboon_mcp_server.rs` /
`BaboonMcpRuntime.ts`, and as `AbstractMcpMuxer.java` under Java's
one-type-per-file rule). It is emitted only when the MCP target is selected; it
is **static** (no per-model templating) exactly like `JsonMuxer`. Canonical
generic-parameter spelling below; per-language spelling follows §7 of the
dispatch contract.

It mirrors `JsonMuxer<R>` one tier up: where `JsonMuxer` keys
`Map<serviceName, IBaboonJsonService>` and routes by `method.serviceName()`, the
muxer keys `Map<toolName, owningServer>` and routes by the inbound flat MCP tool
name.

```
// canonical (cs/scala/ts spelling)
final class AbstractMcpMuxer<Ctx> {
  // registration order preserved (LinkedHashMap precedent, JsonMuxer jv:22)
  private final servers: List<IBaboonRoutableMcpServer<Ctx>>
  // tool-name -> owning server, built at registration (§2)
  private final route: Map<String, IBaboonRoutableMcpServer<Ctx>>
  // tool-name -> entry, for the tools/list union and per-call BaboonMethodId
  private final entries: Map<String, McpToolEntry>   // insertion-ordered
  private final mergedServerInfo: McpServerInfo

  // varargs ctor mirrors `JsonMuxer(IBaboonJsonService... services)` jv:24
  AbstractMcpMuxer(McpServerInfo mergedServerInfo, IBaboonRoutableMcpServer<Ctx>... servers)

  void register(IBaboonRoutableMcpServer<Ctx> server)                 // §2, throws DuplicateTool
  Option<JsonRpcResponse> handle(JsonRpcRequest, McpSession, Ctx, BaboonCodecContext)  // §3
}
```

### Method roster (acceptance: name the methods)

- **`register(server)`** — §2. Folds the server's `tools()` into the union table
  in declaration order; throws `DuplicateTool` on a tool-name collision across
  servers. Records the server in `servers` (registration order) and its
  `serverInfo()` into the merge.
- **`handle(request, session, ctx, codecCtx)`** — §3. The same JSON-RPC state
  machine as the per-service base, but `tools/list` returns the union and
  `tools/call` routes by tool name to the owning server. Returns `None`/`null`/
  `nil`/`Optional.empty` for the accepted notification, exactly as the
  per-service base (jv:60).
- **`toolsListUnion()`** (internal, backs `tools/list`) — §3.2.
- **`routeToolsCall(toolName, argsJson, ctx, codecCtx)`** (internal, backs
  `tools/call`) — §3.3. Looks up the owning server, delegates to its public
  `routeToolCall`.

---

## 2. The tool-name → owning-server table (built at registration)

Built once, at registration, NOT per request — mirroring how `JsonMuxer`
populates its `LinkedHashMap` in `register()` (jv:31–36) and how the per-service
registry is generation-time-fixed (dispatch contract §5).

Algorithm, for each server in **registration order**, for each
`McpToolEntry t` in that server's `tools()` in **declaration order**
(`tools()` is already declaration-ordered — dispatch contract §5):

1. if `route.containsKey(t.name)` → throw `DuplicateTool(t.name)` (§6) —
   the exact analogue of `JsonMuxer.register` throwing `DuplicateService` when
   `table.containsKey(service.serviceName())` (jv:32–34);
2. `route.put(t.name, server)`; `entries.put(t.name, t)`.

`route` and `entries` are **insertion-ordered** maps (`LinkedHashMap` in
java/kotlin, `LinkedHashMap`/insertion-ordered `dict` in python, an order-
preserving `Vec<(name, idx)>` or `IndexMap` in rust, `Map` literal in ts which
preserves insertion order, `mutable.LinkedHashMap` in scala) so the union in §3.2
is deterministic. This is the union read **via the public `tools()` accessor**
(T114) — never via a protected field.

---

## 3. `handle` — the muxer JSON-RPC state machine

`handle` is the SAME `switch (request.method)` shape as the per-service base
(AbstractBaboonMcpServer.handle, jv:38–135), with three arms differing only in
that they operate over the union rather than one server:

| `request.method` | muxer action | per-service analogue |
|---|---|---|
| `"initialize"` | validate `params.protocolVersion` present (else `-32602` `INVALID_PARAMS`); set `session.initialized = true`; return the fixed result with the **merged** `serverInfo` (§3.1) and `capabilities.tools = {}`, `protocolVersion = McpProtocol.VERSION`. | jv:41–58 |
| `"notifications/initialized"` | accept, return no response (`None`/`null`). | jv:59–61 |
| `"tools/list"` | require `session.initialized` (else `-32600` `INVALID_REQUEST`); return `{ tools: [union] }` (§3.2). | jv:62–79 |
| `"tools/call"` | require `session.initialized` (else `-32600`); read `params.name` (missing/non-string → `-32602`); route by name (§3.3); unknown name → `NoMatchingTool` (§6). | jv:80–131 |
| anything else | `-32601` `METHOD_NOT_FOUND`. | jv:132–134 |

The error codes, the `params`/`name` validation, the `arguments`→string
serialization, and the Channel-A (`error`) vs Channel-B (`isError:true` content)
mapping are **reused verbatim** from the per-service base — the muxer must NOT
diverge on protocol bytes (consistency requirement, dispatch contract §8).

### 3.1 `initialize` — single merged serverInfo

The muxer answers `initialize` ONCE at the muxer level (NOT by delegating to any
member server). It returns a single `mergedServerInfo` — supplied to the muxer
ctor (the composed endpoint's name/version, e.g. `"baboon-mcp"` + the model
version), built from the member servers' `serverInfo()` read **via the public
accessor** (T114). Members' individual `serverInfo`s are not surfaced
per-connection; the protocol exposes exactly one. The `protocolVersion`
(`McpProtocol.VERSION` = `"2025-06-18"`) and `capabilities.tools = {}` are
identical to the per-service base (jv:54–55).

### 3.2 `tools/list` — the union, with a fixed ordering rule

Returns `{ tools: [...] }` where the array is the **union of all registered
servers' tool entries**, ordered by:

> **registration order of servers, then each server's tool declaration order.**

i.e. the iteration order of `entries` as populated in §2. Each emitted entry has
the same shape the per-service base emits (jv:67–74): `name`, `inputSchema`, and
`description` only when non-null. Because `entries` is insertion-ordered and the
collision check guarantees no name appears twice, the union is deterministic and
collision-free.

### 3.3 `tools/call` — route by flat tool name to the owning server

1. extract `params.name` (same validation as jv:84–91: missing → `-32602`;
   non-string → `-32602`);
2. `server = route.get(toolName)`; if absent → **`NoMatchingTool(toolName)`**
   (§6) surfaced as the protocol error the per-service base uses for an unknown
   tool (`-32602` `INVALID_PARAMS`, "unknown tool '<name>'", jv:94–95) — the
   muxer reuses that mapping so the wire response is identical whether a single
   server or the muxer rejects the name;
3. `entry = entries.get(toolName)` → `entry.method` (the `BaboonMethodId`);
4. serialize `params.arguments` to a JSON string (default `{}` if absent —
   jv:97–99);
5. call **`server.routeToolCall(entry.method, argsJson, ctx, codecCtx)`** — the
   T114 PUBLIC dispatch entry, NOT `server.handle(...)`. This reuses the owning
   server's existing Channel-A/Channel-B mapping unchanged (dispatch contract
   §1.3 `tools/call` arm): a `Right`/`Ok` payload becomes
   `{content:[{type:"text",text:...}], isError:false}`; a `Left`/`Err`
   `BaboonWiringError` becomes `isError:true` content (jv:106–130).

`ctx` and `codecCtx` thread through the muxer untouched, exactly as they thread
through the per-service `handle` (dispatch contract §2): the muxer adds no new
context concept.

---

## 4. Why not `handle()` and why not protected members (acceptance 4 + R112)

- **Not `handle()`:** a member's `handle()` resolves `tools/call` against its own
  `byName()` only (jv:93) and returns `unknown tool` for any name it does not
  own. Re-entering `handle()` from the muxer would make every cross-service tool
  fail. The muxer therefore bypasses `handle()` and calls the narrower public
  dispatch entry `routeToolCall` that maps `BaboonMethodId → service impl` and
  reuses Channel-A/B — the muxer owns the JSON-RPC envelope, the member owns the
  domain dispatch.
- **Not protected members:** `serverInfo`/`tools`/`invokeJson` are `protected`
  and `byName` is `private` in every inheritance-based backend (§0 table), so a
  sibling muxer in the same package cannot compose them portably (protected ≠
  package-visible across all 9 idioms; private is never visible). T114's public
  accessor surface is the sanctioned composition seam.

---

## 5. The ordering rule (normative)

> **`tools/list` order = registration order of servers, then per-server tool
> declaration order; equivalently, the insertion order of the §2 union table.**

This is stable, deterministic, and reproducible across all 9 backends because
(a) `JsonMuxer` already preserves registration order via `LinkedHashMap`
(jv:22), and (b) each server's `tools()` is already declaration-ordered
(dispatch contract §5). No sorting, no de-duplication at request time — a
duplicate would already have failed registration (§6).

---

## 6. Errors — `DuplicateTool` and `NoMatchingTool`

**Placement decision: add both cases to the existing `BaboonWiringError` ADT** in
each `baboon-runtime/<lang>` service-wiring runtime (the **preferred reuse**
path stated in the task), NOT a new MCP-specific error type. Rationale:

- `DuplicateTool` is the exact MCP-tier analogue of `DuplicateService`
  (BaboonWiringError.java jv:5) and `NoMatchingTool` of `NoMatchingService`
  (jv:4); they share the registration-time-throw / dispatch-time-throw split and
  the same `BaboonWiringException` carrier the muxer/`JsonMuxer` already use
  (jv:33, jv:41). Reusing the ADT keeps one error taxonomy, not two.
- It is additive to an existing sealed/`enum`/`sealed interface` ADT — the same
  shape as adding `DuplicateService` did.

```
// added to baboon-runtime/java/BaboonWiringError.java (and per-lang siblings)
record DuplicateTool(String toolName) implements BaboonWiringError {}
record NoMatchingTool(String toolName) implements BaboonWiringError {}
```

Per-language placement (sealed-trait/`enum`/sealed-interface member, mirroring
where `DuplicateService`/`NoMatchingService` already live):

| backend | `BaboonWiringError` location | add cases as |
|---|---|---|
| java | `baboon-runtime/java/BaboonWiringError.java` | two `record`s |
| cs | `BaboonServiceWiring.cs` | two sealed-record cases |
| scala | `BaboonServiceWiring.scala` | two `final case class`es |
| typescript | `BaboonSharedRuntime.ts` | two union members |
| kotlin (+kmp) | `BaboonServiceWiring.kt` | two `sealed`/`data class`es |
| python | `baboon_service_wiring.py` | two dataclass cases |
| rust | `baboon_service_wiring.rs` | two `enum` variants |
| dart | `baboon_runtime.dart` | two sealed-subclass cases |
| swift | `baboon_service_wiring.swift` | two `enum` cases |

**Throw sites:**

- `DuplicateTool(toolName)` — thrown in `register()` (§2) when two registered
  servers contribute the same `t.name`, via `BaboonWiringException` (the
  `DuplicateService` precedent, jv:33).
- `NoMatchingTool(toolName)` — surfaced in `tools/call` (§3.3 step 2) when the
  inbound name has no owning server. Per the per-service "unknown tool" precedent
  (jv:94–95) it is mapped to a JSON-RPC `-32602` `INVALID_PARAMS` response with
  message `tools/call: unknown tool '<name>'`, so the wire bytes are identical to
  a single server rejecting the name. (`NoMatchingTool` is the typed internal
  carrier; the wire mapping reuses the existing per-service path.)

`DuplicateTool` is a **registration-time programmer error** (mis-wired
composition) and propagates as `BaboonWiringException` to the integrator, exactly
as `DuplicateService` does — it is never a per-request protocol response.

---

## 7. Per-backend sync/async matrix (R112 criticism 3 — NORMATIVE)

Async parity is **per-backend, NOT uniform**. The muxer's `handle` return shape
and `routeToolCall` result container follow the SAME axis the per-service MCP
base already takes in that backend (verified against each runtime):

| backend | async MCP base exists? | muxer surface | evidence |
|---|---|---|---|
| **cs** | yes (`AbstractBaboonMcpServerAsync<Ctx>`, `Handle → Task<JsonRpcResponse?>`) | sync `AbstractMcpMuxer` **and** async `AbstractMcpMuxerAsync` (`routeToolCall → Task<Either<..>>`, `Handle → Task<..>`) | cs:253–257, async delegate cs:122 |
| **python** | yes (`AbstractBaboonMcpServerAsync`, `async def handle`) | sync **and** async muxer (`async def route_tool_call`, `async def handle`) | py:248–264, async delegate py:233 |
| **rust** | yes (`--rs-async-services`; delegate `McpJsonInvoke` return container) | sync **and** async muxer threading the async result container | rs:124, service-wiring async rs:39–40 |
| **swift** | yes (`IBaboonAsyncMcpServer`, `async throws` handle) | sync **and** async muxer (`async throws routeToolCall`, `async throws handle`) | sw:274–301 |
| **kotlin** | **no** (sync only) | sync-only muxer (`handle → JsonRpcResponse?`) | BaboonMcpRuntime.kt — no `suspend`/async base |
| **java** | **no** (sync only; `handle` returns `JsonRpcResponse`) | sync-only muxer | AbstractBaboonMcpServer.java jv:38 — no `CompletableFuture` MCP base |
| **dart** | **no** (sync only) | sync-only muxer | baboon_mcp_runtime.dart — no `Future`/async base |
| **scala** | **Either-only** (D24/T69: MCP base is sync + Either-shaped) | Either-only muxer (`routeToolCall → Either[BaboonWiringError, String]`, `handle → Option[JsonRpcResponse]`) | sc:98–112; CLAUDE.md "Scala MCP requires Either-mode" |

Concretely:

- **cs / python / rust / swift (async-capable):** the muxer is emitted in BOTH a
  sync and an async flavour, mirroring the existing `AbstractBaboonMcpServer` /
  `AbstractBaboonMcpServerAsync` split. The async muxer threads the async result
  container of `routeToolCall` (`Task<Either<..>>` / awaitable
  `BaboonEither[..]` / async `Result<String, BaboonWiringError>` /
  `async throws -> String`) and is itself `async`/`Task`/awaitable; the
  Channel-A/B mapping is applied AFTER the single `await` hop, exactly as the
  per-service async base does. The `--<lang>-async-services=false` path emits the
  byte-identical sync muxer.
- **kotlin / java / dart (sync-only):** there is no async axis. The muxer is
  sync-only: `handle → JsonRpcResponse?` and `routeToolCall` returns the sync
  service-result container. No `suspend`/`CompletableFuture`/`Future` muxer is
  emitted.
- **scala (Either-pinned, D24/T69):** the MCP base is synchronous AND
  Either-shaped — `handle` matches `Right`/`Left` on the wiring's `invokeJson`
  result (sc:174). The muxer is therefore **Either-only**: `routeToolCall`
  returns `Either[BaboonWiringError, String]`, `handle` returns
  `Option[JsonRpcResponse]`. Combining a non-Either `serviceResult` with Scala
  MCP is already rejected up front (`TranslationIssue.ScalaMcpRequiresEither`),
  so the muxer never needs a non-Either Scala path. No async/HKT muxer is
  emitted for Scala (the async axis is the out-of-scope D24 rework).

---

## 8. Consistency requirements across the 9 backends

For the same set of registered servers, the following MUST be structurally
identical across all 9 backends (additive to the dispatch contract §8):

1. The muxer `handle` dispatch arms and their `request.method` → behaviour
   mapping (§3 table) — same set, same checks, same error codes, same
   Channel-A/B outcome as the per-service base.
2. The §2 union-table build (declaration-order fold, `DuplicateTool` on
   collision) and the §5 ordering rule.
3. `tools/call` routes via the T114 **public** `routeToolCall`, never via
   `handle()`, never via protected members.
4. `DuplicateTool`/`NoMatchingTool` live in the existing `BaboonWiringError`
   ADT (§6), with `DuplicateTool` mirroring `DuplicateService` and
   `NoMatchingTool` mirroring `NoMatchingService`.

Free to follow each language's idiom (none observable on the wire): the sync vs
async muxer split (§7), symbol casing, file splitting, `Option`-vs-`?`-vs-
`undefined` encoding, and associated-type-vs-generic encoding (swift).

---

## 9. Verification

This is a DESIGN doc; verification is **internal consistency against the cited
source**, not a compile (the worktree cannot run `sbt`/`mdl` — sbt-git fails in a
linked worktree, CLAUDE.md). Every structural claim above carries a `file:line`
citation to a shipped resource file:

- service-muxer precedent (LinkedHashMap, register/invoke, `DuplicateService`/
  `NoMatchingService`): `JsonMuxer.java` jv:21–48, `BaboonWiringError.java`
  jv:3–10 — the §2/§5/§6 design mirrors these exactly.
- per-service `handle()` state machine (the arms the muxer reuses verbatim):
  `AbstractBaboonMcpServer.java` jv:38–135 — the §3 table maps arm-for-arm.
- the protected/private access modifiers that force the T114 public surface (§0,
  §4): jv:24–29, cs:138–142, ts:118–122, sc:110–114, sw:151, py:92–111;
  rust's already-public `BaboonMcpServerBase` (rs:124–144) confirms the surface
  is feasible.
- the per-backend async/sync/Either matrix (§7): cs:253–257, py:248–264,
  sw:274–301, rs:39–40/rs:124, sc:98–112, and the absence of any async MCP base
  in kotlin/java/dart runtimes; cross-checked against CLAUDE.md's "Scala MCP
  requires Either-mode serviceResult (D24/T69)" note.

---

## 10. Provenance / cross-references

- Task: `tasks:T103` (this note).
- Dispatch contract (sibling, one tier down): `docs/research/mcp-dispatch-runtime-contract.md`.
- Wire contract: `docs/research/mcp-protocol-contract.md` (`decisions:K4`).
- Hard dependency: `tasks:T114` (the public routable-server surface the muxer composes).
- Reviewed against: `JsonMuxer.java` (service-muxer precedent) and the
  `AbstractBaboonMcpServer.handle()` state machine for consistency (§9).
