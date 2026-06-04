# MCP Protocol Contract (v1) — language-agnostic specification

Status: locked decision (ledger `decisions:K4`, milestone M1, ledgerRefs `goals:G1`).
Author: opus-4.8[1m]. Session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b.
Date: 2026-06-04.

This document fixes the **wire contract** that every one of the 9 per-language
Baboon MCP server generators (scala, cs, python, rust, typescript, kotlin,
java, dart, swift) MUST implement **identically**. Anything not nailed down
here becomes 9 divergent implementations, so the shapes below are normative and
exhaustive for v1.

It is grounded in:

- **JSON-RPC 2.0** — <https://www.jsonrpc.org/specification> (request/response/
  error object shapes, reserved error codes).
- **Model Context Protocol**, revision **2025-06-18** — base protocol
  (<https://modelcontextprotocol.io/specification/2025-06-18/basic>),
  lifecycle/`initialize` (`.../basic/lifecycle`), and the **Tools** server
  feature (`.../server/tools`). MCP frames all messages as JSON-RPC 2.0.
- The established Baboon service-wiring contract it must mirror:
  `baboon-compiler/src/main/resources/baboon-runtime/cs/BaboonServiceWiring.cs`
  (`BaboonMethodId(ServiceName, MethodName)`, the `Invoke(method, data, ctx,
  codecCtx)` dispatch entrypoint, the `IBaboon{Json,Ueba}ServiceCtx<Ctx,R>`
  interfaces, and the `BaboonWiringError` taxonomy: `NoMatchingService`,
  `NoMatchingMethod`, `DecoderFailed`, `EncoderFailed`, `CallFailed`,
  `DuplicateService`). See the T1 grounding note
  `docs/research/mcp-generators-grounding.md` §2 for the per-language line refs.

---

## 0. Scope (per G1 / Q1 / Q2) — what v1 IS and IS NOT

**v1 IS:**

- **One MCP server per Baboon service.** A Baboon `service` (the typer's
  `Typedef.Service`) becomes one MCP server surface. Composition of multiple
  services behind one server is handled by the existing cross-domain *muxer*
  (`JsonMuxer` keyed by `method.ServiceName`); see §1.3.
- **Each service method = exactly one MCP tool.** No grouping, no fan-out: a
  1:1 map from Baboon method to MCP `tool`.
- **Tools-only protocol surface.** Exactly three JSON-RPC methods are
  implemented: `initialize`, `tools/list`, `tools/call`. Plus the one
  lifecycle notification the spec *requires* after `initialize`
  (`notifications/initialized`), accepted and ignored (§2.1).
- **Transport-abstract.** The generator emits the protocol state machine +
  tool dispatch ONLY. Bytes in / bytes out is an injected adapter; there is no
  baked-in stdio/HTTP I/O loop, mirroring the abstract-context server contract
  (`invoke(method, data, ctx, codecCtx)`). The dispatch entrypoint consumes a
  parsed JSON-RPC request value and returns a JSON-RPC response value (§1.4).
- **JSON only.** Tool argument/result (de)serialization reuses each language's
  existing **JSON** codecs. UEBA is out of scope (MCP is JSON-RPC).

**v1 IS NOT (explicit exclusions — MUST NOT be emitted in v1):**

- `resources/*` (resources list/read/templates/subscribe) — OUT.
- `prompts/*` (prompts list/get) — OUT.
- `completion/complete` (argument autocompletion) — OUT.
- `sampling/*` (server→client LLM sampling) — OUT.
- `roots/*`, `elicitation/*`, `logging/setLevel` — OUT.
- **All notifications beyond `notifications/initialized`** — OUT. In
  particular `notifications/tools/list_changed` is NOT emitted (the tool set is
  static, fixed at generation time), so the server MUST NOT advertise
  `tools.listChanged: true` (§2.2).
- Pagination of `tools/list` (`cursor` / `nextCursor`) — OUT for v1; the full
  tool set is returned in one response (§2.3).
- Real transport, multi-language NxN client matrix — OUT (v1 test rigor is a
  local round-trip through the dispatch entrypoint; see G1 Q5).

---

## 1. Service → tool model

### 1.1 Identifiers from the Baboon model

A Baboon method is identified, in the existing runtime, by
`BaboonMethodId(serviceName, methodName)` — both are plain strings derived from
the typed model. The MCP layer is a thin renaming/dispatch shell over this same
identity.

### 1.2 Tool-naming scheme (NORMATIVE)

The MCP `tool.name` MUST be a stable, collision-free, deterministic function of
`(serviceName, methodName)`:

```
tool.name  =  <serviceName> "_" <methodName>
```

Rules:

1. **Separator.** A single ASCII underscore `_` joins service and method. We use
   `_` (not `/`, `.`, or `:`) because: (a) MCP tool names are matched verbatim
   as JSON-RPC `params.name` strings and many clients restrict tool names to
   `[A-Za-z0-9_-]`; (b) it round-trips unambiguously given the constraints in
   rule 3.
2. **Casing is preserved verbatim** from the Baboon model. The generator does
   NOT apply per-language casing (no C#-style PascalCase here): the wire tool
   name is identical across all 9 languages. (Per-language *symbol* casing
   stays inside the generated handler code, never on the wire.) This is the
   single most divergence-prone point — all 9 backends MUST emit byte-identical
   `tool.name` for the same model.
3. **Component charset.** Baboon identifiers for services and methods are
   already restricted to `[A-Za-z_][A-Za-z0-9_]*` by the parser. To keep the
   `serviceName "_" methodName` join injective, the **service name component
   MUST NOT itself contain `_`-ambiguity that collides with another
   `(service, method)` pair**. Because both components are full Baboon
   identifiers (which DO allow `_`), the join `a_b` + `c` and `a` + `b_c` would
   both yield `a_b_c`. v1 resolves this deterministically (rule 4) rather than
   by escaping, to keep names human-readable.
4. **Collision handling (NORMATIVE).** Within a single generated MCP server the
   set of tool names MUST be unique. The generator computes candidate names per
   §1.2 across ALL methods of ALL services bound to that server, then:
   - if all candidates are unique → use them as-is;
   - if any two candidates collide → the generator MUST **fail compilation**
     with a deterministic diagnostic naming the two colliding
     `(service, method)` pairs. v1 does NOT silently disambiguate (no numeric
     suffixing), because a silent rename would diverge across the 9 backends and
     break the client's stable tool-name contract. A collision is a model-author
     error to be surfaced, not hidden.

   Rationale for fail-fast over auto-suffix: auto-suffix order would depend on
   iteration order, which is not guaranteed identical across 9 language
   runtimes; fail-fast is the only choice that cannot diverge.
5. **No cross-domain namespacing in the wire name.** Since v1 is *one server
   per service surface* and the muxer already routes by `serviceName`,
   `<serviceName>_<methodName>` is globally sufficient. Domain/package
   qualification is NOT prepended to the tool name (it would bloat names and is
   not needed for routing). If a future version composes services with
   identical names from different domains behind one server, that is a
   follow-up; v1's collision rule (rule 4) catches the duplicate-service-name
   case via the existing `DuplicateService` wiring error.

### 1.3 Reverse mapping (tool → method) at `tools/call`

On `tools/call`, the server maps `params.name` back to a
`BaboonMethodId(serviceName, methodName)` using the SAME generated table built
in §1.2 (forward map inverted at generation time). The server MUST NOT re-parse
the name by splitting on `_` at runtime (rule 3 makes splitting ambiguous);
instead it looks `params.name` up in the generated `name → BaboonMethodId`
table. Unknown name → tool-not-found (§3, JSON-RPC `-32602`, see decision in
§3.2).

### 1.4 Dispatch entrypoint (transport-abstract)

The generated MCP server exposes a single synchronous (R-parametric, per the
existing muxer's `R` convention) entrypoint analogous to the service-wiring
`Invoke`:

```
handle(request: JsonRpcRequest) : JsonRpcResponse
```

- Input: an already-parsed JSON-RPC request value (the transport adapter owns
  framing/parsing of raw bytes).
- Output: a JSON-RPC response value (or, for a notification, nothing — §2.1).
- Internally, `tools/call` delegates to the existing JSON service dispatch
  (`JsonMuxer.Invoke(BaboonMethodId, data, codecCtx)` / the
  `IBaboonJsonServiceCtx` ctx-carrying variant when `service.context` is
  active), reusing the per-language JSON codecs for argument decoding and result
  encoding. No new transport, no I/O loop.

---

## 2. JSON-RPC 2.0 envelopes (NORMATIVE)

All messages are JSON-RPC 2.0. Every request/response carries
`"jsonrpc": "2.0"`. Requests carry an `id` (string or number); notifications
carry NO `id`. Responses echo the request `id`.

Base shapes (JSON-RPC 2.0):

```jsonc
// Request
{ "jsonrpc": "2.0", "id": <string|number>, "method": <string>, "params": <object?> }
// Success response
{ "jsonrpc": "2.0", "id": <string|number>, "result": <object> }
// Error response
{ "jsonrpc": "2.0", "id": <string|number|null>, "error": { "code": <int>, "message": <string>, "data": <any?> } }
// Notification (no id, no response)
{ "jsonrpc": "2.0", "method": <string>, "params": <object?> }
```

### 2.1 Lifecycle

1. Client → server: `initialize` request (§2.2).
2. Server → client: `initialize` result.
3. Client → server: `notifications/initialized` notification — the server MUST
   accept it and produce NO response. The server MUST reject `tools/list` /
   `tools/call` issued *before* a successful `initialize` with JSON-RPC
   `-32600` (Invalid Request); see §3.

### 2.2 `initialize`

**Request params:**

```jsonc
{
  "protocolVersion": "2025-06-18",
  "capabilities": { /* client capabilities; v1 server ignores all of them */ },
  "clientInfo": { "name": <string>, "version": <string> }
}
```

- `protocolVersion`: the server MUST compare against its single supported
  version string `"2025-06-18"`. On exact match it echoes it. On mismatch it
  still returns a result echoing **its own** supported `"2025-06-18"` (per MCP
  version-negotiation: the server replies with a version it supports; the client
  decides whether to proceed). v1 supports exactly one version.

**Result:**

```jsonc
{
  "protocolVersion": "2025-06-18",
  "capabilities": {
    "tools": {}            // tools capability advertised; NO "listChanged": true
  },
  "serverInfo": {
    "name": <string>,      // generated: the Baboon service name (or model-derived server name)
    "version": <string>    // generated: the Baboon model/domain version string
  }
}
```

- `capabilities.tools` is present (an empty object) to advertise that this
  server offers tools. `listChanged` is OMITTED (tool set is static — §0).
- `resources`, `prompts`, `completions`, `logging`, `sampling` keys are ABSENT
  (the explicit v1 exclusions).
- `serverInfo.name`/`version` are filled deterministically from the model at
  generation time; the exact source field is a generator detail but MUST be
  stable across regenerations.

### 2.3 `tools/list`

**Request params:** none required. (A `cursor` MAY appear but is IGNORED in v1;
the full set is returned in one response — no `nextCursor`.)

**Result:**

```jsonc
{
  "tools": [
    {
      "name": <string>,             // §1.2 tool name
      "description": <string?>,     // OPTIONAL: from the Baboon method's doc comment if present, else omitted
      "inputSchema": { /* JSON Schema, see below */ }
    }
    // ... one entry per method, deterministic order = model declaration order
  ]
}
```

- `tools[].name`: per §1.2.
- `tools[].description`: emitted only if the method carries a doc comment;
  otherwise the key is omitted (not `null`).
- `tools[].inputSchema`: a JSON Schema **object** (`{"type":"object", ...}`)
  derived from the method's **request DTO**. The schema emitter to use is fixed
  by a SEPARATE locked decision: `decisions:K3` resolves *build a dedicated
  shared MCP inputSchema emitter (NOT extend the OpenAPI emitter)*, and
  `decisions:K1` fixes the validity-coverage gate. This contract only fixes
  that `inputSchema` MUST
  be a well-formed JSON Schema `object` describing the request DTO's fields,
  and that all 9 backends emit a STRUCTURALLY IDENTICAL schema for the same DTO.
- **Ordering** of the `tools` array MUST be deterministic: model declaration
  order (service order, then method order within a service). Non-deterministic
  ordering would diverge across the 9 backends and break golden-file tests.

An MCP `outputSchema` is NOT emitted in v1 (the result is returned as
`content`; see §2.4). This is an explicit v1 simplification.

### 2.4 `tools/call`

**Request params:**

```jsonc
{
  "name": <string>,          // tool name, §1.2
  "arguments": { /* object: the JSON encoding of the Baboon method request DTO */ }
}
```

- `arguments` is the JSON object that the existing per-language JSON codec for
  the method's request DTO decodes. The server serializes `arguments` back to a
  JSON string and hands it to `JsonMuxer.Invoke(BaboonMethodId, data,
  codecCtx)` (the established dispatch). If `arguments` is absent, it is treated
  as `{}` (empty object) — methods whose request DTO has no required fields
  succeed; otherwise decoding fails (§3.3).

**Result (success):**

```jsonc
{
  "content": [
    { "type": "text", "text": <string> }   // the JSON-encoded Baboon method result DTO, as a text block
  ],
  "isError": false        // MAY be omitted when false; if present it is false on success
}
```

- v1 returns the method's result DTO as a single `text` content block whose
  `text` is the JSON string produced by the result DTO's JSON codec. (Using a
  `text` block carrying JSON is the broadly-supported MCP convention; a
  `structuredContent` field is NOT emitted in v1 — explicit simplification,
  consistent with not advertising `outputSchema` in §2.3.)

**Result (tool-level error):** see §3.3 — `isError: true` with a `text` content
block describing the domain error. This is a *successful JSON-RPC response*
whose `result.isError` is `true`, NOT a JSON-RPC `error` object.

---

## 3. Error mapping (NORMATIVE)

MCP distinguishes two error channels, and the split is the crux of this
contract. The Baboon `BaboonWiringError` taxonomy maps onto them as follows.

### 3.1 Channel A — JSON-RPC protocol errors (`error` object)

These are *protocol/transport-level* faults: the request itself is malformed,
unknown, or unusable. They are returned as a JSON-RPC `error` object (NOT as a
`tools/call` result). Reserved codes per JSON-RPC 2.0:

| Code | Name | Baboon condition that maps here |
|---|---|---|
| `-32700` | Parse error | Transport adapter could not parse the incoming bytes as JSON. (Raised by the adapter, before `handle`; documented here for completeness — `id` is `null`.) |
| `-32600` | Invalid Request | Not a valid JSON-RPC 2.0 request object (missing/invalid `jsonrpc`/`method`); OR a `tools/list`/`tools/call` arrived before a successful `initialize` (§2.1). |
| `-32601` | Method not found | The JSON-RPC `method` is not one of `initialize` / `tools/list` / `tools/call` (e.g. a client tries `resources/list` or `prompts/list` — the v1 exclusions). |
| `-32602` | Invalid params | Required params missing/ill-typed for a known method: `initialize` without `protocolVersion`; `tools/call` without `name`; **`tools/call` with a `name` that does not resolve to a known tool** (unknown-tool → `-32602`, see §3.2); `arguments` present but not a JSON object. |
| `-32603` | Internal error | An unexpected server-side fault not attributable to the request (e.g. `EncoderFailed` serializing a *successful* result; an unanticipated runtime exception in the dispatch shell). |

Mapping from `BaboonWiringError`:

- `NoMatchingService` / `NoMatchingMethod` at the JSON-RPC routing layer
  (unknown JSON-RPC `method`) → `-32601`.
- Unknown **tool** name in `tools/call` → `-32602` (§3.2).
- `EncoderFailed` while encoding a successful result → `-32603` (the call
  succeeded but the server cannot represent the answer — an internal fault).
- `DuplicateService` → cannot occur at runtime (caught at generation, §1.2
  rule 4).

### 3.2 Decision: unknown tool name → `-32602` (Invalid params), not `-32601`

The JSON-RPC `method` *was* found (`tools/call` is a real method); what is
invalid is its `params.name`. Therefore an unknown/unmapped tool name is an
**Invalid params** (`-32602`) protocol error, NOT Method-not-found (`-32601`).
This is deliberate and MUST be identical across all 9 backends. `-32601` is
reserved strictly for an unknown JSON-RPC `method`.

### 3.3 Channel B — tool execution errors (`tools/call` result, `isError: true`)

These are *the tool ran and failed in a domain-meaningful way* — the request was
well-formed, routed to a real tool, but the underlying Baboon method reported a
business/domain error or its arguments could not be decoded into the request
DTO. Per the MCP Tools spec, such errors are reported **inside the `tools/call`
result** so the LLM can see and react to them — NOT as a JSON-RPC `error`:

```jsonc
{
  "content": [ { "type": "text", "text": <human-readable error description> } ],
  "isError": true
}
```

Mapping from `BaboonWiringError`:

- `DecoderFailed` — `arguments` is a JSON object but does not decode into the
  method's request DTO (missing required field, type mismatch) → tool-level
  error `isError: true`. Rationale: the *protocol* call was valid (`tools/call`
  with a known tool and an object `arguments`); the *domain payload* was wrong.
  This is exactly the class of failure an LLM client is expected to observe and
  self-correct, so it belongs in Channel B. (Contrast: `arguments` not being a
  JSON object at all is a `-32602` protocol error per §3.1 — that is malformed
  params, not a bad domain payload.)
- `CallFailed(method, domainError)` — the Baboon method executed and returned a
  declared error (errors-mode service result) → tool-level error
  `isError: true`, with `text` carrying the JSON-encoded domain error (so the
  client can inspect it). The domain error is NOT flattened into a JSON-RPC
  `error.code`.

### 3.4 Summary table

| Situation | Channel | Wire form |
|---|---|---|
| Non-JSON bytes | A | `-32700`, `id: null` |
| Not a JSON-RPC 2.0 request; pre-`initialize` tools call | A | `-32600` |
| Unknown JSON-RPC method (e.g. `resources/list`) | A | `-32601` |
| `tools/call` missing `name` / `arguments` not an object | A | `-32602` |
| `tools/call` unknown tool name | A | `-32602` (§3.2) |
| Result encoding failure / unexpected server fault | A | `-32603` |
| Bad domain payload (`arguments` object won't decode to DTO) | B | result `isError: true` |
| Declared Baboon method error (`CallFailed`) | B | result `isError: true` |

---

## 4. Consistency requirements across the 9 backends

The following MUST be byte-/structure-identical across scala, cs, python, rust,
typescript, kotlin, java, dart, swift for the same input model:

1. `tool.name` strings (§1.2 — verbatim casing, `_` join).
2. The `tools/list` `tools[]` ordering (model declaration order, §2.3).
3. Each tool's `inputSchema` structure (§2.3; codec-rendering divergence is the
   risk closed by `decisions:K1`).
4. The error channel chosen (A vs B) and the exact code (§3) for every failure
   class.
5. `initialize` result `capabilities` shape: `{"tools": {}}`, no `listChanged`,
   no excluded capabilities.

Per-language *symbol* casing, formatting, and idioms inside the generated
handler bodies are free to follow each language's conventions, because they are
never observable on the wire.

---

## 5. Provenance / cross-references

- Goal: `goals:G1` (MCP server generators across all backends).
- Milestone: `decisions:M1` (Plan: MCP server generators).
- Related locked decisions: `decisions:K1` (inputSchema validity coverage),
  `decisions:K3` (dedicated MCP inputSchema emitter strategy).
- Grounding: `docs/research/mcp-generators-grounding.md` (T1).
- This contract: `decisions:K4`.
