# MCP Round-Trip Scenario — Language-Neutral Specification (T7)

Status: locked scenario (ledger `decisions:K4`, `decisions:K6`, milestone M1, goal `goals:G1`).
Author: sonnet-4-6. Session: agent-a77ba78a93ede5ead.
Date: 2026-06-04.

This document defines the **canonical language-neutral round-trip scenario** that each
of the 9 per-language MCP server overlay tests (T9–T18 / Scala, C#, Python, Rust,
TypeScript, Kotlin, Java, Dart, Swift) MUST assert against the T22 stub model.

The scenario is derived from:

- **K4** (`docs/research/mcp-protocol-contract.md`) — the normative wire contract.
- **K6** + the fixture at
  `baboon-compiler/src/test/resources/baboon/mcp-stub-ok/mcp_stub.baboon` — the stub
  model: service `McpTools` with 5 methods.
- **K1** — inputSchema validity-coverage gate (full validator at T5/T9/T11,
  structural-parity check at T12–T18).
- **T5** emitter (`McpInputSchemaEmitter.scala`) + golden test
  (`McpInputSchemaEmissionTest.scala`) — the authoritative source for expected
  `inputSchema` values.

All JSON payloads below are the EXACT expected wire values. Per-language tests MUST
assert structural equality to these values, not just key presence (see §5).

---

## 0. Fixture summary

Model: `mcp.stub`, version `1.0.0`.  
Service: `McpTools` (package path `["mcp","stub"]`, top-level owner).  
5 methods in declaration order: `listCollections`, `submitComposite`, `processShape`,
`pagePoints`, `ping`.

Tool names (K4 §1.2 — `<serviceName>_<methodName>`, casing preserved verbatim):

| # | Method | Tool name |
|---|--------|-----------|
| 1 | `listCollections` | `McpTools_listCollections` |
| 2 | `submitComposite` | `McpTools_submitComposite` |
| 3 | `processShape` | `McpTools_processShape` |
| 4 | `pagePoints` | `McpTools_pagePoints` |
| 5 | `ping` | `McpTools_ping` |

These 5 names are byte-identical across all 9 language backends. The test MUST
assert all 5 names and no others, in this exact order.

---

## 1. Step 1 — `initialize`

### 1.1 Request

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "initialize",
  "params": {
    "protocolVersion": "2025-06-18",
    "capabilities": {},
    "clientInfo": { "name": "test-client", "version": "0.0.1" }
  }
}
```

The client sends `protocolVersion: "2025-06-18"` (the single version supported in v1).
`capabilities` and `clientInfo` are required per the MCP spec; their exact values are
test-supplied and irrelevant to the assertions below.

### 1.2 Expected result

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "protocolVersion": "2025-06-18",
    "capabilities": {
      "tools": {}
    },
    "serverInfo": {
      "name": "<server-name>",
      "version": "<model-version>"
    }
  }
}
```

Mandatory assertions (per K4 §2.2 and §4):

- `result.protocolVersion` is exactly `"2025-06-18"`.
- `result.capabilities` is an object containing exactly one key `"tools"` whose value
  is an empty object `{}`.  `"listChanged"` MUST be absent from `result.capabilities.tools`.
  `"resources"`, `"prompts"`, `"completions"`, `"logging"`, and `"sampling"` MUST be
  absent from `result.capabilities`.
- `result.serverInfo` is an object with string keys `"name"` and `"version"` (exact
  values are generator-specific; the test asserts both keys are non-empty strings).

Post-initialize: the test sends the required lifecycle notification before issuing
`tools/list` or `tools/call`:

```json
{ "jsonrpc": "2.0", "method": "notifications/initialized" }
```

This is a JSON-RPC notification (no `id`). The server MUST produce no response.

---

## 2. Step 2 — `tools/list`

### 2.1 Request

```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "tools/list"
}
```

No `params` are required; `cursor` is ignored if supplied (no pagination in v1).

### 2.2 Expected result structure

```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "result": {
    "tools": [
      { "name": "McpTools_listCollections", "inputSchema": { ... } },
      { "name": "McpTools_submitComposite",  "inputSchema": { ... } },
      { "name": "McpTools_processShape",     "inputSchema": { ... } },
      { "name": "McpTools_pagePoints",       "inputSchema": { ... } },
      { "name": "McpTools_ping",             "inputSchema": { ... } }
    ]
  }
}
```

Mandatory assertions:

- `result.tools` is an array of exactly 5 elements, in the order shown (model declaration
  order, per K4 §2.3).
- The 5 tool names are exactly as listed in §0. No `"description"` key appears for any
  tool (the stub model's methods carry no doc comments).
- No `"nextCursor"` key is present.
- Each tool's `"inputSchema"` is structurally equal to the reference schema defined in §2.3.

#### 2.3 Reference `inputSchema` values

The schemas below are the authoritative values produced by `McpInputSchemaEmitter` for
the T22 stub model. They are derived from the T5 emitter code and golden test
(`McpInputSchemaEmissionTest`). JSON Schema dialect is Draft 2020-12; `$schema` is
present on every root.

**Schema-name convention** (from `OasTypeTranslator.schemaName`): for `model mcp.stub`,
the package path is `["mcp","stub"]`. For a top-level type `Foo`, the schema name is
`mcp_stub_Foo`. For an ADT branch `Bar` inside ADT `Shape`, the schema name is
`mcp_stub_Shape_Bar` (owner path `["Shape"]` inserted between package and type name).

**Scalar type mapping** (from `OasTypeTranslator.scalarSchema`):

| Baboon scalar | JSON Schema fragment |
|---|---|
| `bit` | `{"type":"boolean"}` |
| `str` | `{"type":"string"}` |
| `i32` | `{"type":"integer","format":"int32"}` |
| `i64` | `{"type":"integer","format":"int64"}` |
| `u32` | `{"type":"integer","format":"int32","minimum":0}` |
| `f64` | `{"type":"number","format":"double"}` |

**Foreign type `FFancyStr`**: all 9 per-language bindings map to string types
(`System.String`, `java.lang.String`, `builtins.str`, `std::string::String`, `string`,
`dart.core.String`, `Swift.String`). The emitter resolves this to `{"type":"string"}`.
`FFancyStr` does NOT appear in `$defs`.

---

##### Tool 1: `McpTools_ping`

Baseline scalar-only tool. No `$defs` (no referenced named types).

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "type": "object",
  "properties": {
    "seqno": { "type": "integer", "format": "int32" },
    "label": { "type": "string" }
  },
  "required": ["seqno", "label"]
}
```

Validation:
- A conforming instance: `{"seqno": 7, "label": "hi"}` — MUST be accepted.
- A non-conforming instance: `{"seqno": 7}` (missing required `"label"`) — MUST be
  rejected (negative control proving liveness).

---

##### Tool 2: `McpTools_submitComposite`

Nested DTO + `opt[DTO]` + enum + foreign-string. `$defs` closure:
`mcp_stub_Color`, `mcp_stub_Nested`, `mcp_stub_Point`.

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "type": "object",
  "properties": {
    "nested":     { "$ref": "#/$defs/mcp_stub_Nested" },
    "maybePoint": { "oneOf": [{ "$ref": "#/$defs/mcp_stub_Point" }, { "type": "null" }] },
    "color":      { "$ref": "#/$defs/mcp_stub_Color" },
    "fancy":      { "type": "string" }
  },
  "required": ["nested", "color", "fancy"],
  "$defs": {
    "mcp_stub_Color": {
      "type": "string",
      "enum": ["Red", "Green", "Blue"]
    },
    "mcp_stub_Nested": {
      "type": "object",
      "properties": {
        "point": { "$ref": "#/$defs/mcp_stub_Point" },
        "color": { "$ref": "#/$defs/mcp_stub_Color" },
        "label": { "oneOf": [{ "type": "string" }, { "type": "null" }] }
      },
      "required": ["point", "color"]
    },
    "mcp_stub_Point": {
      "type": "object",
      "properties": {
        "x": { "type": "integer", "format": "int32" },
        "y": { "type": "integer", "format": "int32" }
      },
      "required": ["x", "y"]
    }
  }
}
```

Notes:
- `"maybePoint"` (`opt[Point]`) is NOT in `required` — optional field, absent from the
  required array.
- `"fancy"` (`FFancyStr`) resolves to `{"type":"string"}` inline; NOT a `$defs` entry.
- The `$defs` ordering in the emitted object is sorted by `TypeId.toString` (lexicographic):
  `mcp_stub_Color` < `mcp_stub_Nested` < `mcp_stub_Point`.

Validation:
- Conforming instance:
  ```json
  {
    "nested": { "point": {"x": 1, "y": 2}, "color": "Red" },
    "maybePoint": null,
    "color": "Blue",
    "fancy": "anything"
  }
  ```
  MUST be accepted.
- Non-conforming instance (invalid enum value): same as above with `"color": "Purple"` —
  MUST be rejected (proves enum constraint is live).

---

##### Tool 3: `McpTools_listCollections`

List / set / map[str] / map[enum-key]. `$defs` closure: `mcp_stub_Color`.

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "type": "object",
  "properties": {
    "tags":      { "type": "array", "items": { "type": "string" } },
    "uniqueIds": { "type": "array", "items": { "type": "integer", "format": "int64" }, "uniqueItems": true },
    "labels":    { "type": "object", "additionalProperties": { "type": "string" } },
    "byColor":   {
      "type": "array",
      "items": {
        "type": "object",
        "required": ["key", "value"],
        "properties": {
          "key":   { "$ref": "#/$defs/mcp_stub_Color" },
          "value": { "type": "string" }
        }
      }
    }
  },
  "required": ["tags", "uniqueIds", "labels", "byColor"],
  "$defs": {
    "mcp_stub_Color": {
      "type": "string",
      "enum": ["Red", "Green", "Blue"]
    }
  }
}
```

Notes:
- `uniqueIds` (`set[i64]`) → array + `"uniqueItems": true`; element type `i64` →
  `{"type":"integer","format":"int64"}`.
- `byColor` (`map[Color, str]` — non-string key) → array of `{key, value}` entry
  objects; the `key` schema is a local `$ref` to `mcp_stub_Color`.
- `labels` (`map[str, str]` — string key) → object + `additionalProperties`.

Validation:
- Conforming instance:
  ```json
  {
    "tags": ["a", "b"],
    "uniqueIds": [1, 2],
    "labels": { "k": "v" },
    "byColor": [{ "key": "Green", "value": "ok" }]
  }
  ```
  MUST be accepted.

---

##### Tool 4: `McpTools_processShape`

ADT `oneOf` + recursive `Tree`. `$defs` closure: `mcp_stub_Shape`,
`mcp_stub_Shape_Circle`, `mcp_stub_Shape_Rect`, `mcp_stub_Tree`.

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "type": "object",
  "properties": {
    "shape": { "$ref": "#/$defs/mcp_stub_Shape" },
    "tree":  { "$ref": "#/$defs/mcp_stub_Tree" }
  },
  "required": ["shape", "tree"],
  "$defs": {
    "mcp_stub_Shape": {
      "oneOf": [
        { "$ref": "#/$defs/mcp_stub_Shape_Circle" },
        { "$ref": "#/$defs/mcp_stub_Shape_Rect" }
      ]
    },
    "mcp_stub_Shape_Circle": {
      "type": "object",
      "properties": {
        "radius": { "type": "number", "format": "double" }
      },
      "required": ["radius"]
    },
    "mcp_stub_Shape_Rect": {
      "type": "object",
      "properties": {
        "w": { "type": "number", "format": "double" },
        "h": { "type": "number", "format": "double" }
      },
      "required": ["w", "h"]
    },
    "mcp_stub_Tree": {
      "type": "object",
      "properties": {
        "value":    { "type": "integer", "format": "int32" },
        "left":     { "oneOf": [{ "$ref": "#/$defs/mcp_stub_Tree" }, { "type": "null" }] },
        "children": { "type": "array", "items": { "$ref": "#/$defs/mcp_stub_Tree" } }
      },
      "required": ["value", "children"]
    }
  }
}
```

Notes:
- `mcp_stub_Shape` has `oneOf` of its two branch `$ref`s. ADT branches are emitted as
  separate `$defs` entries with schema names `mcp_stub_Shape_Circle` and
  `mcp_stub_Shape_Rect`.
- `mcp_stub_Tree` is recursive: `left` (opt[Tree]) and `children` (lst[Tree]) both
  self-ref `#/$defs/mcp_stub_Tree`. This terminates without infinite expansion because
  the entry already exists in `$defs`.
- `left` is `opt[Tree]` → NOT in `required`.
- `$defs` ordering (by `TypeId.toString` lexicographic sort): for types in `mcp.stub`,
  top-level owner `/:` renders as `mcp.stub/:#<Name>` (colon-prefixed) while ADT-owned
  types render as `mcp.stub/[mcp.stub/:#Shape]#<Name>` (bracket-prefixed). ASCII `:` (58)
  < `[` (91), so all top-level types sort before ADT-branch types:
  1. `mcp_stub_Shape` (`mcp.stub/:#Shape`)
  2. `mcp_stub_Tree` (`mcp.stub/:#Tree`)
  3. `mcp_stub_Shape_Circle` (`mcp.stub/[mcp.stub/:#Shape]#Circle`)
  4. `mcp_stub_Shape_Rect` (`mcp.stub/[mcp.stub/:#Shape]#Rect`)
  Tests MUST compare `$defs` by key lookup (§5.4), so this ordering is informational.

Validation:
- Conforming instance (Circle branch + shallow recursive tree):
  ```json
  {
    "shape": { "radius": 1.5 },
    "tree": { "value": 0, "children": [{ "value": 1, "children": [] }] }
  }
  ```
  MUST be accepted.

---

##### Tool 5: `McpTools_pagePoints`

Template-instantiation alias `PointPage = Page[Point]`. The emitter materialises
the alias as a concrete DTO (`items: lst[Point]`, `total: u32`). `$defs` closure:
`mcp_stub_Point`, `mcp_stub_PointPage`.

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "type": "object",
  "properties": {
    "page": { "$ref": "#/$defs/mcp_stub_PointPage" }
  },
  "required": ["page"],
  "$defs": {
    "mcp_stub_Point": {
      "type": "object",
      "properties": {
        "x": { "type": "integer", "format": "int32" },
        "y": { "type": "integer", "format": "int32" }
      },
      "required": ["x", "y"]
    },
    "mcp_stub_PointPage": {
      "type": "object",
      "properties": {
        "items": { "type": "array", "items": { "$ref": "#/$defs/mcp_stub_Point" } },
        "total": { "type": "integer", "format": "int32", "minimum": 0 }
      },
      "required": ["items", "total"]
    }
  }
}
```

Notes:
- `PointPage` is the materialised alias for `Page[Point]`. It appears in `$defs` as
  `mcp_stub_PointPage` (the alias name, not `mcp_stub_Page`).
- `total` (`u32`) → `{"type":"integer","format":"int32","minimum":0}`.
- `items` element type is `mcp_stub_Point` via local `$ref`.
- `$defs` ordering: `mcp_stub_Point` < `mcp_stub_PointPage` (lexicographic on
  `TypeId.toString`).

Validation:
- Conforming instance:
  ```json
  { "page": { "items": [{ "x": 1, "y": 2 }], "total": 1 } }
  ```
  MUST be accepted.

---

## 3. Step 3 — `tools/call` (successful paths)

### 3.1 Simple call: `McpTools_ping`

**Request:**

```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "tools/call",
  "params": {
    "name": "McpTools_ping",
    "arguments": { "seqno": 42, "label": "hello" }
  }
}
```

**Expected result (success):**

```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "result": {
    "content": [
      { "type": "text", "text": "{\"ok\":true}" }
    ],
    "isError": false
  }
}
```

Assertions:
- `result.content` is an array of exactly one element.
- `result.content[0].type` is `"text"`.
- `result.content[0].text` is the JSON encoding of the `ping` response DTO
  (`data out { ok: bit }`). The expected payload is `{"ok":true}` (the stub
  handler returns `ok = true` by convention; see note below).
- `result.isError` is `false` (or absent — the field MAY be omitted when false per K4
  §2.4; tests MUST accept both forms).

> **Stub-handler convention**: the per-language overlay tests wire a stub handler for
> each method that returns the success DTO with `ok = true`. The exact JSON encoding of
> `bit` (boolean true) varies only in key: all 9 backends emit `{"ok":true}` because
> the JSON codec renders `bit` as a JSON boolean. Tests MUST assert `"ok"` key with
> value `true`.

### 3.2 Composite call: `McpTools_submitComposite`

**Request:**

```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "method": "tools/call",
  "params": {
    "name": "McpTools_submitComposite",
    "arguments": {
      "nested": {
        "point": { "x": 10, "y": 20 },
        "color": "Red"
      },
      "maybePoint": null,
      "color": "Blue",
      "fancy": "test-value"
    }
  }
}
```

**Expected result (success):**

```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "result": {
    "content": [
      { "type": "text", "text": "{\"ok\":true}" }
    ],
    "isError": false
  }
}
```

Assertions:
- `result.content[0].type` is `"text"`.
- `result.content[0].text` is the JSON encoding of the `submitComposite` response DTO
  (`data out { ok: bit }`): `{"ok":true}`.
- `result.isError` is `false` (or absent).

This call exercises: nested DTO argument decoding (Nested → Point + Color),
`opt[Point]` as JSON null, enum value, and foreign-string field.

---

## 4. Step 4 — `tools/call` (error paths)

### 4.1 Channel A error: unknown tool name → `-32602`

**Request:**

```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "method": "tools/call",
  "params": {
    "name": "McpTools_nonexistent",
    "arguments": {}
  }
}
```

**Expected error response (K4 §3.2 — unknown tool name is `-32602`, NOT `-32601`):**

```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "error": {
    "code": -32602,
    "message": "<any non-empty string>"
  }
}
```

Assertions:
- The response is a JSON-RPC `error` object (Channel A), NOT a `tools/call` result.
- `error.code` is exactly `-32602`.
- `error.message` is a non-empty string (exact text is implementation-defined).
- No `"result"` key is present.

This is the **negative control** proving that the dispatch table is live and that
unknown tool names are rejected with the correct error code.

### 4.2 Channel B error: `DecoderFailed` — malformed domain payload → `isError: true`

**Request (missing required field `"seqno"`):**

```json
{
  "jsonrpc": "2.0",
  "id": 6,
  "method": "tools/call",
  "params": {
    "name": "McpTools_ping",
    "arguments": { "label": "missing-seqno" }
  }
}
```

**Expected result (Channel B — tool-level error, K4 §3.3):**

```json
{
  "jsonrpc": "2.0",
  "id": 6,
  "result": {
    "content": [
      { "type": "text", "text": "<human-readable decoder error>" }
    ],
    "isError": true
  }
}
```

Assertions:
- The response is a JSON-RPC **success** response (has `"result"`, not `"error"`).
  This is Channel B: the protocol call was valid; the domain payload was wrong.
- `result.isError` is `true`.
- `result.content` is an array with at least one element of `{"type":"text", ...}`.
- `result.content[0].text` is a non-empty string (exact text is implementation-defined).

Rationale: `arguments` IS a JSON object (satisfying the Channel A `-32602` guard), so
the fault is a domain-payload decode failure. This class of failure is expected to be
observable and self-correctable by an LLM client, hence Channel B.

---

## 5. Assertion discipline (applies to all 9 per-language tests)

### 5.1 Unconditional asserts — MANDATORY

Assertions MUST be unconditional throws/failures, not vacuous no-ops. Several languages
default to no-op assert behaviour:

- **C#**: `Debug.Assert` is elided in release builds; use `throw` or a test-framework
  assertion (e.g. `Assert.AreEqual`, `Assert.Throws`) that throws on failure in all
  build configurations.
- **Dart**: `assert(...)` is elided outside debug mode; use `if (!cond) throw ...` or
  the `test`-package `expect(...)` matcher.
- **Swift**: `assert(...)` is elided in release; use `XCTAssert` or a throw.
- **JVM (Kotlin/Java)**: `assert` is disabled unless `-ea` is passed to the JVM; use
  the test framework's assertion methods (`assertEquals`, `assertTrue`, etc.).
- **Scala, Rust, TypeScript, Python**: test framework assertions are unconditional by
  default; use them (ScalaTest `assert`/`assertResult`, Rust `assert_eq!`, Jest
  `expect(...).toBe(...)`, `pytest` `assert`).

### 5.2 Liveness — negative control REQUIRED

Each per-language test MUST include AT LEAST ONE assertion that can fail and DOES fail
when the dispatch table is empty or the tool names are wrong. The unknown-tool error
test (§4.1) serves as this negative control:
- If the server returned ANY success for `McpTools_nonexistent`, the test MUST fail.
- If `error.code` is wrong (e.g. `-32601` instead of `-32602`), the test MUST fail.

A test that only checks positive paths cannot prove the server is actually dispatching.

### 5.3 inputSchema validation tiers (K1)

**T9 (TypeScript/AJV) and T11 (C#/NJsonSchema)**: each returned `inputSchema` MUST be
run through a real Draft 2020-12 JSON Schema validator (AJV / NJsonSchema /
JsonSchema.Net). Every schema in the `tools` array must validate as well-formed (compile
without errors, with all `$ref`s resolved). Additionally validate the conforming
instance from §2.3 against each tool's schema.

**T12–T18 (Scala, Python, Rust, Kotlin, Java, Dart, Swift replicas)**: each returned
`inputSchema` MUST be asserted **structurally equal** to the corresponding reference
value from §2.3 of this document (after parsing from the wire). This closes the
codec-rendering divergence gap (K1) without requiring a per-language JSON-Schema
validator. Structural equality means key-by-key recursive comparison of the parsed
JSON object, not string comparison (key ordering may vary in the serialised form).

**All 9 backends**: the `tools` array length MUST be exactly 5, the tool names MUST
match §0 exactly, and the `$schema` field of each `inputSchema` root MUST be
`"https://json-schema.org/draft/2020-12/schema"`.

### 5.4 Order-sensitive assertions

- The `tools` array MUST be in model declaration order (§0). Tests MUST assert position
  0 = `McpTools_listCollections`, position 4 = `McpTools_ping`, not just set membership.
- The `required` arrays in inputSchema objects may be in any order at the wire level
  (JSON arrays are ordered but the emitter's insertion order is an implementation
  detail). Tests MUST compare required sets as sets, not as ordered arrays.
- The `$defs` object key order is an implementation detail; tests MUST compare `$defs`
  by key lookup, not by key order.

---

## 6. Sequence summary

```
Client                       Server (generated McpTools MCP handler)
  │                              │
  │── initialize ───────────────>│  (§1.1)
  │<─ {protocolVersion, capab., serverInfo} (§1.2)
  │                              │
  │── notifications/initialized ─>│  (no response)
  │                              │
  │── tools/list ───────────────>│  (§2.1)
  │<─ {tools: [5 entries]} ──────│  (§2.2, §2.3)
  │                              │
  │── tools/call ping ──────────>│  (§3.1)
  │<─ {content:[text{ok:true}]} ─│
  │                              │
  │── tools/call submitComposite>│  (§3.2)
  │<─ {content:[text{ok:true}]} ─│
  │                              │
  │── tools/call nonexistent ───>│  (§4.1)
  │<─ {error:{code:-32602,...}} ─│
  │                              │
  │── tools/call ping bad args ──>│  (§4.2)
  │<─ {result:{isError:true,...}}│
```

---

## 7. Cross-references

- Wire contract (normative): `decisions:K4` / `docs/research/mcp-protocol-contract.md`
- Stub model (T22): `decisions:K6` /
  `baboon-compiler/src/test/resources/baboon/mcp-stub-ok/mcp_stub.baboon`
- inputSchema emitter (T5): `McpInputSchemaEmitter.scala`, `OasTypeTranslator.scala`
- T5 golden test (authoritative inputSchema source):
  `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/McpInputSchemaEmissionTest.scala`
- inputSchema validity coverage decision: `decisions:K1`
- Goal: `goals:G1` (MCP server generators across all backends)
- Per-language overlay tasks: T9 (TypeScript), T11 (C#), T12 (Scala), T13 (Rust),
  T14 (Kotlin), T15 (Java), T16 (Dart), T17 (Swift), T18 (Python)
  (T8 = reference TypeScript *generator* task; T10 = reference C# *generator* task —
  neither is a per-language overlay task)
