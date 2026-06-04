# MCP tool `inputSchema` — gap analysis of the existing OpenAPI JSON-Schema emitter

Task T4 (goal G1). Reproduce-then-decide. All emitter output below is **actual
captured output**, not asserted. Generated with the worktree's prebuilt native
binary:

```
Baboon 0.0.188-SNAPSHOT @ main#e61a5068992d1ecd58079de2fda648e12bd53c55
```

(the OpenAPI emitter is byte-identical between that commit and the T4 base
`8ada6961` — no `translator/openapi/` change in between.)

## 1. What "MCP tool inputSchema" requires (the bar to clear)

Per goal G1 / Q4: each Baboon **service method** becomes one MCP tool. At
`tools/list` the server advertises, for every tool, an `inputSchema` field whose
value is a **single, self-contained JSON Schema object** describing that tool's
request arguments (derived from the method's request DTO). The MCP client feeds
that schema straight to a JSON-Schema validator / form generator. There is **no
shared component registry** in the MCP wire protocol — each `inputSchema` stands
alone.

Operational acceptance test for "MCP-usable" below: *a single tool's
`inputSchema`, extracted in isolation, validates a conforming argument object
under a stock Draft 2020-12 validator without any external `$ref` resolution
context.*

## 2. Reproduction

Model: `/tmp/t4-repro/model/mcp_repro.baboon` (committed inline in §5 below for
reproducibility). It exercises DTO, enum, ADT, recursive DTO, nested DTO,
collections (list/set/string-map/non-string-map), option, foreign type, type
alias (template instantiation), the full scalar set, the `any`-opaque envelope,
and — critically — a `root service Tools` whose four methods (`collections`,
`composite`, `shapes`, `ping`) carry `data in` blocks that are exactly the MCP
tool request types.

Command (mirrors `.mdl/defs/tests.md` `test-gen-openapi`):

```bash
baboon --model-dir /tmp/t4-repro/model/ :openapi \
  --output /tmp/t4-repro/out --disable-conversions=true --runtime=without
```

Output: one file `mcp_repro/v1_0_0/openapi.json`, well-formed JSON
(`python3 -m json.tool` exit 0).

## 3. Gap table — construct → emitter behaviour

"Schema body" = the JSON-Schema fragment the emitter produces for the type
itself (under `components/schemas`). "MCP-usable as a standalone inputSchema" =
whether that fragment, lifted out as a tool's top-level `inputSchema`, is
self-contained and valid with no external `$ref` context.

| # | Baboon construct | Emitter renders schema body? | MCP-usable standalone? | Note |
|---|---|---|---|---|
| 1 | DTO (object, required/optional fields) | YES — correct `object`/`properties`/`required` | **NO (only as a leaf)** | A request DTO whose fields are all scalars *is* self-contained. The moment a field references another named type the body emits a `$ref` (rows 3,4,5,8,9,11). |
| 2 | Scalars (bit/str/i*/u*/f32/f64/f128/uid/tsu/tso/bytes) | YES — correct type+format, `minimum:0` for unsigned | YES | `f128`→string/decimal and `tsu`/`tso`→date-time collapse are lossy but valid. |
| 3 | Enum | YES — `string`+`enum` | YES standalone; **NO as a field** | As a *field* it is referenced by `$ref` (see row 8/9). |
| 4 | Nested DTO field | field emitted as `{"$ref":"#/components/schemas/..."}` | **NO** | Bare `$ref` into `#/components/schemas` — unresolvable in a standalone inputSchema. This is the dominant gap. |
| 5 | ADT | `oneOf:[{$ref branch}...]`, branch bodies emitted as **sibling** top-level entries | **NO** | Both the `oneOf` `$ref`s AND the branch schemas live as separate `components/schemas` entries; none are inlined into the ADT's own object. |
| 6 | Recursive / self-referential DTO (`opt[Tree]`, `lst[Tree]`) | YES structurally, via `$ref` back to itself | **NO without a `$defs` mechanism** | A standalone inputSchema for a recursive type *requires* either a self-`$ref` to the document root (`#`) or a local `$defs` map. The emitter only produces `#/components/schemas/...` cross-refs, which do not exist in a standalone schema. |
| 7 | Collections — `lst` / `set` / `map[str,V]` / `map[K,V]` non-string key | YES — array / array+uniqueItems / object+additionalProperties / array-of-entry-objects | partial | The *collection shape* is correct and inline; but `V`/`K` element schemas use `$ref` whenever they are named types (same root-4 gap). |
| 8 | Option (`opt[T]`) | YES — `oneOf:[T,{type:null}]` | partial | Correct nullable shape; inner `T` is a `$ref` when named. |
| 9 | Foreign type WITH language mappings but no `rt` (Baboon→Baboon) | **opaque** `{"type":"object","description":"Foreign type: ..."}` | YES (valid) but **semantically empty** | `FFancy` maps to `string` in every target language, yet emits as an opaque object. The emitter only resolves a foreign type when it has a `runtimeMapping` to *another Baboon type* (`rt`), never from the per-language type strings. For MCP, an opaque `object` is a near-useless inputSchema fragment. |
| 10 | Type alias / template instantiation (`Page[Point]`) | YES — typer materialises before codegen; emits as a normal DTO | partial | Same `$ref` gap for the element type. |
| 11 | `any`-opaque envelope (`any`, `any[T]`, …) | YES — inline `BaboonAny` object schema with `$ak/$ad/$av/$at/$c` | YES (self-contained) | The single construct that is fully inlined and standalone-valid. |
| 12 | Service / Contract types | **skipped** (`NonDataTypedef`) | n/a | BUT the method `data in`/`data out` blocks are reified by the typer as standalone DTOs (`<Svc>_<method>_in`/`_out`) and ARE emitted — see §4. The service node itself is skipped; its method I/O survives as DTOs. |

## 4. The decisive finding: service method request DTOs ARE emitted, but only as `$ref`-linked library entries

The emitter doc-comment says services are "skipped". They are — as *nodes*. But
the typer reifies each method's inline `data in` block into a first-class DTO,
so the OpenAPI document already contains exactly the per-tool request schemas
MCP needs, named `<Pkg>_<Service>_<method>_in`:

Captured (verbatim) for the `shapes` tool's request:

```json
"mcp_repro_Tools_shapes_in": {
    "type": "object",
    "required": ["shape", "tree"],
    "properties": {
        "shape": { "$ref": "#/components/schemas/mcp_repro_Shape" },
        "tree":  { "$ref": "#/components/schemas/mcp_repro_Tree" }
    }
}
```

This is **structurally correct** but **not MCP-usable as-is**: the two
properties are bare `$ref`s into `#/components/schemas`. An MCP client receiving
this object as `inputSchema` has no `components/schemas` registry to resolve
`mcp_repro_Shape` or `mcp_repro_Tree` against. The schema is inert.

Compare the empty request, which IS standalone-usable:

```json
"mcp_repro_Tools_ping_in": { "type": "object" }
```

And the all-scalar / collection-shape fields case (still has one `$ref` for the
enum key):

```json
"mcp_repro_Tools_collections_in": {
    "type": "object",
    "required": ["tags", "uniqueIds", "labels", "byColor"],
    "properties": {
        "tags":      { "type": "array", "items": { "type": "string" } },
        "uniqueIds": { "type": "array", "items": { "type": "integer", "format": "int64" }, "uniqueItems": true },
        "labels":    { "type": "object", "additionalProperties": { "type": "string" } },
        "byColor":   { "type": "array", "items": { "type": "object",
                         "required": ["key", "value"],
                         "properties": { "key": { "$ref": "#/components/schemas/mcp_repro_Color" },
                                         "value": { "type": "string" } } } }
    }
}
```

Note even here `byColor`'s entry `key` is a `$ref` to the `Color` enum — so the
collections tool is also not fully standalone.

ADT branch emission, captured (branches are siblings, not inlined):

```json
"mcp_repro_Shape_Circle": { "type": "object", "required": ["radius"], "properties": { "radius": { "type": "number", "format": "double" } } },
"mcp_repro_Shape_Rect":   { "type": "object", "required": ["w", "h"], "properties": { "w": {"type":"number","format":"double"}, "h": {"type":"number","format":"double"} } },
"mcp_repro_Shape":        { "oneOf": [ {"$ref":"#/components/schemas/mcp_repro_Shape_Circle"}, {"$ref":"#/components/schemas/mcp_repro_Shape_Rect"} ] }
```

Recursive type, captured (self-`$ref` into `components/schemas`):

```json
"mcp_repro_Tree": {
    "type": "object", "required": ["value", "children"],
    "properties": {
        "value":    { "type": "integer", "format": "int32" },
        "left":     { "oneOf": [ {"$ref":"#/components/schemas/mcp_repro_Tree"}, {"type":"null"} ] },
        "children": { "type": "array", "items": {"$ref":"#/components/schemas/mcp_repro_Tree"} }
    }
}
```

Foreign type, captured (opaque despite having `string` mappings in 9 languages):

```json
"mcp_repro_FFancy": { "type": "object", "description": "Foreign type: mcp_repro_FFancy" }
```

`any`-opaque, captured (the one fully-inline, standalone-valid construct):

```json
"payload": {
    "type": "object", "title": "BaboonAny",
    "description": "Opaque any-envelope. ...",
    "properties": { "$ak": {"type":"integer","minimum":0,"maximum":7},
                    "$ad": {"type":"string"}, "$av": {"type":"string"},
                    "$at": {"type":"string"}, "$c": {} },
    "required": ["$ak", "$c"]
}
```

## 5. Root-cause summary of the gaps

The OpenAPI emitter is built for the OpenAPI **document model**: one document, a
shared `components/schemas` registry, and `$ref`s that resolve *within that
document*. Every gap above reduces to **one architectural mismatch with MCP**:

1. **`$ref` resolution scope.** The emitter emits `#/components/schemas/<Name>`
   cross-document refs. MCP `inputSchema` is a *single standalone schema* with no
   such registry. Every request DTO that transitively references any named type
   (nested DTO, enum, ADT, collection element, recursive self-ref, alias target)
   produces a dangling `$ref`. From the repro, 3 of 4 tool request schemas
   (`composite_in`, `shapes_in`, `collections_in`) are affected; only `ping_in`
   is clean.

To make these self-contained, an MCP emitter must either **inline** all
transitively-referenced schemas into each tool's `inputSchema`, or rewrite refs
to a **local `$defs`** block rooted at that schema (`#/$defs/<Name>`) and bundle
the reachable closure. The OpenAPI emitter does neither and has no concept of
either.

Secondary, lower-severity gaps (all valid JSON but lossy/empty for MCP):

2. **Foreign types ignore language mappings** — emit opaque `object` even when a
   precise scalar mapping exists (row 9).
3. **No per-tool wrapper** — the emitter has no notion of "tool" at all; it emits
   a flat type library. The service→tool mapping, tool naming, and the
   `inputSchema` extraction live entirely outside the current emitter.
4. **Lossy scalar collapses** (`f128`→string, `tsu`/`tso`→date-time) — acceptable
   for v1 but recorded.

## 6. Reproduction model (verbatim)

`/tmp/t4-repro/model/mcp_repro.baboon`:

```
model mcp.repro

version "1.0.0"

foreign FFancy {
  cs = "System.String"
  scala = "java.lang.String"
  kotlin = "java.lang.String"
  java = "java.lang.String"
  py = "builtins.str"
  rust = "std::string::String"
  typescript = "string"
  dart = "dart.core.String"
  swift = "Swift.String"
}

enum Color { Red  Green  Blue }

data Point { x: i32  y: i32 }

data Tree { value: i32  left: opt[Tree]  children: lst[Tree] }

data Nested { point: Point  color: Color }

adt Shape {
  data Circle { radius: f64 }
  data Rect   { w: f64
                h: f64 }
}

data Page[T] { items: lst[T]  total: u32 }
root type PointPage = Page[Point]

root data AllScalars {
  b: bit  s: str  i8v: i08  i16v: i16  i32v: i32  i64v: i64
  u8v: u08  u64v: u64  f32v: f32  f64v: f64  f128v: f128
  uidv: uid  tsuv: tsu  tsov: tso  bytesv: bytes
}

root service Tools {
  def collections ( data in { tags: lst[str]  uniqueIds: set[i64]
                              labels: map[str, str]  byColor: map[Color, str] }
                    data out { ok: bit } )
  def composite   ( data in { maybePoint: opt[Point]  nested: Nested
                              color: Color  fancy: FFancy }
                    data out { ok: bit } )
  def shapes      ( data in { shape: Shape  tree: Tree }
                    data out { ok: bit } )
  def ping        ( data in { }  data out { ok: bit } )
}

root data WithAny { payload: any }
```

Note: `map[Point, str]` was rejected by the typer ("multi-field DTO key
ineligible") — only single-primitive wrappers, `id` types, enums, and foreign
types are legal map keys. The non-string-key map branch is therefore exercised
with `map[Color, str]` (enum key).

## 7. Decision input for T5

T5 implements "the shared, language-agnostic inputSchema emitter". The evidence
above shows the OpenAPI emitter's *scalar/collection/object fragment generation*
is correct and reusable, but its *document assembly model* (`$ref` into a shared
registry, flat type library, service-node skip, no per-tool wrapper) is
fundamentally incompatible with MCP's standalone-per-tool `inputSchema`
requirement. See the locked decision item (decisions ledger, milestone M1) for
the chosen strategy and rationale.
