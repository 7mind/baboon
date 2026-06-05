---
ledger: defects
counters:
  milestone: 0
  item: 7
archives: []
---

# defects

## M2

### D1 — open

- createdAt: 2026-06-04T19:45:09.373Z
- updatedAt: 2026-06-04T19:45:09.373Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: MCP inputSchema emitter drops ADT common fields
- description: "Filed by the T5 reviewer (round 0, out-of-scope, low severity). McpInputSchemaEmitter.adtSchema (~L209-212) emits only `oneOf` of branch $refs and ignores `Typedef.Adt.fields` (common fields hoisted across branches). A branch schema therefore omits the ADT's shared fields. OUT OF SCOPE for T5: the K6 stub model's ADT case (Shape) carries no common fields, so this is unexercised and untested by T5's acceptance. Triage context: surfaced reviewing tasks:T5 diff 484e1617..60b6a26e; belongs to a later task/decision that exercises ADT-with-common-fields, or a documented wontfix if v1 MCP tool requests never carry such ADTs."
- severity: low
- suggestedFix: Merge adt.fields into each branch DTO body, or emit them as an allOf alongside the branch oneOf, when adt.fields is non-empty.
- ledgerRefs: ["tasks:T5","goals:G1"]

### D2 — open

- createdAt: 2026-06-04T19:45:14.494Z
- updatedAt: 2026-06-04T19:45:14.494Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: "MCP inputSchema: contract in reachable closure would emit a dangling $defs ref"
- description: "Filed by the T5 reviewer (round 0, out-of-scope, low severity). McpInputSchemaEmitter.emitNamedType (~L166-183) returns List.empty for `Typedef.NonDataTypedef` (Service/Contract), but the reachable-closure walk would still enqueue and `localRef` a contract id if a DTO field referenced one, producing a `#/$defs/<Contract>` ref with no matching $defs entry (dangling). In practice Baboon contracts are structural mixins applied via +/^, not field-referenceable types, so this path is likely unreachable; the K6 stub has no contract and the inventory does not include it. OUT OF SCOPE for T5. Triage context: surfaced reviewing tasks:T5 diff 484e1617..60b6a26e."
- severity: low
- suggestedFix: If contracts can ever be referenced as field types, inline their structural fields rather than ref-ing them; otherwise add an explicit guard/assertion that no contract id enters the closure.
- ledgerRefs: ["tasks:T5","goals:G1"]

## M3

### D3 — open

- createdAt: 2026-06-04T20:46:54.147Z
- updatedAt: 2026-06-04T20:46:54.147Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: TypeScript generator emits nondeterministic import ordering (pre-existing)
- description: "Filed by the T8 reviewer (round 0, out-of-scope, low severity, PRE-EXISTING — not introduced by T8). The non-MCP TS generator produces run-to-run-varying import-statement order in generated files (observed in mcp/stub/mcp-tools/service.ts, .../submitcomposite/in.ts, .../Nested.ts) when codegen is repeated with identical inputs and the SAME flag value. Verified by running codegen on baboon-compiler/src/test/resources/baboon/mcp-stub-ok three times with --ts-generate-mcp-server=false: the import SET is constant but its ORDER differs between runs, indicating iteration over an unordered Set/HashMap in the TS import-collection path. TS-semantically inert (tsc passes) but breaks byte-reproducible codegen. Independent of MCP; surfaced while confirming T8 flag-off byte-identity. Triage note: candidate for a separate fix or wontfix; does NOT affect MCP feature correctness."
- severity: low
- suggestedFix: Sort the TS import collection deterministically (by module path then symbol name) before rendering in the TS translator's import emission path.
- ledgerRefs: ["tasks:T8","goals:G1"]

## M4

### D4 — open

- createdAt: 2026-06-04T22:41:06.312Z
- updatedAt: 2026-06-04T22:41:06.312Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: "Scala codegen: foreign type FFancyStr imported but no bare symbol emitted (pre-existing)"
- description: "Filed by the T12 reviewer (round 0, out-of-scope, PRE-EXISTING — reproduced by generating mcp-stub-ok with the regular Scala flags WITHOUT --scala-generate-mcp-server). Generated mcptools_submitcomposite.in.scala (package mcp.stub.mcptools.submitcomposite) imports `mcp.stub.{... FFancyStr ...}`, but the generated FFancyStr.scala only defines FFancyStr_JsonCodec/_KeyCodec/_UEBACodec objects — no bare `object FFancyStr` or `type FFancyStr`, so the import does not resolve. Independent of MCP; surfaced authoring the Scala MCP test overlay (worked around with a FFancyStrShim). Triage: candidate for separate fix or wontfix; does not affect MCP feature correctness."
- severity: medium
- suggestedFix: Scala generator should either not emit the bare FFancyStr import for foreign types mapped to a primitive, or emit a matching `object FFancyStr` companion.
- ledgerRefs: ["tasks:T12","goals:G1"]

### D5 — open

- createdAt: 2026-06-04T22:41:10.641Z
- updatedAt: 2026-06-04T22:41:10.641Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: "Scala codegen: Color_JsonCodec referenced but not imported in nested-package codec file (pre-existing)"
- description: "Filed by the T12 reviewer (round 0, out-of-scope, PRE-EXISTING — reproduced without the MCP flag). Generated mcptools_listcollections.in.scala declares `package mcp.stub.mcptools.listcollections` and imports only `mcp.stub.{Color, Color_UEBACodec}` (not Color_JsonCodec), yet references `Color_JsonCodec.instance` in the map-with-enum-key decoder; from the nested package the bare name does not resolve. Independent of MCP; worked around with a listcollections package object re-export. Triage: candidate for separate fix or wontfix; does not affect MCP feature correctness."
- severity: medium
- suggestedFix: Scala generator should add Color_JsonCodec to the import set (or fully-qualify the reference) when an enum is used as a map key inside a nested-package codec file.
- ledgerRefs: ["tasks:T12","goals:G1"]

### D6 — open

- createdAt: 2026-06-05T00:17:15.479Z
- updatedAt: 2026-06-05T00:17:15.479Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: "Python JSON codec mishandles non-string-keyed maps (map[Color,str] enum key) (pre-existing)"
- description: "Filed by the T18 reviewer (round 0, out-of-scope, PRE-EXISTING). The Python JSON codec does not round-trip the {key,value} entry-array shape the MCP/OAS inputSchema declares for a non-string-keyed map like map[Color,str] (listCollections.byColor); the T18 round-trip test passes `byColor: {}` to avoid the enum-key codec path (mirrors the Kotlin replica's `byColor:{}`). Independent of the MCP flag; not introduced by T18. Triage: align the Python JSON codec's non-string-keyed-map handling with the OAS/MCP entry-array representation, under a separate task."
- severity: low
- suggestedFix: Investigate the Python JSON codec's handling of non-string-keyed maps and align it with the OAS/MCP entry-array representation.
- ledgerRefs: ["tasks:T18","goals:G1"]

### D7 — open

- createdAt: 2026-06-05T00:17:22.593Z
- updatedAt: 2026-06-05T00:17:22.593Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: "Python codegen: self-referential/recursive pydantic models fail on py3.12 without `from __future__ import annotations` (pre-existing, latent)"
- description: "Surfaced during T18 (filed by orchestrator from the T18 review). A recursive Baboon DTO (e.g. the T22 stub's `Tree { left: opt[Tree], children: lst[Tree] }`) generates Python whose pydantic v2 model raises NameError on py3.12 because the forward self-reference isn't resolvable without `from __future__ import annotations` (PEP 563). Latent until the recursive Tree type was introduced (mcp-stub-ok). The proper fix is in the Python generator (PyBaboonTranslator.renderTree) emitting the future-import for recursive models — but doing so changes flag-off Python output, so it must land as an intentional, separately-scoped baseline change (NOT inside T18, whose acceptance requires flag-off byte-identity). T18 is being re-scoped to apply the future-import only in its test harness (overlay/test-gen action) so the generator stays untouched; this defect tracks the real generator fix."
- severity: medium
- suggestedFix: In a separate task, have the Python generator emit `from __future__ import annotations` (or otherwise resolve forward refs) for files containing recursive/self-referential types, and update the Python regular-adt baseline expectation accordingly.
- ledgerRefs: ["tasks:T18","goals:G1"]
