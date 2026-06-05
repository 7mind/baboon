---
ledger: decisions
counters:
  milestone: 0
  item: 7
archives: []
---

# decisions

## M1

### K1 — locked

- createdAt: 2026-06-04T18:16:04.864Z
- updatedAt: 2026-06-04T18:16:04.864Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: "MCP inputSchema validity coverage: full validator at T5+T9+T11, structural-parity check at T12-T18"
- rationale: "Resolves R2's sole criticism (validity-gate asymmetry across the 7 replication backends). Decision: do NOT add a full real-JSON-Schema-validator dependency to the 7 replica test toolchains (T12-T18). Rationale grounded in the repo's per-language validator availability and harness cost: (1) the shared MCP inputSchema is emitted ONCE by the shared JVM emitter (T5), which is validator-backed (real JSON-Schema validator, mirroring test-openapi/swagger-parser and test-graphql/graphql-js). The schema OBJECT well-formedness is therefore already proven at the source. (2) The two reference round-trips T9 (ts/ajv) and T11 (cs/NJsonSchema|JsonSchema.Net) additionally validate the schema AFTER serialization through a real per-language JSON codec, in the two ecosystems that have first-class, stable JSON-Schema validators. (3) The residual per-language failure mode the reviewer correctly identifies is JSON-CODEC RENDERING DIVERGENCE (key escaping, number/$ref/additionalProperties/nested-map rendering) when each replica serializes the shared-emitter schema through its OWN codec. (4) Acquiring a production-grade JSON-Schema validator across all 7 heterogeneous replica toolchains (Circe/Scala, serde/Rust, Jackson/Kotlin+Java, dart:convert/Dart, JSONSerialization/Swift, custom/Python) is disproportionate new test-dependency surface, and Swift/Dart in particular lack a first-class stable JSON-Schema validator -- importing one is a maintenance liability out of scale with v1. THEREFORE the 7 replicas (T12-T18) close the codec-divergence gap WITHOUT a schema validator: at tools/list each replica round-trip MUST (a) assert each returned tool inputSchema parses as well-formed JSON in that language's codec, and (b) assert the parsed inputSchema is STRUCTURALLY EQUAL to the reference inputSchema expectation from the T7 language-neutral scenario (the schema already validated well-formed at T5/T9/T11). Structural parity against an already-validated reference catches exactly the codec-rendering divergence the reviewer flagged, at a cost the 7 ecosystems can bear. This makes the coverage a documented, intentional design -- not a silent gap -- while still gating each replica's codec rendering. Full-validator parity for all 9 is recorded as a possible follow-up, NOT v1."
- ledgerRefs: ["goals:G1"]

### K2 — locked

- createdAt: 2026-06-04T18:19:15.347Z
- updatedAt: 2026-06-04T18:19:15.347Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: "plan review: approved"
- rationale: "Reviewer go-ahead on R3 (0 criticisms, 0 new questions): plan fine-grained, sequenced, testable, grounded, complete -> executable."
- ledgerRefs: ["goals:G1"]

### K3 — locked

- createdAt: 2026-06-04T18:45:59.600Z
- updatedAt: 2026-06-04T18:45:59.600Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: "MCP tool inputSchema strategy: build a DEDICATED shared, language-agnostic MCP inputSchema emitter (reusing OAS fragment logic), NOT extend the OpenAPI emitter"
- rationale: |
    Reproduced (not asserted) against the OpenAPI emitter at e61a5068 (byte-identical to T4 base 8ada6961 — no translator/openapi change between). Full gap analysis with captured emitter output committed at docs/research/mcp-inputschema-gap-analysis.md.
    
    KEY REPRODUCED FINDINGS: (1) The typer reifies each service method's inline `data in` block into a first-class DTO named <Pkg>_<Service>_<method>_in, so the OAS document ALREADY contains the per-tool request schemas MCP needs — services are skipped only as nodes, their I/O survives as DTOs. (2) BUT those request schemas are NOT MCP-usable standalone: every field that references a named type (nested DTO, enum, ADT, collection element, recursive self-ref, alias target) emits a bare {"$ref":"#/components/schemas/<Name>"} that resolves only inside the shared OpenAPI document. MCP inputSchema is a SINGLE self-contained schema with no components registry, so those refs are dangling. In the repro 3 of 4 tool requests (composite_in, shapes_in, collections_in) are affected; only ping_in (empty) is clean. (3) ADT branches are emitted as sibling top-level entries, never inlined. (4) Recursive types (Tree) self-$ref into #/components/schemas — needs document-root or local $defs in standalone form. (5) Foreign types with language mappings but no Baboon->Baboon rt emit opaque {type:object} even when they map to `string` everywhere (FFancy). (6) any-opaque is the one fully-inline standalone-valid construct.
    
    ROOT CAUSE: the OAS emitter is architected for the OpenAPI DOCUMENT model (one doc, shared components/schemas registry, intra-document $refs, flat type library, no per-tool wrapper, service-node skip). That assembly model is fundamentally incompatible with MCP's standalone-per-tool inputSchema contract. The SCALAR/COLLECTION/OBJECT FRAGMENT generation (OasTypeTranslator.typeRefSchema/scalarSchema/mapSchema, the opt/lst/set/map shapes) is correct and reusable.
    
    DECISION: Build a DEDICATED shared MCP inputSchema emitter (language-agnostic, JVM-side, mirroring the validator-backed OAS/GraphQL emitters per K1), reusing the OAS fragment-generation logic, that assembles a SELF-CONTAINED JSON Schema per tool by computing the reachable named-type closure from each method request DTO and emitting it as a LOCAL $defs block ($ref rewritten to #/$defs/<Name>), with the request DTO as the schema root. Do NOT extend the OpenAPI emitter: (a) the OAS emitter must keep emitting cross-document #/components/schemas refs — that is correct for OpenAPI consumers and changing it would regress test-openapi/swagger-parser; (b) MCP needs a per-tool wrapper + closure-bundling + ref-rewriting concern the OAS emitter has no place for; (c) a dedicated emitter is the shared single source feeding all 9 language backends (T7/T9/T11 reference, T12-T18 replicas) and is the natural home for the K1 validator-backed well-formedness gate. SECONDARY fixes folded into T5 scope: resolve foreign-type language mappings to precise scalar schemas where determinable (else keep opaque object); record the lossy f128/tsu/tso collapses as known/acceptable for v1.
    
    This directly drives T5 (shared inputSchema emitter implementation).
- alternatives: "REJECTED — extend the OpenAPI emitter to be MCP-inputSchema-complete: would require teaching it two output modes (cross-doc-ref OpenAPI mode vs. inlined/$defs standalone MCP mode), a per-tool wrapper, and closure bundling — concerns alien to an OpenAPI document emitter, and risks regressing the swagger-parser-validated OAS output. The reusable part (fragment generation) is extractable WITHOUT coupling the two assembly models."
- sourceRefs: ["docs/research/mcp-inputschema-gap-analysis.md","baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/openapi/OasBaboonTranslator.scala","baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/openapi/OasTypeTranslator.scala"]
- ledgerRefs: ["goals:G1"]
- tags: ["mcp","inputschema","openapi","T4","T5"]

### K4 — locked

- createdAt: 2026-06-04T18:47:22.053Z
- updatedAt: 2026-06-04T18:47:22.053Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: "MCP v1 protocol contract: tools-only (initialize/tools/list/tools/call); service-method -> one tool; <service>_<method> naming; two-channel error mapping. Contract doc: docs/research/mcp-protocol-contract.md"
- rationale: |
    Fixes the wire contract all 9 per-language MCP server generators (scala,cs,python,rust,typescript,kotlin,java,dart,swift) implement IDENTICALLY. Ambiguity here multiplies into 9 divergent backends, so the contract doc (docs/research/mcp-protocol-contract.md, committed on branch implement/T2) is normative and exhaustive for v1. Grounded in JSON-RPC 2.0 (jsonrpc.org spec) + MCP revision 2025-06-18 (base/lifecycle/tools) and the existing BaboonServiceWiring contract (BaboonMethodId(ServiceName,MethodName), JsonMuxer keyed by ServiceName, Invoke(method,data,ctx,codecCtx), BaboonWiringError taxonomy).
    
    SCOPE (Q1): one MCP server per Baboon service; each service method = exactly one MCP tool (1:1). Protocol surface limited to JSON-RPC 2.0 initialize + tools/list + tools/call, plus the required notifications/initialized (accepted, no response).
    
    TRANSPORT-ABSTRACT (Q2): emit protocol state machine + tool dispatch ONLY; transport is an injected adapter. Single dispatch entrypoint handle(JsonRpcRequest)->JsonRpcResponse, analogous to invoke(...); tools/call delegates to existing JsonMuxer.Invoke reusing per-language JSON codecs. JSON only; UEBA out of scope.
    
    TOOL NAMING: tool.name = <serviceName>_<methodName>, casing preserved VERBATIM from the model (NOT per-language cased) so the wire name is byte-identical across all 9 backends; single '_' join (clients restrict tool names to [A-Za-z0-9_-]). Reverse map via generated name->BaboonMethodId table (no runtime '_' splitting). COLLISION HANDLING: candidate names computed across all bound methods; any collision = FAIL COMPILATION with a deterministic diagnostic naming the two (service,method) pairs (no silent numeric suffixing, which would diverge across runtimes by iteration order). No cross-domain namespacing in v1 (muxer routes by serviceName; DuplicateService wiring error covers dup service names).
    
    ENVELOPES (JSON-RPC 2.0): initialize req params {protocolVersion:'2025-06-18', capabilities, clientInfo{name,version}}; result {protocolVersion:'2025-06-18', capabilities:{tools:{}} (NO listChanged, no resources/prompts/etc), serverInfo{name,version}}. tools/list result {tools:[{name, description?, inputSchema}]}, deterministic order = model declaration order; cursor IGNORED, no nextCursor (no pagination in v1); inputSchema is a JSON Schema object derived from the method request DTO (emitter strategy = decisions:K3, validity coverage = decisions:K1), structurally identical across all 9 backends; outputSchema NOT emitted. tools/call req params {name, arguments(object)}; success result {content:[{type:'text', text:<JSON-encoded result DTO>}], isError:false (may be omitted)}; no structuredContent in v1.
    
    ERROR MAPPING (two channels): Channel A = JSON-RPC protocol error object: -32700 non-JSON bytes (adapter, id null); -32600 not a valid JSON-RPC request OR tools/* before initialize; -32601 unknown JSON-RPC method (e.g. resources/list, prompts/list -> the v1 exclusions); -32602 invalid params incl. missing name, arguments not an object, AND unknown/unmapped tool name (DECISION: unknown tool name -> -32602 Invalid params, NOT -32601, because the JSON-RPC method tools/call WAS found, only params.name is invalid); -32603 internal error incl. EncoderFailed on a successful result. Channel B = tools/call result with isError:true {content:[{type:'text',text:...}]}: DecoderFailed (arguments is an object but won't decode into the request DTO -> bad domain payload, LLM-correctable) and CallFailed(method,domainError) (declared Baboon method error, JSON-encoded in text; NOT flattened into a JSON-RPC error code).
    
    V1 EXCLUSIONS (MUST NOT be emitted): resources/*, prompts/*, completion/complete, sampling/*, roots/*, elicitation/*, logging/setLevel; all notifications beyond notifications/initialized (no tools/list_changed -> no tools.listChanged advertised, tool set is static); tools/list pagination; real transport; cross-language NxN client matrix (all follow-ups, not v1).
    
    GATE: no per-language MCP generator work starts before this is locked.
- alternatives: "Tool naming: (a) '/' or '.' separator -- rejected, many MCP clients restrict tool names to [A-Za-z0-9_-]. (b) silent numeric suffixing on collision -- rejected, suffix order depends on per-runtime iteration order and would diverge across the 9 backends; fail-fast is the only non-divergent choice. (c) prepend domain/package namespace to every tool name -- rejected for v1, bloats names and is unnecessary since v1 is one-server-per-service and the muxer routes by serviceName. Unknown tool name: -32601 Method-not-found -- rejected, the JSON-RPC method tools/call exists; -32601 is reserved for an unknown JSON-RPC method. Result shape: structuredContent + outputSchema -- deferred out of v1 to a single text block carrying the JSON-encoded result DTO (broadly-supported convention), reducing per-backend surface. DecoderFailed as -32602 protocol error -- rejected for the object-but-undecodable case; treated as Channel B isError so an LLM client can observe and self-correct (malformed params i.e. arguments-not-an-object stays Channel A -32602)."
- sourceRefs: ["docs/research/mcp-protocol-contract.md","docs/research/mcp-generators-grounding.md","https://www.jsonrpc.org/specification","https://modelcontextprotocol.io/specification/2025-06-18/server/tools","baboon-compiler/src/main/resources/baboon-runtime/cs/BaboonServiceWiring.cs"]
- ledgerRefs: ["goals:G1"]
- dependsOn: ["K1","K3"]
- landsIn: ["M2","M3"]
- tags: ["mcp","protocol","contract","v1","json-rpc","T2"]

### K5 — locked

- createdAt: 2026-06-04T19:01:28.541Z
- updatedAt: 2026-06-04T19:01:28.541Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: "MCP transport-abstract dispatch runtime contract: per-request handle(JsonRpcRequest, McpSession, Ctx, BaboonCodecContext) -> Option<JsonRpcResponse> entrypoint; additive IBaboonMcpServer<Ctx> + JsonRpc{Request,Response,Error} + McpToolEntry + McpSession runtime types mirroring IBaboon{Json,Ueba}ServiceCtx; tools/call delegates to existing JsonMuxerCtx.invoke; NO generated I/O loop. Doc: docs/research/mcp-dispatch-runtime-contract.md"
- rationale: |
    Runtime-shape sibling of the K4 wire contract: K4 fixes what goes on the wire, this fixes the generated-code shape + additive runtime types all 9 per-language MCP generators (scala,cs,python,rust,typescript,kotlin,java,dart,swift) target IDENTICALLY, so T8/T10/T12-T18 implement one non-divergent design. Grounded against the verified per-language service-wiring runtimes (cs BaboonServiceWiring.cs, ts BaboonSharedRuntime.ts, swift baboon_service_wiring.swift, python baboon_service_wiring.py, rust baboon_service_wiring.rs) per T1 grounding section 2.
    
    DESIGN PRINCIPLE: mirror the abstract-context service contract one tier up. The MCP runtime adds NO new dispatch mechanism; it is a protocol shell over the established {Json,Ueba}MuxerCtx<Ctx,R>.
    
    DISPATCH ENTRYPOINT (transport-abstract, per K4 section 1.4): single synchronous IBaboonMcpServer<Ctx>.handle(request: JsonRpcRequest, session: McpSession, ctx: Ctx, codecCtx: BaboonCodecContext): Option<JsonRpcResponse>. Generic over the SAME Ctx as IBaboonJsonServiceCtx<Ctx,R> (NOT R-parametric: the response envelope is fixed). None return == accepted notification (no reply). Performs no I/O; no sockets/stdin/stdout/while-loop in generated code.
    
    RUNTIME TYPE FAMILY (additive, analogous to IBaboon*ServiceCtx, shipped as static baboon-runtime/<lang>/ resource files siblings of the service-wiring runtime): IBaboonMcpServer<Ctx> (the entrypoint interface); JsonRpcRequest{id, method, params}, JsonRpcResponse{id, result, error}, JsonRpcError{code, message, data} (transport-neutral parsed values; params/result reuse each language's EXISTING JSON value type, no new JSON model); McpSession{initialized:bool} (the ONLY mutable value, adapter-owned per connection, carries the K4 section 2.1 pre-initialize latch out of the immutable server object); McpToolEntry{name, method:BaboonMethodId, description?, inputSchema}.
    
    TRANSPORT-INJECTION SEAM: generated surface EXCLUDES the I/O loop, framing (newline/Content-Length/SSE), sockets, and connection lifecycle. A hand-written adapter (NOT generated, documented recipe) owns the loop: parses bytes -> JsonRpcRequest (-32700 id:null on parse fail, raised by adapter), creates per-connection McpSession + per-request Ctx, calls handle, serializes Some(response) or writes nothing for None. Exact analogue of how the service-acceptance harness wraps the muxer behind a real HTTP server.
    
    TOOL REGISTRY: immutable ordered McpToolEntry list + derived name->entry map, fields of the concrete <Service>McpServer, order = model declaration order (deterministic, K4 section 2.3). inputSchema is the precomputed self-contained JSON Schema from the K3 dedicated shared emitter (validator-backed at source per K1), embedded as a generated constant -- runtime carries schemas, does not compute them. Codec-backed invoke = lookup(params.name)->BaboonMethodId then muxer.invoke(method, argsJson, ctx, codecCtx); codecs reached ONLY through the existing muxer (no second codec path). Collision handling is a generation-time check (K4 section 1.2 rule 4); runtime registry is built from an already-validated collision-free set.
    
    CTX THREADING: Ctx is the SAME service-wiring abstract-context Ctx. Adapter constructs it; handle passes it opaquely into muxer.invoke for tools/call (unused for initialize/tools/list); reaches the user method body unchanged via the unmodified muxer. service.context=none targets the context-free JsonMuxer<R> with Unit/void Ctx -- same none/abstract/type trichotomy ServiceContextResolver drives, to be MCP-extended in T5/T6.
    
    RUNTIME HELPER LOCATION: baboon-runtime/<lang>/ resource files siblings of the service-wiring files (cs BaboonMcpRuntime.cs, scala BaboonMcpRuntime.scala, ts append/BaboonMcpRuntime.ts, kt(+kmp) BaboonMcpRuntime.kt, java split per-type .java, python baboon_mcp_runtime.py, rust baboon_mcp_runtime.rs, dart append baboon_runtime.dart, swift baboon_mcp_runtime.swift). Static (no per-model templating); only the concrete <Service>McpServer + registry literals are generated, by a per-language *McpTranslator sibling of *ServiceWiringTranslator. sbt clean required after editing any baboon-runtime resource (PortableResource.embedSources caches).
    
    PER-LANGUAGE FEASIBILITY (all 9 sketched in the doc section 7, mapping cleanly onto each existing IBaboon*ServiceCtx style): scala/cs/ts/kotlin/java/dart use a generic IBaboonMcpServer<Ctx> interface/trait with Option/nullable/Optional return; rust uses a generic trait with Box<dyn> storage and session:&mut McpSession (generic-placement hard case handled, mirrors rs:151/161); python uses Generic[Ctx]+Protocol[Ctx] reusing the existing Ctx=TypeVar (hard case handled, py:189/193/206); swift uses an associatedtype-Ctx protocol + AnyMcpServer<Ctx> type-eraser (hard case handled, verbatim AnyJsonServiceCtx pattern sw:171/185); cs PascalCases ONLY internal symbols (Handle/InputSchema) -- wire tool.name and JSON-RPC method/result-key strings stay verbatim lowercase literals (K4 section 1.2 rule 2).
- alternatives: "Latch as ambient mutable state inside handle or the server object -- REJECTED: violates no-global-state, breaks concurrent connections sharing one immutable server; instead an adapter-owned per-connection McpSession value threads the latch explicitly. Make handle R-parametric like the muxer -- REJECTED: the MCP response envelope is fixed (always a JsonRpcResponse), so only Ctx is free; async, if ever needed, wraps the whole return (Future<Option<JsonRpcResponse>>) at the per-language level like the muxer wraps R. Introduce a new MCP-specific JSON value model -- REJECTED: reuse each language's existing JSON value type (Circe Json / Newtonsoft JToken / serde_json::Value / Jackson JsonNode / py dict / dart Object? / swift Any / ts unknown) the codecs already speak. Re-implement dispatch inside the MCP runtime -- REJECTED: tools/call delegates to the existing JsonMuxerCtx.invoke so there is one codec/dispatch path, not a divergent second one. Generate the transport I/O loop -- REJECTED by Q2/K4: transport is an injected hand-written adapter."
- sourceRefs: ["docs/research/mcp-dispatch-runtime-contract.md","docs/research/mcp-protocol-contract.md","docs/research/mcp-generators-grounding.md","baboon-compiler/src/main/resources/baboon-runtime/cs/BaboonServiceWiring.cs","baboon-compiler/src/main/resources/baboon-runtime/swift/baboon_service_wiring.swift","baboon-compiler/src/main/resources/baboon-runtime/python/baboon_service_wiring.py","baboon-compiler/src/main/resources/baboon-runtime/rust/baboon_service_wiring.rs","baboon-compiler/src/main/resources/baboon-runtime/typescript/BaboonSharedRuntime.ts"]
- ledgerRefs: ["goals:G1"]
- dependsOn: ["K3","K4"]
- landsIn: ["M2","M3"]
- tags: ["mcp","dispatch","runtime","contract","v1","transport-abstract","T3"]

### K6 — locked

- createdAt: 2026-06-04T19:11:33.346Z
- updatedAt: 2026-06-04T19:15:05.978Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: "MCP test stub model type inventory (LOCKED fixture for T5): 11 constructs enumerated; model at baboon-compiler/src/test/resources/baboon/mcp-stub-ok/mcp_stub.baboon"
- rationale: |
    Task T22 authors the .baboon MCP stub model that is the FIXTURE T5 is graded against. The model must compile under existing regular-adt codegen flags (verified: all 9 language backends + OpenAPI backend, zero errors, at commit implementing T22 on de570ae6 base). The model defines a `root service McpTools` with 5 methods whose inline `data in` blocks collectively exercise every construct the T5 inputSchema emitter must handle.
    
    LOCKED TYPE INVENTORY (closed checklist — T5 is graded against ALL items):
    
    1. DTO (data type, plain and nested): `Point {x:i32, y:i32}` and `Nested {point:Point, color:Color, label:opt[str]}` — Nested exercises a DTO field whose own field is another DTO (nested DTO reference).
    
    2. ADT (sealed/branching): `Shape` with two inline data branches `Circle {radius:f64}` and `Rect {w:f64, h:f64}` — exercises ADT oneOf schema with multiple discriminated branches.
    
    3. Enum: `Color {Red, Green, Blue}` — exercises enum-as-string-literal schema; also used as a map key (legal key type).
    
    4. Collection list: `lst[str]` (method `listCollections`.tags) and `lst[Tree]` (recursive child list in `Tree`.children) — exercises array schema.
    
    5. Collection set: `set[i64]` (method `listCollections`.uniqueIds) — exercises array+uniqueItems schema.
    
    6. Collection map with string key: `map[str,str]` (method `listCollections`.labels) — exercises object+additionalProperties schema.
    
    7. Collection map with non-string key (enum key — legal): `map[Color,str]` (method `listCollections`.byColor) — exercises array-of-{key,value}-entry-objects schema. NOTE: `map[Point,str]` (multi-field DTO key) is REJECTED by the typer; only single-primitive wrappers, id types, enums, and foreign types are legal map keys. Enum key (`Color`) is the canonical non-string map key form.
    
    8. Option (optional field): `opt[str]` in `Nested`.label, `opt[Tree]` in `Tree`.left, `opt[Point]` in `submitComposite` method request — exercises oneOf[T,null] nullable schema.
    
    9. Foreign type (with per-language string mappings, no rt= override): `FFancyStr` mapped to the string type in all 9 target languages (cs/scala/kotlin/java/py/rust/typescript/dart/swift) — exercises the opaque foreign-type schema path (K3 records this emits as opaque object without rt= but T5 should resolve per-language mappings to precise scalars where determinable).
    
    10. Type alias / template instantiation: `data Page[T] {items:lst[T], total:u32}` + `root type PointPage = Page[Point]` — exercises the typer materialising a generic template into a concrete DTO alias before codegen.
    
    11. Recursive / self-referential DTO: `data Tree {value:i32, left:opt[Tree], children:lst[Tree]}` — exercises the $defs closure-bundling requirement (a standalone inputSchema for a recursive type MUST use a local $defs block, as reproduced in the T4 gap analysis).
    
    MODEL PATH: `baboon-compiler/src/test/resources/baboon/mcp-stub-ok/mcp_stub.baboon`
    
    SERVICE METHODS (5 total — each construct appears in at least one method's request DTO):
    - `listCollections`: covers list, set, map[str,V], map[Color,V] (items 4,5,6,7)
    - `submitComposite`: covers nested DTO, opt[DTO], enum, foreign type (items 1,8,3,9)
    - `processShape`: covers ADT, recursive DTO (items 2,11)
    - `pagePoints`: covers type alias PointPage (item 10)
    - `ping`: covers scalar-only baseline (i32, str) — the trivially standalone case for comparison
    
    COMPILE VERIFICATION: model compiles with zero typer/validator errors under all 9 regular-adt codegen backends (cs/scala/python/rust/typescript/kotlin/java/dart/swift with --*-wrapped-adt-branch-codecs=false --*-write-evolution-dict=true --generate-ueba-codecs-by-default=true --generate-json-codecs-by-default=true) and under the openapi backend (--disable-conversions=true --runtime=without). Baboon 0.0.188-SNAPSHOT @ main#e61a5068992d1ecd58079de2fda648e12bd53c55*; 30 retained definitions, 12 unreachable, no errors.
- sourceRefs: ["baboon-compiler/src/test/resources/baboon/mcp-stub-ok/mcp_stub.baboon","docs/research/mcp-inputschema-gap-analysis.md","docs/research/mcp-generators-grounding.md"]
- ledgerRefs: ["goals:G1"]
- dependsOn: ["K3","K4","K5"]
- tags: ["mcp","fixture","stub-model","type-inventory","T22","T5"]
