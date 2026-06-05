---
ledger: tasks
counters:
  milestone: 0
  item: 23
archives: []
---

# tasks

## M2

### T1 — done

- createdAt: 2026-06-04T18:02:24.431Z
- updatedAt: 2026-06-04T18:39:58.800Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: Re-verify repo grounding for MCP generators (4-site flags, server contract, openapi emitter)
- description: "Ground the implementation against CURRENT code (the planning grounding is from dated memory). Confirm the exact file:line sites for: (1) the per-language *Options 4-site mechanism -- CompilerOptions.scala *Options case classes, .jvm Baboon.scala + CLIOptions.scala, .js BaboonJS.scala constructions, test constructions, playground options.ts/compiler.ts; (2) the transport-abstract service-wiring runtime contract (IBaboon{Json,Ueba}ServiceCtx<Ctx,R>, per-invocation invoke, per-lang *ServiceWiringTranslator.scala, ServiceContextResolver) under translator/; (3) the existing translator/openapi JSON-Schema emitter surface and what types it can/can't render; (4) the wiring-overlay test harness layout (test/<lang>-stub-*-overlay/, .mdl/defs/tests.md test-gen-<lang>-wiring*/test-<lang>-wiring* actions, the `test` aggregator). Produce a short grounding note (file:line map) consumed by every downstream task."
- acceptance: "A grounding note exists listing the verified current file:line for each of the 4 flag sites, the service-wiring runtime contract types, the openapi schema emitter entrypoint, and the wiring-overlay harness actions; each citation checked against current source (not memory). Discrepancies vs. the goal grounding are flagged."
- suggestedModel: standard
- ledgerRefs: ["goals:G1"]
- resultCommit: 8ada6961304fceb858c75748f051d3f24155929f
- completion: "Added docs/research/mcp-generators-grounding.md: verified file:line map for the 4 flag sites, service-wiring runtime contract, openapi emitter, and wiring-overlay harness, with 8 flagged discrepancies vs. plan grounding."
- sessionLogs: ["docs/logs/20260604-183708-a995d92497150fc94.md","docs/logs/20260604-183921-a3a486c7fedf930fb.md"]

### T2 — done

- createdAt: 2026-06-04T18:02:33.039Z
- updatedAt: 2026-06-04T18:51:33.428Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: Lock the MCP scope + protocol-mapping decision (tools-only; method -> tool)
- description: "Record a locked `decisions` item fixing the v1 MCP scope per Q1/Q2: per-Baboon-service MCP server; each service method maps to exactly one MCP tool; protocol surface limited to JSON-RPC 2.0 `initialize` (capability handshake advertising tools), `tools/list` (enumerate tools + their inputSchema), and `tools/call` (dispatch a tool by name with JSON arguments, return JSON result). Resources/prompts/completions explicitly OUT of scope for v1. Define the exact JSON-RPC request/response envelope shapes, the tool-naming scheme (service method -> tool name), and how a Baboon method error surfaces as an MCP tool error vs. JSON-RPC error. This decision is the contract the per-language generators implement identically."
- acceptance: "A `locked` decisions item exists under M1 (ledgerRefs goals:G1) specifying: the service->tool mapping, the three JSON-RPC methods and their envelope shapes, tool-naming, error mapping, and the explicit v1 exclusions. No per-language generator work starts before this is locked."
- suggestedModel: frontier
- dependsOn: ["T1"]
- ledgerRefs: ["goals:G1"]
- resultCommit: 3c9f08c0f846847868f1f6a179b5ec853c6deb57
- completion: "Locked MCP v1 protocol contract (decisions:K4) + docs/research/mcp-protocol-contract.md: per-service server, method->tool (1:1), <service>_<method> naming, tools-only initialize/tools-list/tools-call JSON-RPC envelopes, two-channel error mapping, v1 exclusions."
- sessionLogs: ["docs/logs/20260604-184825-aef410a29233b5d63.md","docs/logs/20260604-185042-accf9595268f73098.md"]

### T3 — done

- createdAt: 2026-06-04T18:02:43.682Z
- updatedAt: 2026-06-04T19:04:49.888Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: Design the transport-abstract MCP dispatch runtime contract
- description: "Per Q2, design the language-agnostic runtime contract for the MCP server: a transport-abstract dispatch entrypoint that handles JSON-RPC method routing + tool dispatch and leaves the concrete stdio/Streamable-HTTP transport to an injected adapter. Mirror the existing abstract-context service contract (per-invocation invoke(method,data,ctx,codecCtx) and IBaboon{Json,Ueba}ServiceCtx<Ctx,R>): the generated MCP server should expose something like a per-request `handle(jsonRpcRequest, ctx) -> jsonRpcResponse` (or `dispatch`) entrypoint, additive runtime types analogous to the IBaboon*ServiceCtx family, no baked-in I/O loop. Specify: the runtime interface(s) the generated code targets, where the tool registry lives, how ctx threads through, and where the runtime helpers go (analogous to baboon-runtime resource files). Output is a portable design that maps cleanly onto all 9 languages."
- acceptance: "A design note (and a locked decisions item) define the MCP dispatch runtime contract: the per-request dispatch entrypoint signature, the runtime interface/types (analogous to IBaboon{Json,Ueba}ServiceCtx), the transport-injection seam, and the tool-registry shape -- with a mapping sketch onto each of the 9 target languages confirming feasibility. No transport I/O loop is part of the generated surface."
- suggestedModel: frontier
- dependsOn: ["T2"]
- ledgerRefs: ["goals:G1"]
- resultCommit: de570ae678cb9edadea2baf54029a6b4fd7835b3
- completion: "Locked MCP dispatch runtime contract (decisions:K5) + docs/research/mcp-dispatch-runtime-contract.md: handle(JsonRpcRequest,McpSession,Ctx,BaboonCodecContext)->Option<JsonRpcResponse>, IBaboonMcpServer<Ctx> + JsonRpc{Request,Response,Error}/McpToolEntry/McpSession family, transport-injection seam (no I/O loop), tool registry, baboon-runtime/<lang>/ home, 9-language feasibility sketch."
- sessionLogs: ["docs/logs/20260604-190215-ab68645134bca8eaa.md","docs/logs/20260604-190416-aec5b0bdad463cbe5.md"]

### T4 — done

- createdAt: 2026-06-04T18:02:53.337Z
- updatedAt: 2026-06-04T18:51:36.913Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: Review the openapi JSON-Schema emitter and decide the tool inputSchema strategy
- description: "Per Q4 (and the user's explicit caveat that the translator/openapi JSON-Schema machinery is demo-grade and 'most likely won't be enough'): review the current openapi/JSON-Schema emitter against the full Baboon type system (DTOs, ADTs, enums, collections list/set/dict, options, foreign types, type aliases, nested/recursive types) used as MCP tool request types. Identify concretely which constructs it renders correctly and which it cannot. Then DECIDE and LOCK: extend the existing openapi schema emitter to be MCP-inputSchema-complete, OR write a dedicated MCP inputSchema emitter (shared, language-agnostic, producing the JSON-Schema each tool advertises in tools/list). Reproduce the gaps first (concrete failing/insufficient schema outputs) before deciding -- per reproduction discipline."
- acceptance: "A locked decisions item records the chosen inputSchema strategy (extend openapi emitter vs. dedicated MCP emitter) with rationale, backed by a documented gap analysis: a list of Baboon type constructs with each marked supported/unsupported by the current openapi emitter, plus concrete example outputs demonstrating the gaps that drove the decision."
- suggestedModel: frontier
- dependsOn: ["T1"]
- ledgerRefs: ["goals:G1"]
- resultCommit: d3e0557dbd41dfaa66e6b45975cf096c668498db
- completion: "Locked inputSchema strategy (decisions:K3) = dedicated shared MCP inputSchema emitter (not extend OpenAPI), backed by docs/research/mcp-inputschema-gap-analysis.md reproducing the dangling-$ref scope mismatch + full construct table."
- sessionLogs: ["docs/logs/20260604-184825-a1fdda4c72e5d6185.md","docs/logs/20260604-185042-addb8396550df9d03.md"]

### T5 — done

- createdAt: 2026-06-04T18:03:02.126Z
- updatedAt: 2026-06-04T19:45:48.862Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: Implement the shared/language-agnostic MCP inputSchema emitter (per the locked strategy)
- description: "Implement the inputSchema-derivation machinery chosen and locked in T4: either extend the translator/openapi JSON-Schema emitter to fully cover the Baboon type system, or build the dedicated shared MCP inputSchema emitter. This is the language-agnostic schema core that every per-language MCP generator calls to produce each tool's inputSchema from its method request DTO. Cover DTOs, ADTs, enums, collections, options, foreign types, aliases, and nested/recursive types as exercised by the MCP stub model authored in T22 (whose locked type inventory is the closed checklist this emitter must satisfy). Defect-fix discipline: if T4 reproduced concrete gap outputs, those become the failing cases this task must turn correct. VALIDITY GATE (per review R1): the JVM unit test MUST run each emitted inputSchema through a real JSON-Schema validator (mirroring how test-openapi uses swagger-parser and test-graphql uses graphql-js buildSchema), asserting each tool's inputSchema is well-formed JSON Schema -- not merely a golden/shape match."
- acceptance: The schema emitter produces valid JSON Schema for every Baboon construct enumerated in the T22 locked type inventory and used by the T22 stub model; the gap cases reproduced in T4 now emit correct schemas; a JVM unit test (a) asserts representative inputSchema outputs (golden/shape) AND (b) runs every emitted inputSchema through a real JSON-Schema validator and asserts well-formedness. This emitter is the single source the per-language generators consume.
- suggestedModel: frontier
- dependsOn: ["T4","T3","T22"]
- ledgerRefs: ["goals:G1"]
- resultCommit: 60b6a26e1539c9a2c12750ed080b4f49f87bba13
- completion: Added shared McpInputSchemaEmitter (translator/mcp/) producing self-contained Draft-2020-12 inputSchema with local $defs closure (fixes T4 dangling-$ref + recursion); JVM test McpInputSchemaEmissionTest validates every emitted schema via networknt + golden shape (6/6); sbt +compile (JVM+Scala.js) green. Latent gaps filed as D1/D2.
- sessionLogs: ["docs/logs/20260604-193715-ae9288c27fb7acd53.md","docs/logs/20260604-194449-a76ca749b5bafa422.md"]

### T22 — done

- createdAt: 2026-06-04T18:11:20.514Z
- updatedAt: 2026-06-04T19:15:38.272Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: Author the MCP test stub model + lock its type inventory (fixture-before-emitter)
- description: "Split the MCP stub-model authoring OUT of T7 so the fixture exists BEFORE the inputSchema emitter (T5) is graded against it -- resolving the T5<->T7 circularity flagged in review R1. Author the .baboon MCP stub model: a small service with a few methods whose request DTOs deliberately span the representative Baboon type inventory the inputSchema emitter must cover -- DTOs, ADTs, enums, collections (list/set/dict), options, foreign types, type aliases, and nested/recursive types. Record a LOCKED `decisions` item enumerating that exact type inventory (the closed checklist of constructs the stub exercises) so T5's acceptance has a concrete, pre-existing fixture + inventory to grade against. The model must compile under the existing regular-adt codegen flags. This task produces the model + locked inventory ONLY; the language-neutral round-trip scenario stays in T7."
- acceptance: "A .baboon MCP stub model exists and compiles under the existing regular-adt codegen flags, AND a `locked` decisions item (ledgerRefs goals:G1) enumerates the exact type inventory it exercises (DTOs/ADTs/enums/collections/options/foreign/aliases/recursive). This model + inventory is the fixture T5 is graded against, and it exists before T5 runs."
- suggestedModel: standard
- dependsOn: ["T2","T3"]
- ledgerRefs: ["goals:G1"]
- resultCommit: 484e16177e402b65c9924eaf6760bb69859aa970
- completion: "Added MCP stub model fixture baboon-compiler/src/test/resources/baboon/mcp-stub-ok/mcp_stub.baboon (service McpTools, 5 methods); locked type inventory in decisions:K6 (11 constructs); compiles green under all 9 regular-adt backends."
- sessionLogs: ["docs/logs/20260604-191217-a39f3a1faa01a5339.md","docs/logs/20260604-191459-a6da1814ad437ab91.md"]

## M3

### T6 — done

- createdAt: 2026-06-04T18:03:13.175Z
- updatedAt: 2026-06-04T20:16:44.577Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: Add --<lang>-generate-mcp-server flag (all 9) + own the flag->MCP-generator dispatch seam
- description: "Per Q3, add one boolean per language `--<lang>-generate-mcp-server` (default false) for all 9 languages, threaded through the established 4 sites: (1) add the field to each per-language *Options case class in CompilerOptions.scala; (2) declare the CLI flag in .jvm CLIOptions.scala and wire opts.<x>.getOrElse(false) in .jvm Baboon.scala; (3) update every *Options(...) construction in .js BaboonJS.scala (THE cross-build trap -- sbt +compile must stay green); (4) update all manual *Options constructions in tests. Defer playground exposure to W4. ALSO OWN THE DISPATCH SEAM (per review R1): this task explicitly owns reading the per-language flag and dispatching to the MCP generator inside each per-language BaboonAbstractTranslator invocation path -- i.e. the flag-read -> MCP-generator-call wiring is created here as an explicit, named seam (calling an initially-stubbed MCP generator hook). The per-language generator tasks (T8/T10/T12-T18) then only fill the generator BEHIND this existing seam; they do not have to (re)wire dispatch. When the flag is false (default) behavior is byte-identical to today; when true the seam calls the generator hook (initially a clearly-stubbed nothing until T8+ fill it)."
- acceptance: "All 9 --<lang>-generate-mcp-server flags exist, default false; `sbt +compile` (cross-build, e.g. via mdl :build / test-sbt-basic) is green with all BaboonJS.scala constructions updated; an explicit, named flag-read -> MCP-generator dispatch seam exists in each per-language BaboonAbstractTranslator path (initially calling a stubbed MCP generator hook), so no downstream generator task has to wire dispatch. With every flag false the compiler output is byte-identical to the pre-change baseline (NoMcp path proven inert). Toggling a flag true is accepted by the CLI even though the generator hook is still a stub (no crash; emits nothing or a clearly-stubbed nothing)."
- suggestedModel: standard
- dependsOn: ["T1","T3"]
- ledgerRefs: ["goals:G1"]
- resultCommit: f3766aff005f7eda30d645912fb561ad4add91a7
- completion: Added generateMcpServer flag to all 9 *Options + 4-site threading (CompilerOptions/CLIOptions/Baboon.scala/BaboonJS.scala + 26 test sites); new McpServerGeneratorHook trait+stub wired into 9 per-language translators with explicit flag-read->hook dispatch seam; off-by-default byte-identical; sbt +compile (JVM+JS) + baboonJVM/test 578/578 green.
- sessionLogs: ["docs/logs/20260604-200534-a845c35972bb7e30b.md","docs/logs/20260604-201605-a662e9f88829c4f5b.md"]

### T7 — done

- createdAt: 2026-06-04T18:03:21.931Z
- updatedAt: 2026-06-04T20:19:55.307Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: Define the shared language-neutral MCP round-trip scenario (against the T22 stub model)
- description: "Define the shared cross-language round-trip scenario the per-language MCP overlays will exercise, AGAINST the MCP stub model already authored + inventory-locked in T22 (this task no longer authors the model -- that was split out to T22 to resolve the T5<->T7 circularity flagged in review R1). Define the canonical round-trip script in language-neutral terms: an `initialize` call (assert advertised tool capabilities), a `tools/list` call (assert tool names + the inputSchemas emitted for the T22 model match expectations), and `tools/call` invocations (assert correct dispatch + JSON result, plus at least one error path). This scenario is the contract each per-language test asserts against."
- acceptance: A documented language-neutral round-trip scenario (initialize/tools-list/tools-call incl. an error case) exists with expected tool names, inputSchemas, and call results, defined against the T22 stub model; ready to be implemented as a per-language overlay test.
- suggestedModel: standard
- dependsOn: ["T3","T22","T5"]
- ledgerRefs: ["goals:G1"]
- resultCommit: b548d682d6027d0456987f66f82d1c9e35644999
- completion: "Added docs/research/mcp-roundtrip-scenario.md: language-neutral initialize/tools-list/tools-call(+error) scenario against the McpTools stub model with concrete tool names, inputSchemas (verified vs T5 emitter), call results, and assertion-discipline/K1-tier notes. §7 task->language mapping corrected to ledger after 1 review round."
- sessionLogs: ["docs/logs/20260604-200534-a77ba78a93ede5ead.md","docs/logs/20260604-201825-a71663a1b7d381360.md","docs/logs/20260604-201605-a905954a2dc8c687b.md","docs/logs/20260604-201921-a753a27edceb3fdf3.md"]

### T8 — done

- createdAt: 2026-06-04T18:03:30.913Z
- updatedAt: 2026-06-04T20:47:16.526Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: Implement the TypeScript MCP server generator + runtime
- description: "Implement the MCP server generator for TypeScript (one of the two reference backends, mirroring the TS-first order used for the abstract-context work). Behind --ts-generate-mcp-server, FILLING the dispatch seam already wired by T6 (T6 owns reading the flag and calling the MCP-generator hook in the per-language BaboonAbstractTranslator path -- this task supplies the generator behind that existing seam, it does NOT re-wire dispatch). Generate, per Baboon service: the transport-abstract MCP dispatch entrypoint per the T3 contract (JSON-RPC routing for initialize/tools-list/tools-call, transport injected), the tool registry mapping each method->tool, inputSchema from the T5 emitter, and JSON arg/result (de)serialization reusing the existing TS JSON codecs. Add any additive TS runtime types (analogous to the IBaboon*ServiceCtx runtime). UEBA not involved."
- acceptance: With --ts-generate-mcp-server=true the T6 dispatch seam invokes this generator and the compiler emits a per-service TS MCP server that compiles under tsc; with the flag false TS output is byte-identical to baseline. Generated dispatch exposes a transport-abstract per-request entrypoint (no baked-in I/O). A JVM codegen-shape test asserts the TS MCP output shape.
- suggestedModel: frontier
- dependsOn: ["T6","T7"]
- ledgerRefs: ["goals:G1"]
- resultCommit: 346ff3345237203ec6168594e3c9474f8990b94a
- completion: "Filled T6 TS dispatch seam: TsMcpServerGenerator emits per-service <Service>McpServer<Ctx> (declaration-ordered tool registry, <service>_<method> names, inputSchema from T5 emitter, tools/call via injected invokeJson delegate, two-channel K4 errors) backed by static baboon-runtime/typescript/BaboonMcpRuntime.ts (transport-abstract handle(), no I/O loop). tsc exit 0; sbt +compile green; JVM shape test TypeScriptMcpServerEmissionTest green; flag-off byte-identical. Reference shape for T10/T12-T18."
- sessionLogs: ["docs/logs/20260604-203649-a1215c23d32f156c1.md","docs/logs/20260604-204636-a65b17c5c2f913ef2.md"]

### T9 — done

- createdAt: 2026-06-04T18:05:06.986Z
- updatedAt: 2026-06-04T21:18:25.374Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: TypeScript MCP local round-trip overlay + test-gen-ts-mcp/test-ts-mcp actions
- description: "Build the TS MCP test harness mirroring the wiring-overlay pattern: a test/ts-stub-mcp-overlay/ that drives the generated transport-abstract dispatch entrypoint through the T7 round-trip scenario (initialize/tools-list/tools-call incl. error path) using an in-test fake transport (no real stdio/HTTP). Add the test-gen-ts-mcp action (rsync stub+overlay, codegen with TS regular-adt flags verbatim + --ts-generate-mcp-server=true) and the test-ts-mcp action (build+run the round-trip class) in .mdl/defs/tests.md. VALIDITY GATE (per review R1): at tools/list, the round-trip MUST validate each returned tool inputSchema is well-formed JSON Schema against a real JSON-Schema validator available in the TS toolchain (e.g. ajv), not merely string-compare against an expectation. Value checks must be UNCONDITIONAL throws; prove liveness with a negative control (flip one expected value -> test fails)."
- acceptance: test-gen-ts-mcp generates the MCP overlay and test-ts-mcp builds+runs green, exercising initialize/tools-list/tools-call + an error case against the generated dispatch entrypoint with an injected fake transport; at tools/list each returned inputSchema is validated well-formed against a real JSON-Schema validator (e.g. ajv). A negative control (deliberately wrong expectation) makes the test fail, proving the checks are live.
- suggestedModel: standard
- dependsOn: ["T8"]
- ledgerRefs: ["goals:G1"]
- resultCommit: 58464250af6e2efbdabb7c1746de2aa2c83e9d51
- completion: "Added test/ts-stub-mcp-overlay/ (Vitest, 16 tests) driving generated McpToolsMcpServer.handle() through full T7 scenario via fake invokeJson; ajv Draft-2020-12 validity gate + 4 live negative controls; test-gen-ts-mcp + test-ts-mcp actions in .mdl/defs/tests.md, test-ts-mcp registered in :test aggregator. Reference harness shape for T11/T12-T18."
- sessionLogs: ["docs/logs/20260604-210835-a92c4919b52df019c.md","docs/logs/20260604-211741-a916dee3c59c87ede.md"]

### T10 — done

- createdAt: 2026-06-04T18:05:15.320Z
- updatedAt: 2026-06-04T21:18:28.866Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: Implement the C# MCP server generator + runtime
- description: "Implement the MCP server generator for C# (second reference backend), behind --cs-generate-mcp-server, FILLING the dispatch seam already wired by T6 (this task supplies the C# generator behind T6's existing flag-read->generator-call seam; it does NOT re-wire dispatch). Match the TS reference shape from T8 and the T3 dispatch contract. Per service: transport-abstract dispatch entrypoint (JSON-RPC initialize/tools-list/tools-call routing, transport injected), tool registry, inputSchema from the T5 emitter, JSON arg/result (de)serialization reusing the C# Newtonsoft.Json codecs, additive C# runtime types. Apply the C#-only PascalCase method-naming convention established by the abstract-context service work (tool dispatch must still match the wire tool names). UEBA not involved."
- acceptance: With --cs-generate-mcp-server=true the T6 dispatch seam invokes this generator and the compiler emits a per-service C# MCP server that builds under dotnet; flag false -> C# output byte-identical to baseline. Dispatch is transport-abstract; C# PascalCase convention applied without breaking tool-name matching. A JVM codegen-shape test asserts the C# MCP output shape.
- suggestedModel: frontier
- dependsOn: ["T8"]
- ledgerRefs: ["goals:G1"]
- resultCommit: 001b0e5eaf9e2fc3651423132f523fdbed6b71fc
- completion: "Filled T6 C# dispatch seam: CsMcpServerGenerator (mirrors T8) + baboon-runtime/cs/BaboonMcpRuntime.cs emit per-service <Service>McpServer<Ctx> (transport-abstract Handle/no I/O loop, lowercase wire names, T5 inputSchema, injected McpJsonInvoke delegate, K4 two-channel errors); PascalCase confined to C# symbols. dotnet build 0/0; flag-off byte-identical; JVM CSharpMcpServerEmissionTest green; sbt +compile green."
- sessionLogs: ["docs/logs/20260604-210835-ae078b0b8e4553009.md","docs/logs/20260604-211741-ae97c4aee2c9e7443.md"]

### T11 — done

- createdAt: 2026-06-04T18:05:22.623Z
- updatedAt: 2026-06-04T21:47:08.132Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: C# MCP local round-trip overlay + test-gen-cs-mcp/test-cs-mcp actions
- description: "Build the C# MCP test harness mirroring the wiring-overlay pattern and the TS MCP harness (T9): test/cs-stub-mcp-overlay/ driving the generated dispatch entrypoint through the T7 round-trip scenario (initialize/tools-list/tools-call incl. error) via an in-test fake transport. Add test-gen-cs-mcp (rsync+overlay, codegen with C# regular-adt flags verbatim + --cs-generate-mcp-server=true) and test-cs-mcp (build+run) in .mdl/defs/tests.md. VALIDITY GATE (per review R1): at tools/list, validate each returned tool inputSchema is well-formed JSON Schema against a real JSON-Schema validator available in the .NET toolchain (e.g. NJsonSchema/JsonSchema.Net), not just a string compare. Value checks UNCONDITIONAL throws (C# Debug.Assert is vacuous in Release); negative-control liveness proof required."
- acceptance: test-gen-cs-mcp + test-cs-mcp build+run green, exercising initialize/tools-list/tools-call + error case against the generated C# dispatch entrypoint with an injected fake transport; at tools/list each returned inputSchema is validated well-formed against a real .NET JSON-Schema validator; negative control fails as expected. This closes the two-reference-backend milestone, giving a reviewable shape for the remaining 7.
- suggestedModel: standard
- dependsOn: ["T10"]
- ledgerRefs: ["goals:G1"]
- resultCommit: 8bdcba2b26da28b3c5d48ed6849df58b0460ed89
- completion: "Added test/cs-stub-mcp-overlay/ (NUnit McpTests, 16 tests) driving generated McpToolsMcpServer.Handle through full T7 scenario via fake McpJsonInvoke; NJsonSchema Draft-2020-12 validity gate (with $defs->definitions validator-side normalization) + live negative controls; test-gen-cs-mcp + test-cs-mcp actions, test-cs-mcp registered in :test aggregator. dotnet test 16/16."
- sessionLogs: ["docs/logs/20260604-214004-a9d1082641c528afb.md","docs/logs/20260604-214617-a1be70cc457fb5df1.md"]

## M4

### T12 — done

- createdAt: 2026-06-04T18:05:33.107Z
- updatedAt: 2026-06-04T23:01:46.281Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: Scala MCP server generator + overlay + test-gen-scala-mcp/test-scala-mcp
- description: "Replicate the reference MCP generator (T8/T10 shape, T3 dispatch contract, T5 inputSchema) to Scala behind --scala-generate-mcp-server: per-service transport-abstract dispatch entrypoint, tool registry, inputSchema, Circe JSON codec reuse, additive runtime types. Add test/scala-stub-mcp-overlay/ + test-gen-scala-mcp (Scala regular-adt flags + flag on) + test-scala-mcp (build+run round-trip) in .mdl/defs/tests.md. Round-trip drives the dispatch entrypoint through initialize/tools-list/tools-call + error via an injected fake transport; unconditional throws + negative control. VALIDITY GATE (per locked decision K1 resolving review R2): no full JSON-Schema validator is required for this replica; instead at tools/list the round-trip MUST (a) assert each returned tool inputSchema parses as well-formed JSON through the Scala/Circe codec, and (b) assert the parsed inputSchema is STRUCTURALLY EQUAL to the reference inputSchema expectation from the T7 language-neutral scenario (the schema already validated well-formed at T5/T9/T11). This catches Circe codec-rendering divergence (key escaping, number/$ref/additionalProperties/nested-map) without a per-language schema validator."
- acceptance: Flag on -> compiling per-service Scala MCP server (sbt +compile green, incl. .js cross-build unaffected); flag off -> byte-identical baseline. test-gen-scala-mcp + test-scala-mcp green; at tools/list each returned inputSchema parses as well-formed JSON through the Circe codec AND is structurally equal to the T7 reference inputSchema (per K1); negative control fails. JVM codegen-shape assertion present.
- suggestedModel: standard
- dependsOn: ["T11"]
- ledgerRefs: ["goals:G1"]
- resultCommit: a36e3bf5072c82f9d46e5af9c58b825d25eca804
- completion: "Scala MCP generator (ScMcpServerGenerator + BaboonMcpRuntime.scala, Circe codec reuse) behind --scala-generate-mcp-server, mirrors T8/T10/K5; overlay test/scala-stub-mcp + test-gen-scala-mcp/test-scala-mcp (in :test); K1 real structural equality vs T7 §2.3 refs (10/10); flag-off byte-identical. Pre-existing Scala codegen issues filed D4/D5."
- sessionLogs: ["docs/logs/20260604-223038-a82b978872a1fa7fc.md","docs/logs/20260604-224034-afc74e884b46a7584.md","docs/logs/20260604-225606-a05bc3d968ba4b235.md","docs/logs/20260604-225919-a7eddde5e8a8b4783.md"]

### T13 — done

- createdAt: 2026-06-04T18:05:38.275Z
- updatedAt: 2026-06-04T23:01:48.487Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: Rust MCP server generator + overlay + test-gen-rust-mcp/test-rust-mcp
- description: "Replicate the reference MCP generator to Rust behind --rust-generate-mcp-server: per-service transport-abstract dispatch entrypoint per T3, tool registry, inputSchema from T5, serde JSON codec reuse, additive runtime types. Mind Rust generic-placement gotchas noted for the wiring work. Add test/rust-stub-mcp-overlay/ + test-gen-rust-mcp (Rust regular-adt flags + flag on) + test-rust-mcp (cargo build+run round-trip) in .mdl/defs/tests.md; injected fake transport; unconditional panics/asserts as real failures; negative control. VALIDITY GATE (per locked decision K1 resolving review R2): no full JSON-Schema validator is required for this replica; instead at tools/list the round-trip MUST (a) assert each returned tool inputSchema parses as well-formed JSON through the Rust/serde_json codec, and (b) assert the parsed inputSchema is STRUCTURALLY EQUAL to the reference inputSchema expectation from the T7 language-neutral scenario (already validated well-formed at T5/T9/T11). This catches serde codec-rendering divergence (key escaping, number/$ref/additionalProperties/nested-map) without a per-language schema validator."
- acceptance: Flag on -> compiling per-service Rust MCP server (cargo build clean); flag off -> byte-identical baseline. test-gen-rust-mcp + test-rust-mcp green exercising initialize/tools-list/tools-call + error; at tools/list each returned inputSchema parses as well-formed JSON through serde_json AND is structurally equal to the T7 reference inputSchema (per K1); negative control fails. JVM codegen-shape assertion present.
- suggestedModel: standard
- dependsOn: ["T11"]
- ledgerRefs: ["goals:G1"]
- resultCommit: 1224f1fdc1230adcc92e0ebbdae37e4e015061d8
- completion: "Rust MCP generator (RsMcpServerGenerator + baboon_mcp_server.rs, serde reuse; RsBaboonTranslator emits MCP into module pass) behind --rs-generate-mcp-server, mirrors T8/T10/K5; overlay test/rust-stub-mcp-overlay + test-gen-rust-mcp/test-rust-mcp (in :test); K1 real serde_json::Value structural equality vs T7 §2.3 refs (cargo 12/12, -D warnings); flag-off inert."
- sessionLogs: ["docs/logs/20260604-223038-a8f70596e4f947d08.md","docs/logs/20260604-224034-aed725b66bef6d1e4.md","docs/logs/20260604-225606-abe0566f6578ac4e8.md","docs/logs/20260604-225919-a91a6e21f143ab4ea.md"]

### T14 — done

- createdAt: 2026-06-04T18:05:43.923Z
- updatedAt: 2026-06-04T23:01:53.727Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: Kotlin MCP server generator + overlay + test-gen-kotlin-mcp/test-kotlin-mcp
- description: "Replicate the reference MCP generator to Kotlin behind --kotlin-generate-mcp-server: per-service transport-abstract dispatch entrypoint per T3, tool registry, inputSchema from T5, Jackson JSON codec reuse, additive runtime types. Mind Kotlin generic-placement gotchas from the wiring work and the local Kotlin-daemon OOM (use mdl --seq when needed). Add test/kotlin-stub-mcp-overlay/ + test-gen-kotlin-mcp (Kotlin regular-adt flags + flag on) + test-kotlin-mcp (build+run round-trip) in .mdl/defs/tests.md; injected fake transport; unconditional throws (no -ea reliance); negative control. VALIDITY GATE (per locked decision K1 resolving review R2): no full JSON-Schema validator is required for this replica; instead at tools/list the round-trip MUST (a) assert each returned tool inputSchema parses as well-formed JSON through the Kotlin/Jackson codec, and (b) assert the parsed inputSchema is STRUCTURALLY EQUAL to the reference inputSchema expectation from the T7 language-neutral scenario (already validated well-formed at T5/T9/T11). This catches Jackson codec-rendering divergence (key escaping, number/$ref/additionalProperties/nested-map) without a per-language schema validator."
- acceptance: Flag on -> compiling per-service Kotlin MCP server; flag off -> byte-identical baseline. test-gen-kotlin-mcp + test-kotlin-mcp green exercising initialize/tools-list/tools-call + error; at tools/list each returned inputSchema parses as well-formed JSON through Jackson AND is structurally equal to the T7 reference inputSchema (per K1); negative control fails. JVM codegen-shape assertion present.
- suggestedModel: standard
- dependsOn: ["T11"]
- ledgerRefs: ["goals:G1"]
- resultCommit: 67e7eae9834de36fddb74138fa53850486b27895
- completion: "Kotlin MCP generator (KtMcpServerGenerator + BaboonMcpRuntime.kt, Jackson/kotlinx reuse) behind --kt-generate-mcp-server, mirrors T8/T10/K5; overlay test/kotlin-stub-mcp-overlay + test-gen-kotlin-mcp/test-kotlin-mcp (in :test); K1 real kotlinx JsonElement structural equality vs T7 §2.3 refs (gradle 11/11); flag-off byte-identical."
- sessionLogs: ["docs/logs/20260604-223038-a0b38a5b81780930f.md","docs/logs/20260604-224034-a37afe2ff57235d12.md","docs/logs/20260604-225606-a11c98bf42d659334.md","docs/logs/20260604-225919-a5b6dba22ba511da9.md"]

### T15 — done

- createdAt: 2026-06-04T18:05:50.048Z
- updatedAt: 2026-06-05T00:19:29.241Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: Java MCP server generator + overlay + test-gen-java-mcp/test-java-mcp
- description: "Replicate the reference MCP generator to Java behind --java-generate-mcp-server: per-service transport-abstract dispatch entrypoint per T3, tool registry, inputSchema from T5, Jackson JSON codec reuse, additive runtime types. CRITICAL gotcha: the codegen action MUST include --jv-write-evolution-dict=true (and the rest of Java regular-adt flags verbatim) or the stub fails to compile. Watch the 64KB JVM constant limit (recent split precedent) for large generated methods. Add test/java-stub-mcp-overlay/ + test-gen-java-mcp + test-java-mcp in .mdl/defs/tests.md; injected fake transport; unconditional throws (no -ea reliance); negative control. VALIDITY GATE (per locked decision K1 resolving review R2): no full JSON-Schema validator is required for this replica; instead at tools/list the round-trip MUST (a) assert each returned tool inputSchema parses as well-formed JSON through the Java/Jackson codec, and (b) assert the parsed inputSchema is STRUCTURALLY EQUAL to the reference inputSchema expectation from the T7 language-neutral scenario (already validated well-formed at T5/T9/T11). This catches Jackson codec-rendering divergence (key escaping, number/$ref/additionalProperties/nested-map) without a per-language schema validator."
- acceptance: Flag on -> compiling per-service Java MCP server (no 64KB-constant violation); flag off -> byte-identical baseline. test-gen-java-mcp (with --jv-write-evolution-dict) + test-java-mcp green exercising initialize/tools-list/tools-call + error; at tools/list each returned inputSchema parses as well-formed JSON through Jackson AND is structurally equal to the T7 reference inputSchema (per K1); negative control fails. JVM codegen-shape assertion present.
- suggestedModel: standard
- dependsOn: ["T11"]
- ledgerRefs: ["goals:G1"]
- resultCommit: 51e99ad0702a54eaeece5fca0114c4a818f5535f
- completion: "Java MCP generator (JvMcpServerGenerator + 10 java runtime files, Jackson reuse) behind --java-generate-mcp-server, mirrors T8/T10/K5; overlay test/java-stub-mcp-overlay (Maven) + test-gen-java-mcp(--jv-write-evolution-dict)/test-java-mcp (in :test); K1 real Jackson JsonNode structural equality vs T7 §2.3 refs (Maven 11/11); flag-off byte-identical; no 64KB violation."
- sessionLogs: ["docs/logs/20260605-000303-a556d7b9a8147e33a.md","docs/logs/20260605-001643-a524651f2843048c7.md"]

### T16 — done

- createdAt: 2026-06-04T18:05:55.211Z
- updatedAt: 2026-06-05T00:19:32.815Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: Dart MCP server generator + overlay + test-gen-dart-mcp/test-dart-mcp
- description: "Replicate the reference MCP generator to Dart behind --dart-generate-mcp-server: per-service transport-abstract dispatch entrypoint per T3, tool registry, inputSchema from T5, dart:convert JSON codec reuse, additive runtime types. Gotcha: Dart needs the regular-adt post-codegen step (mv runtime files into packages/baboon_runtime/lib/). Add test/dart-stub-mcp-overlay/ + test-gen-dart-mcp + test-dart-mcp in .mdl/defs/tests.md; injected fake transport; unconditional throws (asserts vacuous without --enable-asserts); negative control. VALIDITY GATE (per locked decision K1 resolving review R2): no full JSON-Schema validator is required for this replica (Dart lacks a first-class stable JSON-Schema validator -- a key driver of K1); instead at tools/list the round-trip MUST (a) assert each returned tool inputSchema parses as well-formed JSON through the dart:convert codec, and (b) assert the parsed inputSchema is STRUCTURALLY EQUAL to the reference inputSchema expectation from the T7 language-neutral scenario (already validated well-formed at T5/T9/T11). This catches dart:convert codec-rendering divergence (key escaping, number/$ref/additionalProperties/nested-map) without a per-language schema validator."
- acceptance: "Flag on -> compiling per-service Dart MCP server (dart analyze/build clean, runtime files relocated); flag off -> byte-identical baseline. test-gen-dart-mcp + test-dart-mcp green exercising initialize/tools-list/tools-call + error; at tools/list each returned inputSchema parses as well-formed JSON through dart:convert AND is structurally equal to the T7 reference inputSchema (per K1); negative control fails. JVM codegen-shape assertion present."
- suggestedModel: standard
- dependsOn: ["T11"]
- ledgerRefs: ["goals:G1"]
- resultCommit: 157eb8e019033e0b6ea63ad8a386ddb82aa2bbc6
- completion: "Dart MCP generator (DtMcpServerGenerator + baboon_mcp_runtime.dart, dart:convert reuse) behind --dt-generate-mcp-server, mirrors T8/T10/K5; overlay test/dart-stub-mcp-overlay + test-gen-dart-mcp/test-dart-mcp (in :test); K1 real recursive structural equality vs T7 §2.3 refs (SetEquality for required; dart test 11/11, analyze clean); flag-off byte-identical."
- sessionLogs: ["docs/logs/20260605-000303-a9952cbf4d9f65ce0.md","docs/logs/20260605-001643-a77ec6e66cd4bc090.md"]

### T17 — planned

- createdAt: 2026-06-04T18:06:01.777Z
- updatedAt: 2026-06-04T18:16:50.128Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: Swift MCP server generator + overlay + test-gen-swift-mcp/test-swift-mcp
- description: "Replicate the reference MCP generator to Swift behind --swift-generate-mcp-server: per-service transport-abstract dispatch entrypoint per T3, tool registry, inputSchema from T5, JSONSerialization JSON codec reuse, additive runtime types. Swift is the hardest generic case (abstract-context precedent used protocol associatedtype Ctx + AnyJsonService<R>-style type-erasers); apply the same pattern for the MCP dispatch generics. Gotcha: Swift regular-adt post-codegen fans out CrossLanguageFixturePath.swift into each Tests/BaboonTests/<Module>/. Add test/swift-stub-mcp-overlay/ + test-gen-swift-mcp + test-swift-mcp in .mdl/defs/tests.md; injected fake transport; unconditional throws (Swift release asserts vacuous, use precondition); negative control. VALIDITY GATE (per locked decision K1 resolving review R2): no full JSON-Schema validator is required for this replica (Swift lacks a first-class stable JSON-Schema validator -- a key driver of K1); instead at tools/list the round-trip MUST (a) assert each returned tool inputSchema parses as well-formed JSON through the JSONSerialization codec, and (b) assert the parsed inputSchema is STRUCTURALLY EQUAL to the reference inputSchema expectation from the T7 language-neutral scenario (already validated well-formed at T5/T9/T11). This catches JSONSerialization codec-rendering divergence (key escaping, number/$ref/additionalProperties/nested-map) without a per-language schema validator."
- acceptance: Flag on -> compiling per-service Swift MCP server (swiftc -typecheck / swift build clean, fixtures fanned out); flag off -> byte-identical baseline. test-gen-swift-mcp + test-swift-mcp green exercising initialize/tools-list/tools-call + error; at tools/list each returned inputSchema parses as well-formed JSON through JSONSerialization AND is structurally equal to the T7 reference inputSchema (per K1); negative control fails. JVM codegen-shape assertion present.
- suggestedModel: frontier
- dependsOn: ["T11"]
- ledgerRefs: ["goals:G1"]

### T18 — wip

- createdAt: 2026-06-04T18:06:06.027Z
- updatedAt: 2026-06-04T23:03:29.375Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: Python MCP server generator + overlay + test-gen-python-mcp/test-python-mcp
- description: "Replicate the reference MCP generator to Python behind --python-generate-mcp-server: per-service transport-abstract dispatch entrypoint per T3, tool registry, inputSchema from T5, the custom Python JSON codec reuse, additive runtime types. Gotcha: ensure Generic/TypeVar imports are present (abstract-context precedent missed them). Add test/py-stub-mcp-overlay/ + test-gen-python-mcp + test-python-mcp in .mdl/defs/tests.md; injected fake transport; unconditional raises; negative control. VALIDITY GATE (per locked decision K1 resolving review R2): no full JSON-Schema validator is required for this replica; instead at tools/list the round-trip MUST (a) assert each returned tool inputSchema parses as well-formed JSON through the Python json codec, and (b) assert the parsed inputSchema is STRUCTURALLY EQUAL to the reference inputSchema expectation from the T7 language-neutral scenario (already validated well-formed at T5/T9/T11). This catches Python-codec rendering divergence (key escaping, number/$ref/additionalProperties/nested-map) without a per-language schema validator."
- acceptance: Flag on -> py_compile-clean per-service Python MCP server (correct Generic/TypeVar imports); flag off -> byte-identical baseline. test-gen-python-mcp + test-python-mcp green exercising initialize/tools-list/tools-call + error; at tools/list each returned inputSchema parses as well-formed JSON through the Python json codec AND is structurally equal to the T7 reference inputSchema (per K1); negative control fails. JVM codegen-shape assertion present.
- suggestedModel: standard
- dependsOn: ["T11"]
- ledgerRefs: ["goals:G1"]

## M5

### T19 — planned

- createdAt: 2026-06-04T18:06:27.736Z
- updatedAt: 2026-06-04T18:06:27.736Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: Register all 9 test-gen-<lang>-mcp + test-<lang>-mcp actions in the `test` aggregator
- description: "Per Q5, dep-register all 9 pairs of MCP actions (ts, cs, scala, rust, kotlin, java, dart, swift, python) into the `test` aggregator target in .mdl/defs/tests.md so the MCP round-trips run as part of `mdl :test` and CI. Mirror exactly how the test-gen-<lang>-wiring*/test-<lang>-wiring* actions are registered. Verify the aggregator picks them up (mdl lists/runs them) and that the existing matrix is unaffected."
- acceptance: "`mdl :test` (and the CI `test` aggregator) runs all 9 test-<lang>-mcp lanes; all green. The pre-existing test matrix (regular/wrapped/wiring/acceptance) still passes unchanged. Each MCP lane is dep-wired identically to the wiring lanes."
- suggestedModel: standard
- dependsOn: ["T9","T11","T12","T13","T14","T15","T16","T17","T18"]
- ledgerRefs: ["goals:G1"]

### T20 — planned

- createdAt: 2026-06-04T18:06:31.271Z
- updatedAt: 2026-06-04T18:06:31.271Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: Expose --<lang>-generate-mcp-server in the playground (all 9)
- description: "Per Q3 and the standing preference to surface new flags in the playground: add the MCP-server checkbox to baboon-playground/src/options.ts (a { kind: 'checkbox', key, label, description, langs: [all 9] } entry) and bridge the field into the Scala.js call in compiler.ts, matching the existing per-language service-flag exposure. The key/field naming must match the BaboonJS.scala bridge field added in T6."
- acceptance: The playground shows a 'generate MCP server' checkbox for all 9 languages; toggling it on causes the playground compile to emit MCP output (and off emits none); the bridge field name matches the BaboonJS.scala option. Playground builds clean.
- suggestedModel: standard
- dependsOn: ["T6","T9","T11","T12","T13","T14","T15","T16","T17","T18"]
- ledgerRefs: ["goals:G1"]

### T21 — planned

- createdAt: 2026-06-04T18:06:42.482Z
- updatedAt: 2026-06-04T18:06:42.482Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- headline: "Full-pipeline CI closeout: mdl :build :test + :ci green, off-by-default verified"
- description: "Final verification pass: run the full CI-equivalent pipeline (`mdl :build :test` then `mdl :ci`, incl. sbt +compile cross-build and the native-image build) and prove the whole MCP feature is green end-to-end across all 9 backends. Explicitly verify off-by-default: with no MCP flags, a full codegen run produces output byte-identical to the pre-feature baseline (the NoMcp path is inert). Confirm the 64KB-constant and Kotlin-OOM hazards did not regress. Update CLAUDE.md / docs if a new flag-site or harness pattern was introduced."
- acceptance: "`mdl :build :test` and `mdl :ci` both green on a clean run (cross-build + native image included); the MCP test lanes all pass; with all MCP flags off the generated output is byte-identical to baseline (diff empty); no regression in the existing regular/wrapped/wiring/service-acceptance matrices. Docs updated if a new pattern was added."
- suggestedModel: standard
- dependsOn: ["T19","T20"]
- ledgerRefs: ["goals:G1"]
