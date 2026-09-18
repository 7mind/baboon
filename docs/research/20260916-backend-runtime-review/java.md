# Java backend and runtime audit

Baseline: `69bbb4c48a519d9a4148f370b76bde19af4bac6a`. Read-only review. Full inspection: all 16 generator files and 59 embedded runtime files, 9,457 lines. Abbreviations below expand to repository-relative paths: **G/** = `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/java/`; **R/** = `baboon-compiler/src/main/resources/baboon-runtime/java/`. C# comparisons refer to its sibling translator directory.

## Ranked findings

### JV-01 — Give fixed MCP metadata a construction-time lifetime

Observed: `G/JvMcpServerGenerator.scala:127` puts `parseSchema(...)` inside every tool-entry expression; generated `tools()` constructs the list afresh (`:173`). `R/AbstractBaboonMcpServer.java:42` calls that accessor and constructs a lookup map on every `tools/call` (`:106`). Thus a single call reparses schemas for all tools. The muxer already builds its routing tables during registration (`R/AbstractMcpMuxer.java:63`).

Generate instance-owned metadata once and build the per-server lookup once, after construction—not by invoking an overridable accessor in a base constructor. Preserve declaration order and define ownership of mutable Jackson schema nodes: exposing cached nodes directly changes the current fresh-value behavior. Defensive copying at the public accessor or an internal immutable registry seam may be necessary. This removes repeated parsing/construction, but throughput savings are unmeasured. Effort M; risk medium; confidence high. Verify repeated `tools/list`/`tools/call`, mutation isolation, duplicate names, notifications, session isolation, and Channel-A/B responses through public dispatch.

### JV-02 — Stop constructing JSON parsers per RPC invocation

Observed: six emitted parse sites construct `ObjectMapper`: client sync/async (`G/JvServiceWiringTranslator.scala:428`, `:446`), no-errors server sync/async (`:604`, `:625`), and errors server sync/async (`:772`, `:881`). The facade instead retains a parser (`R/BaboonCodecsFacade.java:53`, used at `:347`).

Provide a narrow runtime parse operation backed by a configured, encapsulated reusable reader; have generated call sites invoke it. Prefer an explicitly owned dependency where configurability is required; do not expose another mutable global mapper or add unnecessary configuration options. Preserve Jackson defaults and the existing decode-error boundary. Benefit: one maintained parsing policy and removal of mapper construction from each request; latency/allocation improvement requires measurement. Effort M; risk medium; confidence high. Verify identical JSON trees and DecoderFailed classification for malformed input in all six paths, then benchmark allocations separately.

### JV-03 — Centralize scalar wire expressions within the Java backend

Observed: service wiring maintains four scalar tables (`G/JvServiceWiringTranslator.scala:75`, `:101`, `:127`, `:152`), independently of DTO JSON encode/decode (`G/JvJsonCodecGenerator.scala:208`, `:333`) and UEBA encode/decode (`G/JvUEBACodecGenerator.scala:404`, `:467`). These include unsigned carriers, decimal, UUID, timestamp, and bytes policy, not just formatting.

Extract Java-specific scalar codec operations taking typed scalar IDs, codec context, and input/output expression trees. Both service and DTO emitters consume them; collection traversal and JSON map-key string conversion remain separate. This improves maintained-source consistency without requiring runtime interpretation or reducing every emitted expression. Effort M; risk medium; confidence high. Compile generated scalar service signatures and DTOs, and compare JSON/UEBA golden bytes for every scalar, particularly u64, decimal limits, GUID order, and timestamp offsets.

### JV-04 — Represent service invocation stages explicitly

Observed: JSON errors-mode generation (`G/JvServiceWiringTranslator.scala:758`, `:871`) and UEBA counterparts (`:948`, `:1057`) independently implement decode/call/encode failure stages, output-present/output-absent cases, declared errors, and async composition. `bindOutputStep` (`:938`) already demonstrates a useful local extraction.

Introduce a small typed Java method plan for input/output/error shape and resolved context/result modes; use format-specific decode/encode operations around shared invocation-stage emission. Keep sync and CompletableFuture composition explicit: Java checked exceptions, generic inference, and completion failures are real distinctions. Do not replace these with a universal template engine. Benefit: changes to invocation policy have fewer independent branches; emitted code volume need not change. Effort M–L; risk high; confidence high. Verify the cross-product of sync/async, void/value, declared/no declared error, and context modes, including synchronously thrown implementations and exceptionally completed futures. No correctness defect was reproduced here.

### JV-05 — Move schema-independent `any` framing out of each generated codec

Observed: JSON helpers (`G/JvJsonCodecGenerator.scala:560`) and binary helpers (`G/JvUEBACodecGenerator.scala:613`) emit entire envelope algorithms into each any-bearing codec. The generator separately duplicates any detection and static-fallback planning (`G/JvJsonCodecGenerator.scala:496`, `:536`; `G/JvUEBACodecGenerator.scala:546`, `:586`). Runtime metadata/framing support already lives in `R/BaboonAnyOpaque.java:110`.

Move format-specific field-envelope helpers into runtime support, accepting expected kind and a named static-resolution descriptor. Compute that descriptor once in a small shared semantic helper; keep JSON and binary algorithms distinct. This reduces emitted code proportional to any-bearing codec count and separates wire policy from source rendering. It is not a measured speed improvement. Effort M; risk high; confidence high. Verify all six kinds, nested collections, wire-over-static precedence, cross-format conversion, truncated blobs, metadata extensions, and identical bytes/error categories. Add a separate resource if necessary; do not grow embedded files toward the JVM string-constant limit.

### JV-06 — Eliminate discarded UEBA materialization

Observed: `R/BaboonBinCodec.java:61` allocates an ArrayList plus one `int[2]` per index entry. Generated DTO decoding uses only its size (`G/JvUEBACodecGenerator.scala:344`). Indexed encoding copies its complete temporary payload through `toByteArray()` (`:299`), although `R/LEDataOutputStream.java:7` is an OutputStream and supports bulk writes (`:17`).

Add a consume-index/count path for generated readers while retaining the public materializing API. Read the same integers so truncation behavior remains unchanged. Use the buffer's `writeTo(output)` for indexed payload transfer instead of an intermediate copied array; do not remove buffering needed to compute offsets. Benefit: explicit allocations/copy removed; actual impact depends on payloads. Effort S–M; risk medium; confidence high. Verify compact/indexed golden bytes, stream position, truncated indices, assertions enabled/disabled, and old public readIndex users; measure bytes allocated by payload/index size.

### JV-07 — Consolidate reflective metadata discovery

Observed: `R/BaboonExt.java:20`, `:31` and `R/BaboonTypeMeta.java:86`, `:129`, `:138` repeat reflective static-field retrieval and diagnostics. Conversion independently retrieves domain fields (`R/BaboonCodecsFacade.java:548`). These paths rediscover class metadata on repeated operations.

Introduce an internal typed metadata accessor shared by these three consumers. First deduplicate retrieval without caching to preserve evaluation behavior; then assess class-scoped caching for generated immutable metadata. Keep declared-type-dependent ADT selection outside the cached descriptor. Consider class-loader lifetime and externally supplied mutable metadata before caching values. Benefit: one validation boundary and potentially fewer reflective lookups, not a proven throughput gain. Effort M; risk medium; confidence high. Verify missing/wrong-shaped fields, branch-versus-ADT envelopes, min-reader tiers, user-provided implementations, and class-loader isolation.

### JV-08 — Share identifier classification, not target-language parsers

Observed: `G/JvDefnTranslator.scala:669` defines the same identifier-kind algebra/classification as `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/csharp/CSDefnTranslator.scala:722`. Java then interprets it in formatting (`:707`) and parsing (`:837`). This classification describes model semantics rather than Java syntax.

Move only the named kind/classification and, where justified, numeric-domain facts into shared translation support. Java owns unsigned-long operations, narrowing, parser diagnostics, and expression emission. Benefit: fewer semantic tables to extend when identifier support changes; no claimed emitted-size reduction. Effort M; risk medium; confidence high. Verify identifier repr round trips and canonical strings for every field kind, nested IDs, escaping, numeric extremes, and malformed input across participating backends.

## Sequence, limits, and cleanup

Start with JV-02/06 and local metadata extraction, then JV-03/08, followed by JV-05 and the higher-risk service/MCP changes. Keep performance benchmarks distinct from public-behavior correctness checks; the constructive-test-taxonomy skill informed that separation.

The repeated domain orchestration at `G/JvBaboonTranslator.scala:133` is not sufficient reason for a renderer framework. Java's one-public-class-per-file layout, primitive boxing, checked exceptions, nested service wrappers, and MCP-only resource gating are intentional boundaries. Likewise, identifier hex parsing has stricter canonical-input requirements than general ByteString parsing; do not merge them merely because both decode hex. JSON/UEBA envelopes and context/no-context public interfaces are not interchangeable API surface.

Small verified cleanup candidate: `G/JvDefnTranslator.scala:636` private `collectContractFieldNames` has no callers besides its own recursion (`:641`), established by `git grep -n collectContractFieldNames -- '*.scala'` across tracked Scala files, including hidden platform directories. Remove only after normal compiler checks; this is not a meaningful optimization project.

Actual checks: complete source reads using sed/nl, rg reference/call-site searches, inventory/line counts, resource byte-size check (facade 38,312 bytes), and clean `git status --short`. No builds, generated compilations, behavior tests, or benchmarks were run. All verification above is proposed, not reported as passing.

## Coverage appendix

Full-read G/ (all `.scala`): JvBaboonTranslator, JvCodecFixtureTranslator, JvCodecTestsTranslator, JvCodecTranslator, JvConversionTranslator, JvDefnTranslator, JvDomainTreeTools, JvFileTools, JvJsonCodecGenerator, JvMcpServerGenerator, JvServiceWiringTranslator, JvTreeTools, JvTypeTranslator, JvTypes, JvUEBACodecGenerator, JvValue.

Full-read R/ (all `.java`): AbstractBaboonCodecs, AbstractBaboonConversions, AbstractBaboonJsonCodecs, AbstractBaboonMcpServer, AbstractBaboonUebaCodecs, AbstractConversion, AbstractMcpMuxer, BaboonAdtMemberMeta, BaboonAnyOpaque, BaboonBinCodec, BaboonBinCodecIndexed, BaboonBinTools, BaboonClientTransport, BaboonCodecContext, BaboonCodecData, BaboonCodecException, BaboonCodecsFacade, BaboonDomainVersion, BaboonEither, BaboonException, BaboonExt, BaboonGenerated, BaboonGeneratedLatest, BaboonIdentifierRepr, BaboonJsonCodec, BaboonMcpWiringError, BaboonMcpWiringException, BaboonMeta, BaboonMethodId, BaboonRandom, BaboonRandomFactory, BaboonRandomImpl, BaboonTimeFormats, BaboonTypeMeta, BaboonVersion, BaboonWiringError, BaboonWiringException, ByteString, IBaboonJsonService, IBaboonJsonServiceCtx, IBaboonMcpServer, IBaboonRoutableMcpServer, IBaboonUebaService, IBaboonUebaServiceCtx, JsonMuxer, JsonMuxerCtx, JsonRpcError, JsonRpcRequest, JsonRpcResponse, LEDataInputStream, LEDataOutputStream, Lazy, McpJsonInvoke, McpProtocol, McpServerInfo, McpSession, McpToolEntry, UebaMuxer, UebaMuxerCtx.

Truncated initial batches were repaired by narrower reads. Supporting coverage: CLAUDE.md full; C# generator fully read in the preceding assignment; other backends only search matches. No claim of Java test-suite inspection or generated-output coverage.
