// swift-tools-version: 5.9
import PackageDescription

// T66 — Swift ASYNC-MCP round-trip overlay package (D24/G11).
//
// Async sibling of `test/swift-stub-mcp-overlay/Package.swift`. Self-contained
// SPM package built by the `test-gen-swift-mcp-async` mdl action over the
// `mcp-stub-ok` model with BOTH `--sw-generate-mcp-server=true` AND
// `--sw-async-services=true` (no-errors service-result mode). Same three-target
// layout as the sync lane:
//   - BaboonRuntime: the generated Swift runtime (includes the additive
//     baboon_mcp_runtime.swift emitted only when the MCP flag is on).
//   - McpStub: the generated mcp.stub definitions + JSON/UEBA codecs + service
//     wiring + the per-service McpToolsMcpServer.
//   - McpTests: the hand-written round-trip suite driving the generated server
//     through the canonical T7 scenario via an in-process delegate (no I/O).
//
// GREEN (D24/T67): under `--sw-async-services=true` the generated
// `McpToolsWiring.invokeJson` dispatcher is `async throws -> String`
// (SwServiceWiringTranslator.scala:43/508-513), and the generated
// `McpToolsMcpServer.init` now accepts an `async throws -> String` delegate
// closure conforming to `IBaboonAsyncMcpServer`, whose `handle` is genuinely
// `async`. So `swift build`/`swift test` of this overlay drives a real
// `tools/call` round-trip (from both a non-isolated and a @MainActor caller).
// T66 added this lane RED to gate the fix; T67 turns it green. The sync
// `test-swift-mcp` lane is untouched and still passes.
let package = Package(
    name: "BaboonSwiftMcpAsyncTests",
    platforms: [.macOS(.v13)],
    targets: [
        .target(
            name: "BaboonRuntime",
            path: "Sources/BaboonRuntime"
        ),
        .target(
            name: "McpStub",
            dependencies: ["BaboonRuntime"],
            path: "Sources/McpStub"
        ),
        .testTarget(
            name: "McpTests",
            dependencies: ["BaboonRuntime", "McpStub"],
            path: "Tests/McpTests"
        ),
    ]
)
