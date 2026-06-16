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
// EXPECTED RED (D24): under `--sw-async-services=true` the generated
// `McpToolsWiring.invokeJson` dispatcher is `async throws -> String`
// (SwServiceWiringTranslator.scala:43/508-513), while the generated
// `McpToolsMcpServer.init` still requires a SYNC `throws -> String` delegate
// closure (SwMcpServerGenerator.scala:125/127). The async wiring fn cannot
// satisfy that non-async closure type, so `swift build`/`swift test` of this
// overlay MUST FAIL at the `makeServer` binding in McpTests. This lane gates
// the Swift async-MCP backend fix (T67); the sync `test-swift-mcp` lane is
// untouched and still passes.
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
