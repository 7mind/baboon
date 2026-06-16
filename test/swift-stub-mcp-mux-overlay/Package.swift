// swift-tools-version: 5.9
import PackageDescription

// T113 — Swift MCP muxer round-trip overlay package (sync).
//
// Self-contained SPM package built by the `test-gen-swift-mcp-mux` mdl action
// over the `mcp-mux-stub-ok` model (UserService + OrderService) with
// `--sw-generate-mcp-server=true` (no-errors service-result mode). Three targets:
//   - BaboonRuntime: the generated Swift runtime (includes the additive
//     baboon_mcp_runtime.swift — emitted only when the MCP flag is on — which
//     carries AbstractMcpMuxer + the DuplicateTool/NoMatchingTool taxonomy).
//   - McpMuxStub: the generated mcp.mux.stub definitions + JSON/UEBA codecs +
//     service wiring + the per-service UserServiceMcpServer / OrderServiceMcpServer.
//   - McpMuxTests: the hand-written muxer round-trip suite composing the two
//     generated servers behind one AbstractMcpMuxer via the public T114 routable
//     surface (no I/O, no subclassing, never a member's handle()).
let package = Package(
    name: "BaboonSwiftMcpMuxTests",
    platforms: [.macOS(.v13)],
    targets: [
        .target(
            name: "BaboonRuntime",
            path: "Sources/BaboonRuntime"
        ),
        .target(
            name: "McpMuxStub",
            dependencies: ["BaboonRuntime"],
            path: "Sources/McpMuxStub"
        ),
        .testTarget(
            name: "McpMuxTests",
            dependencies: ["BaboonRuntime", "McpMuxStub"],
            path: "Tests/McpMuxTests"
        ),
    ]
)
