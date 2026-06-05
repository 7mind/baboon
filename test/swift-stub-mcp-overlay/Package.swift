// swift-tools-version: 5.9
import PackageDescription

// T17 — Swift MCP round-trip overlay package.
//
// Self-contained SPM package built by the `test-gen-swift-mcp` mdl action over
// the `mcp-stub-ok` model with `--sw-generate-mcp-server=true` (no-errors
// service-result mode). Three targets:
//   - BaboonRuntime: the generated Swift runtime (includes the additive
//     baboon_mcp_runtime.swift emitted only when the MCP flag is on).
//   - McpStub: the generated mcp.stub definitions + JSON/UEBA codecs + service
//     wiring + the per-service McpToolsMcpServer.
//   - McpTests: the hand-written round-trip suite driving the generated server
//     through the canonical T7 scenario via an in-process delegate (no I/O).
let package = Package(
    name: "BaboonSwiftMcpTests",
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
