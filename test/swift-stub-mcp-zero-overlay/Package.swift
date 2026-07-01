// swift-tools-version: 5.9
import PackageDescription

// T178 / D40 — Swift ZERO-SERVICE MCP overlay package (RED baseline).
//
// Self-contained SPM package built by the `test-swift-mcp-zero` mdl action over
// the `mcp-stub-zero-services-ok` model (declares @root types, NO RPC block)
// with `--sw-generate-mcp-server=true` (no-errors service-result mode). Targets:
//   - BaboonRuntime: the generated Swift runtime. With zero services and the
//     current (pre-fix) generator this does NOT contain baboon_mcp_runtime.swift
//     — that omission is the D40 RED baseline.
//   - McpStubZero: the generated mcp.stub.zero definitions + codecs (no service
//     wiring, no per-service server — the model has zero endpoints).
//   - McpZeroTests: the hand-written zero-service suite. It references ONLY the
//     static MCP runtime types (AbstractMcpMuxer etc.) that live in
//     baboon_mcp_runtime.swift, so PRE-FIX it fails to compile ("cannot find
//     'AbstractMcpMuxer' in scope").
let package = Package(
    name: "BaboonSwiftMcpZeroTests",
    platforms: [.macOS(.v13)],
    targets: [
        .target(
            name: "BaboonRuntime",
            path: "Sources/BaboonRuntime"
        ),
        .target(
            name: "McpStubZero",
            dependencies: ["BaboonRuntime"],
            path: "Sources/McpStubZero"
        ),
        .testTarget(
            name: "McpZeroTests",
            dependencies: ["BaboonRuntime", "McpStubZero"],
            path: "Tests/McpZeroTests"
        ),
    ]
)
