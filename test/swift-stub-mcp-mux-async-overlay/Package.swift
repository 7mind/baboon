// swift-tools-version: 5.9
import PackageDescription

// T113 — Swift ASYNC MCP muxer round-trip overlay package.
//
// Async sibling of `test/swift-stub-mcp-mux-overlay/Package.swift`. Built by the
// `test-gen-swift-mcp-mux-async` mdl action over the `mcp-mux-stub-ok` model
// (UserService + OrderService) with BOTH `--sw-generate-mcp-server=true` AND
// `--sw-async-services=true` (no-errors service-result mode). Under the async
// axis the generated `<Svc>Wiring.invokeJson` is `async throws -> String` and the
// generated MCP servers conform to `IBaboonAsyncMcpServer`, so the test composes
// them behind `AbstractAsyncMcpMuxer<Ctx>` (async `handle`, `async throws`
// `routeToolCall`) via the public T114 async routable surface
// (`AnyAsyncRoutableMcpServer<Ctx>`).
let package = Package(
    name: "BaboonSwiftMcpMuxAsyncTests",
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
