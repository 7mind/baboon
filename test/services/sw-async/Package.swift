// swift-tools-version: 5.9
import PackageDescription

// Async-services Swift wiring smoke package. The generation action copies the
// compiler-generated `BaboonRuntime` and `PetstoreApi` modules (built with
// `--sw-async-services=true --service-result-no-errors=true`) into
// `Sources/Generated`. `AsyncMain` exercises the generated `async throws`
// service protocol, invoke dispatchers, muxer-wrapper thunk, and client.
let package = Package(
    name: "BaboonPetStoreServiceAsync",
    platforms: [.macOS(.v13)],
    targets: [
        .target(
            name: "BaboonRuntime",
            path: "Sources/Generated/BaboonRuntime"
        ),
        .target(
            name: "Generated",
            dependencies: ["BaboonRuntime"],
            path: "Sources/Generated/PetstoreApi"
        ),
        .executableTarget(
            name: "AsyncMain",
            dependencies: ["BaboonRuntime", "Generated"],
            path: "Sources/AsyncMain"
        ),
    ]
)
