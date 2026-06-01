// swift-tools-version: 5.9
import PackageDescription

// Errors-mode Swift wiring smoke package. The generation action copies the
// compiler-generated `BaboonRuntime` and `PetstoreApi` modules (built with
// `--service-result-no-errors=false --service-result-type=BaboonEither
// --service-result-pattern=<$error,$success>`) into `Sources/Generated`.
// `ErrorsMain` exercises the generated err-carrying service protocol (which
// returns the result CONTAINER `BaboonEither<Err, Out>`), the invoke
// dispatchers, the muxer-wrapper and the client — the bar is compile/link.
let package = Package(
    name: "BaboonPetStoreServiceErrors",
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
            name: "ErrorsMain",
            dependencies: ["BaboonRuntime", "Generated"],
            path: "Sources/ErrorsMain"
        ),
    ]
)
