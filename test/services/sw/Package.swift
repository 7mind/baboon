// swift-tools-version: 5.9
import PackageDescription

let package = Package(
    name: "BaboonPetStoreService",
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
            name: "ServiceMain",
            dependencies: ["BaboonRuntime", "Generated"],
            path: "Sources/ServiceMain"
        ),
    ]
)
