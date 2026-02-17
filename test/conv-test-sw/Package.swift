// swift-tools-version: 5.9
import PackageDescription

let package = Package(
    name: "BaboonCompatTests",
    platforms: [.macOS(.v13)],
    targets: [
        .target(
            name: "BaboonGenerated",
            path: "Sources/BaboonGenerated"
        ),
        .executableTarget(
            name: "CompatMain",
            dependencies: ["BaboonGenerated"],
            path: "Sources/CompatMain"
        ),
        .testTarget(
            name: "CrossLanguageTests",
            dependencies: ["BaboonGenerated"],
            path: "Tests/CrossLanguageTests"
        ),
    ]
)
