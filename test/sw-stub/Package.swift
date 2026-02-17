// swift-tools-version: 5.9
import PackageDescription

let package = Package(
    name: "BaboonSwiftTests",
    platforms: [.macOS(.v13)],
    targets: [
        .target(
            name: "BaboonGenerated",
            path: "Sources/BaboonGenerated"
        ),
        .testTarget(
            name: "BaboonTests",
            dependencies: ["BaboonGenerated"],
            path: "Tests/BaboonTests"
        ),
    ]
)
