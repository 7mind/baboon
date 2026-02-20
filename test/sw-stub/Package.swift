// swift-tools-version: 5.9
import PackageDescription

let package = Package(
    name: "BaboonSwiftTests",
    platforms: [.macOS(.v13)],
    targets: [
        .target(
            name: "BaboonRuntime",
            path: "Sources/BaboonRuntime"
        ),
        .target(
            name: "TestpkgPkg0_v1_0_0",
            dependencies: ["BaboonRuntime"],
            path: "Sources/TestpkgPkg0_v1_0_0"
        ),
        .target(
            name: "TestpkgPkg0_v2_0_0",
            dependencies: ["BaboonRuntime", "TestpkgPkg0_v1_0_0"],
            path: "Sources/TestpkgPkg0_v2_0_0"
        ),
        .target(
            name: "TestpkgPkg0",
            dependencies: ["BaboonRuntime", "TestpkgPkg0_v1_0_0", "TestpkgPkg0_v2_0_0"],
            path: "Sources/TestpkgPkg0"
        ),
        .target(
            name: "RenameNs_v1_0_0",
            dependencies: ["BaboonRuntime"],
            path: "Sources/RenameNs_v1_0_0"
        ),
        .target(
            name: "RenameNs",
            dependencies: ["BaboonRuntime", "RenameNs_v1_0_0"],
            path: "Sources/RenameNs"
        ),
        .testTarget(
            name: "BaboonTests",
            dependencies: [
                "BaboonRuntime",
                "TestpkgPkg0",
                "TestpkgPkg0_v1_0_0",
                "TestpkgPkg0_v2_0_0",
                "RenameNs",
                "RenameNs_v1_0_0",
            ],
            path: "Tests/BaboonTests"
        ),
    ]
)
