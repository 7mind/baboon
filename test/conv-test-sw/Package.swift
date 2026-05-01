// swift-tools-version: 5.9
import PackageDescription

let package = Package(
    name: "BaboonCompatTests",
    platforms: [.macOS(.v13)],
    targets: [
        .target(
            name: "BaboonRuntime",
            path: "Generated/BaboonRuntime"
        ),
        .target(
            name: "ConvtestTestpkg_v1_0_0",
            dependencies: ["BaboonRuntime"],
            path: "Generated/ConvtestTestpkg_v1_0_0"
        ),
        .target(
            name: "ConvtestTestpkg",
            dependencies: ["BaboonRuntime", "ConvtestTestpkg_v1_0_0"],
            path: "Generated/ConvtestTestpkg",
            swiftSettings: [
                .unsafeFlags(["-enable-testing"])
            ]
        ),
        // PR-I.2 (M24 Phase 3.2) — convtest.m24foreign (custom-foreign KeyCodec hook
        // canonical fixture). Stringy `FStr` foreign + ItemKey wrapper +
        // ForeignKeyHolder map[ItemKey, str] root.
        .target(
            name: "ConvtestM24foreign",
            dependencies: ["BaboonRuntime"],
            path: "Generated/ConvtestM24foreign",
            swiftSettings: [
                .unsafeFlags(["-enable-testing"])
            ]
        ),
        .executableTarget(
            name: "CompatMain",
            dependencies: ["BaboonRuntime", "ConvtestTestpkg", "ConvtestTestpkg_v1_0_0", "ConvtestM24foreign"],
            path: "Sources/CompatMain"
        ),
        .testTarget(
            name: "CrossLanguageTests",
            dependencies: ["BaboonRuntime", "ConvtestTestpkg", "ConvtestTestpkg_v1_0_0", "ConvtestM24foreign"],
            path: "Tests/CrossLanguageTests"
        ),
    ]
)
