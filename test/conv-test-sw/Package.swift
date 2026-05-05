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
        // PR-26.5 (M26) — convtest.m26builtinkeys (non-string builtin map-key
        // cross-language fixture).
        .target(
            name: "ConvtestM26builtinkeys",
            dependencies: ["BaboonRuntime"],
            path: "Generated/ConvtestM26builtinkeys",
            swiftSettings: [
                .unsafeFlags(["-enable-testing"])
            ]
        ),
        // PR-29.10 (M29) — convtest.m29ok (monomorphised-template cross-language
        // wire-format fixture). IntPage/StrPage/ItemPage/IntStrEnvelope aliases.
        .target(
            name: "ConvtestM29ok",
            dependencies: ["BaboonRuntime"],
            path: "Generated/ConvtestM29ok",
            swiftSettings: [
                .unsafeFlags(["-enable-testing"])
            ]
        ),
        // PR-33.5 (M33) — convtest.m33ok (structural-inheritance-via-template
        // cross-language wire-format fixture). IntPageWithStats inlines
        // Page[i32] + Stats[i32] via PR-33.2 lowering.
        .target(
            name: "ConvtestM33ok",
            dependencies: ["BaboonRuntime"],
            path: "Generated/ConvtestM33ok",
            swiftSettings: [
                .unsafeFlags(["-enable-testing"])
            ]
        ),
        .executableTarget(
            name: "CompatMain",
            dependencies: ["BaboonRuntime", "ConvtestTestpkg", "ConvtestTestpkg_v1_0_0", "ConvtestM24foreign", "ConvtestM26builtinkeys", "ConvtestM29ok", "ConvtestM33ok"],
            path: "Sources/CompatMain"
        ),
        .testTarget(
            name: "CrossLanguageTests",
            dependencies: ["BaboonRuntime", "ConvtestTestpkg", "ConvtestTestpkg_v1_0_0", "ConvtestM24foreign", "ConvtestM26builtinkeys", "ConvtestM29ok", "ConvtestM33ok"],
            path: "Tests/CrossLanguageTests"
        ),
    ]
)
