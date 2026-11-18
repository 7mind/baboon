// swift-tools-version: 5.9
import PackageDescription

// PR-68 (M23.4): Swift SPM cannot share basenames across files in a single
// flat .testTarget(); the auto-mangled .o filenames collide (`holder_test.swift`
// in MyOkM19Direct vs MyOkM19Foreign etc.) and the build fails with
// "multiple producers" before any compilation begins. Splitting into one
// .testTarget() per Tests/BaboonTests/<Module>/ subdirectory gives each
// generated test file its own Swift module, so the .o filenames live under
// disjoint module-scoped build dirs.
//
// Hand-written runtime tests live under Tests/RuntimeTests/ to keep
// Tests/BaboonTests/ purely codegen-owned (the rsync action treats it as
// regenerable).
//
// CrossLanguageFixturePath.swift, emitted by codegen at the top of
// Tests/BaboonTests/, is copied into each per-module subdirectory by the
// test-gen-{regular,wrapped}-adt mdl actions (see .mdl/defs/tests.md). Each
// per-module test target then compiles its own copy alongside the generated
// tests as if both files belonged to the same module — matching the
// codegen-emitted test files' expectation that helpers are top-level free
// functions in the current module (no `import` line).

let package = Package(
    name: "BaboonSwiftTests",
    platforms: [.macOS(.v13)],
    targets: [
        // ----- Runtime --------------------------------------------------------------------------
        .target(
            name: "BaboonRuntime",
            path: "Sources/BaboonRuntime"
        ),

        // ----- testpkg.pkg0 evolution chain (v1.0.0 → v2.0.0 → current) -------------------------
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

        // ----- rename.ns evolution chain --------------------------------------------------------
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

        // ----- fwde2e.fwd / fwde2e.chain forward-compat fixtures (docs/forward-compat.md) -------
        .target(
            name: "Fwde2eFwd_v1_0_0",
            dependencies: ["BaboonRuntime"],
            path: "Sources/Fwde2eFwd_v1_0_0"
        ),
        .target(
            name: "Fwde2eFwd",
            dependencies: ["BaboonRuntime", "Fwde2eFwd_v1_0_0"],
            path: "Sources/Fwde2eFwd"
        ),
        .target(
            name: "Fwde2eChain_v1_0_0",
            dependencies: ["BaboonRuntime"],
            path: "Sources/Fwde2eChain_v1_0_0"
        ),
        .target(
            name: "Fwde2eChain_v2_0_0",
            dependencies: ["BaboonRuntime", "Fwde2eChain_v1_0_0"],
            path: "Sources/Fwde2eChain_v2_0_0"
        ),
        .target(
            name: "Fwde2eChain",
            dependencies: ["BaboonRuntime", "Fwde2eChain_v1_0_0", "Fwde2eChain_v2_0_0"],
            path: "Sources/Fwde2eChain"
        ),
        .target(
            name: "Fwde2eRename_v1_0_0",
            dependencies: ["BaboonRuntime"],
            path: "Sources/Fwde2eRename_v1_0_0"
        ),
        .target(
            name: "Fwde2eRename",
            dependencies: ["BaboonRuntime", "Fwde2eRename_v1_0_0"],
            path: "Sources/Fwde2eRename"
        ),
        .target(
            name: "RnnestedModel_v1_0_0",
            dependencies: ["BaboonRuntime"],
            path: "Sources/RnnestedModel_v1_0_0"
        ),
        .target(
            name: "RnnestedModel",
            dependencies: ["BaboonRuntime", "RnnestedModel_v1_0_0"],
            path: "Sources/RnnestedModel"
        ),

        // ----- Single-version fixtures ----------------------------------------------------------
        .target(
            name: "MyOk",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyOk"
        ),
        .target(
            name: "IdentifierOk",
            dependencies: ["BaboonRuntime"],
            path: "Sources/IdentifierOk"
        ),
        .target(
            name: "MyOkIdcontracts",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyOkIdcontracts"
        ),
        .target(
            name: "M30ScDocs",
            dependencies: ["BaboonRuntime"],
            path: "Sources/M30ScDocs"
        ),
        .target(
            name: "MixedCasePkg",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MixedCasePkg"
        ),
        .target(
            name: "MyLspM29",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyLspM29"
        ),
        .target(
            name: "MyLspM33",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyLspM33"
        ),
        .target(
            name: "MyOkM28U64key",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyOkM28U64key"
        ),
        .target(
            name: "MyOkM29",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyOkM29"
        ),
        .target(
            name: "MyOkM30",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyOkM30"
        ),
        .target(
            name: "MyOkM31",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyOkM31"
        ),
        .target(
            name: "MyOkM33",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyOkM33"
        ),
        .target(
            name: "RecursiveContractOk",
            dependencies: ["BaboonRuntime"],
            path: "Sources/RecursiveContractOk"
        ),
        .target(
            name: "ReservedWords",
            dependencies: ["BaboonRuntime"],
            path: "Sources/ReservedWords"
        ),

        // ----- M19 (identifier-and-contract evolution) fixtures ---------------------------------
        .target(
            name: "MyOkM19Alias",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyOkM19Alias"
        ),
        .target(
            name: "MyOkM19Direct",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyOkM19Direct"
        ),
        .target(
            name: "MyOkM19Foreign",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyOkM19Foreign"
        ),
        // NOTE: Tests/BaboonTests/MyOkM19Foreign/ is not auto-generated by codegen
        // because SwCodecFixtureTranslator/SwCodecTestsTranslator exclude types that
        // transitively contain Custom-foreign types (the ItemKey.v field is FStr, a
        // Custom-foreign, and ItemKey_JsonCodec.decode / ItemKey_UebaCodec.encode/decode
        // delegate to the foreign codec's fatalError stub). The JSON round-trip for
        // Holder (which uses ItemKey only as a map key, bypassing the foreign codec
        // in field-value position) is covered by the hand-written ForeignMapKeyRoundTripTests
        // in Tests/RuntimeTests/ — see PR-68-D02.
        .target(
            name: "MyOkM19Multiid",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyOkM19Multiid"
        ),
        .target(
            name: "MyOkM19Nested",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyOkM19Nested"
        ),
        .target(
            name: "MyOkM19Singleid",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyOkM19Singleid"
        ),

        // ----- M20 (ADT branch inheritance) fixtures --------------------------------------------
        .target(
            name: "MyOkM20Chained",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyOkM20Chained"
        ),
        .target(
            name: "MyOkM20Equiv",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyOkM20Equiv"
        ),
        .target(
            name: "MyOkM20Equivmanual",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyOkM20Equivmanual"
        ),
        .target(
            name: "MyOkM20Equivsugared",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyOkM20Equivsugared"
        ),
        .target(
            name: "MyOkM20Exclude",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyOkM20Exclude"
        ),
        .target(
            name: "MyOkM20Intersect",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyOkM20Intersect"
        ),
        .target(
            name: "MyOkM20Lexical",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyOkM20Lexical"
        ),
        .target(
            name: "MyOkM20Multi_intersect",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyOkM20Multi_intersect"
        ),
        .target(
            name: "MyOkM20Nsqual",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyOkM20Nsqual"
        ),
        .target(
            name: "MyOkM20Redefine",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyOkM20Redefine"
        ),
        .target(
            name: "MyOkM20Simple",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyOkM20Simple"
        ),
        .target(
            name: "MyOkM20Was_propagation_v1_0_0",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyOkM20Was_propagation_v1_0_0"
        ),
        .target(
            name: "MyOkM20Was_propagation",
            dependencies: ["BaboonRuntime", "MyOkM20Was_propagation_v1_0_0"],
            path: "Sources/MyOkM20Was_propagation"
        ),

        // ----- scope (references crossing a namespace or an ADT boundary) ---------------------------
        .target(
            name: "MyOkScope",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyOkScope"
        ),

        // ----- recursive.tree (T24 minimal recursive-DTO fixture; recursive DTO `Tree`) ---------
        // T23 / D8 coverage: wired into the Swift matrix so the recursive value-type DTO `Tree`
        // (`left: opt[Tree]`, boxed behind `@BaboonIndirect`) is compiled AND round-tripped.
        // T24 / D9: this coverage previously rode on the MCP stub's `McpStub` module, but that
        // stub lived inside the shared `--model-dir` and its cross-namespace fixtures broke the
        // wiring lanes. The stub was relocated out of the shared dir; this minimal single-type
        // `recursive.tree` model (module `RecursiveTree`) preserves the recursive-DTO coverage
        // without any cross-namespace fixture references.
        .target(
            name: "RecursiveTree",
            dependencies: ["BaboonRuntime"],
            path: "Sources/RecursiveTree"
        ),

        // ----- my.ok.extracted.contracts (T42/T45 extracted-contracts fixture) -----------------
        // Covers has-mirror / has-contract extraction: IMirroredPayload, IBox, IResult, IKey,
        // ITagMirror, ITagged, IContainer.  Hand-written shape assertions live in
        // Tests/RuntimeTests/ExtractedContractsTests.swift (T45).
        .target(
            name: "MyOkExtractedContracts",
            dependencies: ["BaboonRuntime"],
            path: "Sources/MyOkExtractedContracts"
        ),

        // ----- Hand-written runtime tests -------------------------------------------------------
        // AnyMetaCodecTests / AnyRoundTripTests / IdentifierReprTests live under
        // Tests/RuntimeTests/ to keep the codegen-owned Tests/BaboonTests/ tree pristine.
        // AnyRoundTripTests imports MyOk; IdentifierReprTests imports IdentifierOk.
        // ExtractedContractsTests imports MyOkExtractedContracts (T45).
        .testTarget(
            name: "RuntimeTests",
            dependencies: ["BaboonRuntime", "MyOk", "IdentifierOk", "MyOkM19Foreign", "TestpkgPkg0", "TestpkgPkg0_v1_0_0", "RecursiveTree", "MyOkExtractedContracts", "Fwde2eFwd", "Fwde2eChain"],
            path: "Tests/RuntimeTests"
        ),

        // ----- Per-module codegen test targets --------------------------------------------------
        // Each .testTarget(name: "<X>Tests", path: "Tests/BaboonTests/<X>") is its own Swift
        // module, so generated `*_test.swift` files can share basenames across modules without
        // .o filename collisions. The dependency on the corresponding source target satisfies
        // each test's `@testable import <X>`. CrossLanguageFixturePath.swift is materialized
        // into each subdirectory by the mdl test-gen action.
        .testTarget(
            name: "IdentifierOkTests",
            dependencies: ["BaboonRuntime", "IdentifierOk"],
            path: "Tests/BaboonTests/IdentifierOk"
        ),
        .testTarget(
            name: "MyOkTests",
            dependencies: ["BaboonRuntime", "MyOk"],
            path: "Tests/BaboonTests/MyOk"
        ),
        .testTarget(
            name: "MyOkIdcontractsTests",
            dependencies: ["BaboonRuntime", "MyOkIdcontracts"],
            path: "Tests/BaboonTests/MyOkIdcontracts"
        ),
        .testTarget(
            name: "M30ScDocsTests",
            dependencies: ["BaboonRuntime", "M30ScDocs"],
            path: "Tests/BaboonTests/M30ScDocs"
        ),
        .testTarget(
            name: "MixedCasePkgTests",
            dependencies: ["BaboonRuntime", "MixedCasePkg"],
            path: "Tests/BaboonTests/MixedCasePkg"
        ),
        .testTarget(
            name: "MyLspM29Tests",
            dependencies: ["BaboonRuntime", "MyLspM29"],
            path: "Tests/BaboonTests/MyLspM29"
        ),
        .testTarget(
            name: "MyLspM33Tests",
            dependencies: ["BaboonRuntime", "MyLspM33"],
            path: "Tests/BaboonTests/MyLspM33"
        ),
        .testTarget(
            name: "MyOkM28U64keyTests",
            dependencies: ["BaboonRuntime", "MyOkM28U64key"],
            path: "Tests/BaboonTests/MyOkM28U64key"
        ),
        .testTarget(
            name: "MyOkM29Tests",
            dependencies: ["BaboonRuntime", "MyOkM29"],
            path: "Tests/BaboonTests/MyOkM29"
        ),
        .testTarget(
            name: "MyOkM30Tests",
            dependencies: ["BaboonRuntime", "MyOkM30"],
            path: "Tests/BaboonTests/MyOkM30"
        ),
        .testTarget(
            name: "MyOkM31Tests",
            dependencies: ["BaboonRuntime", "MyOkM31"],
            path: "Tests/BaboonTests/MyOkM31"
        ),
        .testTarget(
            name: "MyOkM33Tests",
            dependencies: ["BaboonRuntime", "MyOkM33"],
            path: "Tests/BaboonTests/MyOkM33"
        ),
        .testTarget(
            name: "ReservedWordsTests",
            dependencies: ["BaboonRuntime", "ReservedWords"],
            path: "Tests/BaboonTests/ReservedWords"
        ),
        .testTarget(
            name: "MyOkM19AliasTests",
            dependencies: ["BaboonRuntime", "MyOkM19Alias"],
            path: "Tests/BaboonTests/MyOkM19Alias"
        ),
        .testTarget(
            name: "MyOkM19DirectTests",
            dependencies: ["BaboonRuntime", "MyOkM19Direct"],
            path: "Tests/BaboonTests/MyOkM19Direct"
        ),
        .testTarget(
            name: "MyOkM19MultiidTests",
            dependencies: ["BaboonRuntime", "MyOkM19Multiid"],
            path: "Tests/BaboonTests/MyOkM19Multiid"
        ),
        .testTarget(
            name: "MyOkM19NestedTests",
            dependencies: ["BaboonRuntime", "MyOkM19Nested"],
            path: "Tests/BaboonTests/MyOkM19Nested"
        ),
        .testTarget(
            name: "MyOkM19SingleidTests",
            dependencies: ["BaboonRuntime", "MyOkM19Singleid"],
            path: "Tests/BaboonTests/MyOkM19Singleid"
        ),
        .testTarget(
            name: "MyOkM20ChainedTests",
            dependencies: ["BaboonRuntime", "MyOkM20Chained"],
            path: "Tests/BaboonTests/MyOkM20Chained"
        ),
        .testTarget(
            name: "MyOkM20EquivTests",
            dependencies: ["BaboonRuntime", "MyOkM20Equiv"],
            path: "Tests/BaboonTests/MyOkM20Equiv"
        ),
        .testTarget(
            name: "MyOkM20EquivmanualTests",
            dependencies: ["BaboonRuntime", "MyOkM20Equivmanual"],
            path: "Tests/BaboonTests/MyOkM20Equivmanual"
        ),
        .testTarget(
            name: "MyOkM20EquivsugaredTests",
            dependencies: ["BaboonRuntime", "MyOkM20Equivsugared"],
            path: "Tests/BaboonTests/MyOkM20Equivsugared"
        ),
        .testTarget(
            name: "MyOkM20ExcludeTests",
            dependencies: ["BaboonRuntime", "MyOkM20Exclude"],
            path: "Tests/BaboonTests/MyOkM20Exclude"
        ),
        .testTarget(
            name: "MyOkM20IntersectTests",
            dependencies: ["BaboonRuntime", "MyOkM20Intersect"],
            path: "Tests/BaboonTests/MyOkM20Intersect"
        ),
        .testTarget(
            name: "MyOkM20LexicalTests",
            dependencies: ["BaboonRuntime", "MyOkM20Lexical"],
            path: "Tests/BaboonTests/MyOkM20Lexical"
        ),
        .testTarget(
            name: "MyOkM20Multi_intersectTests",
            dependencies: ["BaboonRuntime", "MyOkM20Multi_intersect"],
            path: "Tests/BaboonTests/MyOkM20Multi_intersect"
        ),
        .testTarget(
            name: "MyOkM20NsqualTests",
            dependencies: ["BaboonRuntime", "MyOkM20Nsqual"],
            path: "Tests/BaboonTests/MyOkM20Nsqual"
        ),
        .testTarget(
            name: "MyOkM20RedefineTests",
            dependencies: ["BaboonRuntime", "MyOkM20Redefine"],
            path: "Tests/BaboonTests/MyOkM20Redefine"
        ),
        .testTarget(
            name: "MyOkM20SimpleTests",
            dependencies: ["BaboonRuntime", "MyOkM20Simple"],
            path: "Tests/BaboonTests/MyOkM20Simple"
        ),
        .testTarget(
            name: "MyOkM20Was_propagationTests",
            dependencies: ["BaboonRuntime", "MyOkM20Was_propagation"],
            path: "Tests/BaboonTests/MyOkM20Was_propagation"
        ),
        .testTarget(
            name: "MyOkScopeTests",
            dependencies: ["BaboonRuntime", "MyOkScope"],
            path: "Tests/BaboonTests/MyOkScope"
        ),
        .testTarget(
            name: "RenameNsTests",
            dependencies: ["BaboonRuntime", "RenameNs"],
            path: "Tests/BaboonTests/RenameNs"
        ),
        .testTarget(
            name: "TestpkgPkg0Tests",
            dependencies: ["BaboonRuntime", "TestpkgPkg0"],
            path: "Tests/BaboonTests/TestpkgPkg0"
        ),
        .testTarget(
            name: "Fwde2eFwdTests",
            dependencies: ["BaboonRuntime", "Fwde2eFwd"],
            path: "Tests/BaboonTests/Fwde2eFwd"
        ),
        .testTarget(
            name: "Fwde2eChainTests",
            dependencies: ["BaboonRuntime", "Fwde2eChain"],
            path: "Tests/BaboonTests/Fwde2eChain"
        ),
        .testTarget(
            name: "Fwde2eRenameTests",
            dependencies: ["BaboonRuntime", "Fwde2eRename"],
            path: "Tests/BaboonTests/Fwde2eRename"
        ),
        .testTarget(
            name: "RnnestedModelTests",
            dependencies: ["BaboonRuntime", "RnnestedModel"],
            path: "Tests/BaboonTests/RnnestedModel"
        ),
        .testTarget(
            name: "MyOkExtractedContractsTests",
            dependencies: ["BaboonRuntime", "MyOkExtractedContracts"],
            path: "Tests/BaboonTests/MyOkExtractedContracts"
        ),
    ]
)
