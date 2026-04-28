import Foundation

// Cross-language fixture path resolution for generated tests.
//
// Layout (per mdl test-gen-{regular,wrapped}-adt actions):
//   <repoRoot>/target/test-{regular,wrapped}/         <-- "test isolation root"
//     ├── <lang>-stub/                                <-- per-language stub dir
//     │     └── ...                                   <-- dart/swift/etc test runs from here
//     └── target/                                     <-- "fixture root"
//           └── <lang>/<format>/<type>                <-- cross-language fixture files
//
// Both `dart test` (Dart) and `swift test` (Swift) are launched with cwd =
// "<repoRoot>/target/test-{regular,wrapped}/<lang>-stub". So the test-isolation
// root is always parent(cwd) under normal invocation.
//
// Resolution:
//  1. BABOON_CROSS_LANG_FIXTURE_ROOT env var — if set, used verbatim as the
//     fixture root. Returns "$root/$lang/$format/$type".
//  2. Walk up from cwd until we find a directory D such that:
//        - D contains a subdir literally named "target", AND
//        - D contains at least one subdir whose name ends with "-stub".
//     That uniquely identifies the test-isolation root. Returns
//     "D/target/$lang/$format/$type".
//  3. If neither succeeds, fatalError — never silently fall back.
//
// `crossLanguageFixtureRoot()` exposes the resolved fixture root for
// bootstrap-time sanity checks. Tests should call
// `assertCrossLanguageFixtureRootExists()` in their per-class setUp so a
// misconfigured root fails LOUDLY rather than silently causing all per-test
// fileExists() guards to skip.

private final class _CrossLangFixtureRootCache: @unchecked Sendable {
    static let shared = _CrossLangFixtureRootCache()
    private let lock = NSLock()
    private var resolved: String?

    func get() -> String {
        lock.lock()
        defer { lock.unlock() }
        if let r = resolved { return r }
        let r = _resolveCrossLanguageFixtureRoot()
        resolved = r
        return r
    }
}

private func _resolveCrossLanguageFixtureRoot() -> String {
    if let envRoot = ProcessInfo.processInfo.environment["BABOON_CROSS_LANG_FIXTURE_ROOT"] {
        return envRoot
    }

    let fm = FileManager.default
    var dir = URL(fileURLWithPath: fm.currentDirectoryPath).standardizedFileURL
    while true {
        let targetDir = dir.appendingPathComponent("target")
        var isDir: ObjCBool = false
        let hasTarget = fm.fileExists(atPath: targetDir.path, isDirectory: &isDir) && isDir.boolValue
        if hasTarget {
            var hasStub = false
            if let entries = try? fm.contentsOfDirectory(at: dir, includingPropertiesForKeys: [.isDirectoryKey], options: [.skipsHiddenFiles]) {
                for entry in entries {
                    var entryIsDir: ObjCBool = false
                    if fm.fileExists(atPath: entry.path, isDirectory: &entryIsDir), entryIsDir.boolValue {
                        if entry.lastPathComponent.hasSuffix("-stub") {
                            hasStub = true
                            break
                        }
                    }
                }
            }
            if hasStub {
                return targetDir.path
            }
        }
        let parent = dir.deletingLastPathComponent()
        if parent.path == dir.path { break }
        dir = parent
    }

    fatalError(
        "Could not locate cross-language fixture root. Walked up from "
        + "\"\(FileManager.default.currentDirectoryPath)\" looking for a directory "
        + "containing both a \"target/\" subdirectory and at least one \"*-stub/\" "
        + "sibling. Set BABOON_CROSS_LANG_FIXTURE_ROOT to override."
    )
}

func crossLanguageFixtureRoot() -> String {
    return _CrossLangFixtureRootCache.shared.get()
}

func crossLanguageFixturePath(_ lang: String, _ type: String, _ format: String) -> String {
    let root = _CrossLangFixtureRootCache.shared.get()
    return "\(root)/\(lang)/\(format)/\(type)"
}

// Sanity check intended for per-class XCTestCase setUp. Asserts the resolved
// fixture root directory exists. If it does not, fails loudly with a
// diagnostic message instead of letting per-test fileExists() guards silently
// skip every cross-language read.
func assertCrossLanguageFixtureRootExists() {
    let root = _CrossLangFixtureRootCache.shared.get()
    let fm = FileManager.default
    var isDir: ObjCBool = false
    let exists = fm.fileExists(atPath: root, isDirectory: &isDir) && isDir.boolValue
    if !exists {
        fatalError(
            "Cross-language fixture root does not exist: \"\(root)\". "
            + "Either the path resolution is wrong, or the fixture root was not "
            + "created by the build. Set BABOON_CROSS_LANG_FIXTURE_ROOT to override."
        )
    }
}
