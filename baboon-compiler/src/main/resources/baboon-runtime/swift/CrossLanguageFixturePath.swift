import Foundation

// Cross-language fixture path resolution for generated tests.
//
// Layout (per mdl test-gen-{regular,wrapped}-adt actions):
//   <repoRoot>/target/test-{regular,wrapped}/         <-- "test isolation root"
//     |-- <lang>-stub/                                <-- per-language stub dir
//     |     |-- ...                                   <-- swift test runs from here
//     |-- target/                                     <-- "fixture root"
//           |-- <lang>/<format>/<type>                <-- cross-language fixture files
//
// Resolution:
//  1. BABOON_CROSS_LANG_FIXTURE_ROOT env var, if set, used verbatim as the
//     fixture root.
//  2. Walk up from cwd. The walk has two layered sentinel rules; the first
//     ancestor matching either rule wins:
//        a. STRICT: directory D contains a literal "target/" subdirectory
//           AND at least one "*-stub/" sibling. (Works whenever any peer
//           language has already populated <isolation>/target/<lang>/...)
//        b. NAMED: directory D's name is exactly "test-regular" or
//           "test-wrapped" AND D contains at least one "*-stub/" sibling.
//           (Works on the very first language run, before any peer has
//           created <isolation>/target/.)
//     Both rules identify D = <isolation>; the fixture root is D/target.
//  3. If neither succeeds, fatalError -- never silently fall back.
//
// The assertion validates the "anchor" -- D itself in walk-up mode, or the
// env-var path in override mode. The anchor is always guaranteed to exist
// (mdl creates <isolation> before launching tests; env-var users supply a
// real path). The fixture root D/target may not yet exist if no peer
// language has written into it -- per-test existence guards handle that.

private final class _CrossLangFixtureRootCache: @unchecked Sendable {
    static let shared = _CrossLangFixtureRootCache()
    private let lock = NSLock()
    private var anchor: String?
    private var fixtureRoot: String?

    func resolve() {
        lock.lock()
        defer { lock.unlock() }
        if fixtureRoot != nil { return }
        if let envRoot = ProcessInfo.processInfo.environment["BABOON_CROSS_LANG_FIXTURE_ROOT"] {
            anchor = envRoot
            fixtureRoot = envRoot
            return
        }
        let a = _walkUpFromCwd()
        anchor = a
        fixtureRoot = (a as NSString).appendingPathComponent("target")
    }

    func getAnchor() -> String {
        resolve()
        return anchor!
    }

    func getFixtureRoot() -> String {
        resolve()
        return fixtureRoot!
    }
}

private func _hasStubSibling(_ dir: URL) -> Bool {
    let fm = FileManager.default
    guard let entries = try? fm.contentsOfDirectory(at: dir, includingPropertiesForKeys: [.isDirectoryKey], options: [.skipsHiddenFiles]) else {
        return false
    }
    for entry in entries {
        var entryIsDir: ObjCBool = false
        if fm.fileExists(atPath: entry.path, isDirectory: &entryIsDir), entryIsDir.boolValue {
            if entry.lastPathComponent.hasSuffix("-stub") {
                return true
            }
        }
    }
    return false
}

private func _walkUpFromCwd() -> String {
    let fm = FileManager.default
    let startPath = fm.currentDirectoryPath
    var dir = URL(fileURLWithPath: startPath).standardizedFileURL
    while true {
        let stub = _hasStubSibling(dir)
        let targetDir = dir.appendingPathComponent("target")
        var isDir: ObjCBool = false
        let strictMatch = stub && fm.fileExists(atPath: targetDir.path, isDirectory: &isDir) && isDir.boolValue
        let name = dir.lastPathComponent
        let namedMatch = stub && (name == "test-regular" || name == "test-wrapped")
        if strictMatch || namedMatch {
            return dir.path
        }
        let parent = dir.deletingLastPathComponent()
        if parent.path == dir.path {
            fatalError(
                "Could not locate cross-language fixture root. Walked up from "
                + "\"\(startPath)\" looking for either: (a) a directory "
                + "containing \"target/\" and at least one \"*-stub/\" sibling, "
                + "or (b) a directory named \"test-regular\" or \"test-wrapped\" "
                + "containing at least one \"*-stub/\" sibling. "
                + "Set BABOON_CROSS_LANG_FIXTURE_ROOT to override."
            )
        }
        dir = parent
    }
}

func crossLanguageFixtureRoot() -> String {
    return _CrossLangFixtureRootCache.shared.getFixtureRoot()
}

func crossLanguageFixturePath(_ lang: String, _ type: String, _ format: String) -> String {
    let root = _CrossLangFixtureRootCache.shared.getFixtureRoot()
    return "\(root)/\(lang)/\(format)/\(type)"
}

// Sanity check intended for per-class XCTestCase setUp. Unconditionally
// asserts the anchor (walk-up matched directory, or env-var path) exists on
// disk and is a directory. The anchor is guaranteed to exist by the mdl
// action layout (the isolation root <repoRoot>/target/test-{regular,wrapped}/
// is created before tests launch). If walk-up failed it has already
// fatalError'd; if env-var pointed to a non-existent path, this catches it.
// Either way: loud-fail at bootstrap rather than silent skip.
func assertCrossLanguageFixtureRootExists() {
    let anchor = _CrossLangFixtureRootCache.shared.getAnchor()
    let fm = FileManager.default
    var isDir: ObjCBool = false
    let exists = fm.fileExists(atPath: anchor, isDirectory: &isDir) && isDir.boolValue
    if !exists {
        fatalError(
            "Cross-language fixture anchor does not exist: \"\(anchor)\". "
            + "Either the path resolution is wrong, or the anchor was not "
            + "created by the build. Set BABOON_CROSS_LANG_FIXTURE_ROOT to override."
        )
    }
}
