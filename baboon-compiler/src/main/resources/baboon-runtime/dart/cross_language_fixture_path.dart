import 'dart:io';

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
//  3. If neither succeeds, throw — never silently fall back. Silent fallback
//     was the failure mode the previous .git-walk-up version exhibited.
//
// `crossLanguageFixtureRoot()` exposes the resolved fixture root for
// bootstrap-time sanity checks. Tests should call `assertCrossLanguageFixtureRootExists()`
// in `setUpAll` so a misconfigured root fails LOUDLY rather than silently
// causing all per-test `existsSync()` guards to skip.

String? _cachedFixtureRoot;

String _resolveFixtureRoot() {
  final cached = _cachedFixtureRoot;
  if (cached != null) return cached;

  final envRoot = Platform.environment['BABOON_CROSS_LANG_FIXTURE_ROOT'];
  if (envRoot != null) {
    _cachedFixtureRoot = envRoot;
    return envRoot;
  }

  var dir = Directory.current;
  while (true) {
    final hasTarget = Directory('${dir.path}/target').existsSync();
    if (hasTarget) {
      var hasStub = false;
      try {
        for (final entry in dir.listSync(followLinks: false)) {
          if (entry is Directory) {
            final name = entry.uri.pathSegments.where((s) => s.isNotEmpty).last;
            if (name.endsWith('-stub')) {
              hasStub = true;
              break;
            }
          }
        }
      } on FileSystemException {
        // unreadable dir — skip and continue walking up
      }
      if (hasStub) {
        final root = '${dir.path}/target';
        _cachedFixtureRoot = root;
        return root;
      }
    }
    final parent = dir.parent;
    if (parent.path == dir.path) break;
    dir = parent;
  }

  throw StateError(
    'Could not locate cross-language fixture root. Walked up from '
    '"${Directory.current.path}" looking for a directory containing both '
    'a "target/" subdirectory and at least one "*-stub/" sibling. '
    'Set BABOON_CROSS_LANG_FIXTURE_ROOT to override.',
  );
}

String crossLanguageFixtureRoot() => _resolveFixtureRoot();

String crossLanguageFixturePath(String lang, String type, String format) {
  final root = _resolveFixtureRoot();
  return '$root/$lang/$format/$type';
}

// Sanity check intended for `setUpAll`. Asserts the resolved fixture root
// directory exists. If it does not, fails loudly with a diagnostic message
// instead of letting per-test `existsSync()` guards silently skip every
// cross-language read.
void assertCrossLanguageFixtureRootExists() {
  final root = _resolveFixtureRoot();
  if (!Directory(root).existsSync()) {
    throw StateError(
      'Cross-language fixture root does not exist: "$root". '
      'Either the path resolution is wrong, or the fixture root was not '
      'created by the build. Set BABOON_CROSS_LANG_FIXTURE_ROOT to override.',
    );
  }
}
