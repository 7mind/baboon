import 'dart:io';

// Cross-language fixture path resolution for generated tests.
//
// Layout (per mdl test-gen-{regular,wrapped}-adt actions):
//   <repoRoot>/target/test-{regular,wrapped}/         <-- "test isolation root"
//     |-- <lang>-stub/                                <-- per-language stub dir
//     |     |-- ...                                   <-- dart test runs from here
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
//  3. If neither succeeds, throw StateError -- never silently fall back.
//
// The assertion validates the "anchor" -- D itself in walk-up mode, or the
// env-var path in override mode. The anchor is always guaranteed to exist
// (mdl creates <isolation> before launching tests; env-var users supply a
// real path). The fixture root D/target may not yet exist if no peer
// language has written into it -- per-test existence guards handle that.

String? _cachedAnchor;
String? _cachedFixtureRoot;

void _resolve() {
  if (_cachedFixtureRoot != null) return;

  final envRoot = Platform.environment['BABOON_CROSS_LANG_FIXTURE_ROOT'];
  if (envRoot != null) {
    _cachedAnchor = envRoot;
    _cachedFixtureRoot = envRoot;
    return;
  }

  final anchor = _walkUpFromCwd();
  _cachedAnchor = anchor;
  _cachedFixtureRoot = '$anchor/target';
}

bool _hasStubSibling(Directory dir) {
  try {
    for (final entry in dir.listSync(followLinks: false)) {
      if (entry is Directory) {
        final segments = entry.uri.pathSegments.where((s) => s.isNotEmpty).toList();
        final name = segments.isEmpty ? '' : segments.last;
        if (name.endsWith('-stub')) {
          return true;
        }
      }
    }
  } on FileSystemException {
    // unreadable, skip
  }
  return false;
}

String _dirName(Directory dir) {
  final segments = dir.uri.pathSegments.where((s) => s.isNotEmpty).toList();
  return segments.isEmpty ? '' : segments.last;
}

String _walkUpFromCwd() {
  final startDir = Directory.current;
  var dir = startDir;
  while (true) {
    final stub = _hasStubSibling(dir);
    final strictMatch = stub && Directory('${dir.path}/target').existsSync();
    final name = _dirName(dir);
    final namedMatch = stub && (name == 'test-regular' || name == 'test-wrapped');
    if (strictMatch || namedMatch) {
      return dir.path;
    }
    final parent = dir.parent;
    if (parent.path == dir.path) {
      throw StateError(
        'Could not locate cross-language fixture root. Walked up from '
        '"${startDir.path}" looking for either: (a) a directory containing '
        '"target/" and at least one "*-stub/" sibling, or (b) a directory '
        'named "test-regular" or "test-wrapped" containing at least one '
        '"*-stub/" sibling. Set BABOON_CROSS_LANG_FIXTURE_ROOT to override.',
      );
    }
    dir = parent;
  }
}

String crossLanguageFixtureRoot() {
  _resolve();
  return _cachedFixtureRoot!;
}

String crossLanguageFixturePath(String lang, String type, String format) {
  final root = crossLanguageFixtureRoot();
  return '$root/$lang/$format/$type';
}

// Sanity check intended for `setUpAll`. Unconditionally asserts the anchor
// (walk-up matched directory, or env-var path) exists on disk and is a
// directory. The anchor is guaranteed to exist by the mdl action layout
// (the isolation root <repoRoot>/target/test-{regular,wrapped}/ is created
// before tests launch). If walk-up failed it has already thrown; if env-var
// pointed to a non-existent path, this catches it. Either way: loud-fail at
// bootstrap rather than silent skip.
void assertCrossLanguageFixtureRootExists() {
  _resolve();
  final anchor = _cachedAnchor!;
  if (!Directory(anchor).existsSync()) {
    throw StateError(
      'Cross-language fixture anchor does not exist: "$anchor". '
      'Either the path resolution is wrong, or the anchor was not '
      'created by the build. Set BABOON_CROSS_LANG_FIXTURE_ROOT to override.',
    );
  }
}
