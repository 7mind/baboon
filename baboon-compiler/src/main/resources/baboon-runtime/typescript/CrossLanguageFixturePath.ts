import * as fs from 'fs';
import * as path from 'path';

// Cross-language fixture path resolution for generated tests.
//
// Layout (per mdl test-gen-{regular,wrapped}-adt actions):
//   <repoRoot>/target/test-{regular,wrapped}/         <-- "test isolation root"
//     |-- <lang>-stub/                                <-- per-language stub dir
//     |     |-- ...                                   <-- vitest/etc test runs from here
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
//  3. If neither succeeds, throw -- never silently fall back.
//
// The assertion validates the "anchor" -- D itself in walk-up mode, or the
// env-var path in override mode. The anchor is always guaranteed to exist
// (mdl creates <isolation> before launching tests; env-var users supply a
// real path). The fixture root D/target may not yet exist if no peer
// language has written into it -- per-test existence guards handle that.

let cachedAnchor: string | null = null;
let cachedFixtureRoot: string | null = null;

function hasStubSibling(dir: string): boolean {
    try {
        const entries = fs.readdirSync(dir, { withFileTypes: true });
        for (const entry of entries) {
            if (entry.isDirectory() && entry.name.endsWith('-stub')) {
                return true;
            }
        }
    } catch {
        // unreadable dir -- skip
    }
    return false;
}

function isExistingDir(p: string): boolean {
    try {
        return fs.statSync(p).isDirectory();
    } catch {
        return false;
    }
}

function resolveOnce(): void {
    if (cachedFixtureRoot !== null) return;

    const envRoot = process.env.BABOON_CROSS_LANG_FIXTURE_ROOT;
    if (envRoot !== undefined) {
        cachedAnchor = envRoot;
        cachedFixtureRoot = envRoot;
        return;
    }

    const startDir = process.cwd();
    let dir = startDir;
    while (true) {
        const stub = hasStubSibling(dir);
        const strictMatch = stub && isExistingDir(path.join(dir, 'target'));
        const name = path.basename(dir);
        const namedMatch = stub && (name === 'test-regular' || name === 'test-wrapped');
        if (strictMatch || namedMatch) {
            cachedAnchor = dir;
            cachedFixtureRoot = path.join(dir, 'target');
            return;
        }
        const parent = path.dirname(dir);
        if (parent === dir) {
            break;
        }
        dir = parent;
    }

    throw new Error(
        `Could not locate cross-language fixture root. Walked up from "${startDir}" looking for either: (a) a directory containing "target/" and at least one "*-stub/" sibling, or (b) a directory named "test-regular" or "test-wrapped" containing at least one "*-stub/" sibling. Set BABOON_CROSS_LANG_FIXTURE_ROOT to override.`,
    );
}

export function crossLanguageFixtureRoot(): string {
    resolveOnce();
    return cachedFixtureRoot as string;
}

export function crossLanguageFixturePath(lang: string, tpe: string, format: string): string {
    const root = crossLanguageFixtureRoot();
    return `${root}/${lang}/${format}/${tpe}`;
}

// Sanity check intended for vitest beforeAll. Unconditionally asserts the
// anchor (walk-up matched directory, or env-var path) exists on disk and is
// a directory. The anchor is guaranteed to exist by the mdl action layout.
// If env-var was set but pointed to a non-existent path, this catches it.
// Either way: loud-fail at bootstrap rather than silent skip.
export function assertCrossLanguageFixtureRootExists(): void {
    resolveOnce();
    const anchor = cachedAnchor as string;
    if (!isExistingDir(anchor)) {
        throw new Error(
            `Cross-language fixture anchor does not exist: "${anchor}". Either the path resolution is wrong, or the anchor was not created by the build. Set BABOON_CROSS_LANG_FIXTURE_ROOT to override.`,
        );
    }
}
