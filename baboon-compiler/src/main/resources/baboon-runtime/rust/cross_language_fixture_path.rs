// Cross-language fixture path resolution for generated tests.
//
// Layout (per mdl test-gen-{regular,wrapped}-adt actions):
//   <repoRoot>/target/test-{regular,wrapped}/         <-- "test isolation root"
//     |-- <lang>-stub/                                <-- per-language stub dir
//     |     |-- ...                                   <-- cargo test runs from here
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
//  3. If neither succeeds, panic! -- never silently fall back.
//
// The assertion validates the "anchor" -- D itself in walk-up mode, or the
// env-var path in override mode. The anchor is always guaranteed to exist
// (mdl creates <isolation> before launching tests; env-var users supply a
// real path). The fixture root D/target may not yet exist if no peer
// language has written into it -- per-test existence guards handle that.

use std::path::{Path, PathBuf};
use std::sync::OnceLock;

struct Resolved {
    anchor: String,
    fixture_root: String,
}

static CACHED: OnceLock<Resolved> = OnceLock::new();

fn resolve() -> &'static Resolved {
    CACHED.get_or_init(compute)
}

fn has_stub_sibling(dir: &Path) -> bool {
    if let Ok(entries) = std::fs::read_dir(dir) {
        for entry in entries.flatten() {
            if let Ok(file_type) = entry.file_type() {
                if file_type.is_dir() {
                    let name = entry.file_name();
                    if let Some(s) = name.to_str() {
                        if s.ends_with("-stub") {
                            return true;
                        }
                    }
                }
            }
        }
    }
    false
}

fn compute() -> Resolved {
    if let Ok(env_root) = std::env::var("BABOON_CROSS_LANG_FIXTURE_ROOT") {
        return Resolved {
            anchor: env_root.clone(),
            fixture_root: env_root,
        };
    }

    let start_dir = std::env::current_dir().expect("current_dir() failed");
    let mut dir: PathBuf = start_dir.clone();
    loop {
        let stub = has_stub_sibling(&dir);
        let strict_match = stub && dir.join("target").is_dir();
        let name_str = dir.file_name().and_then(|s| s.to_str()).unwrap_or("");
        let named_match = stub && (name_str == "test-regular" || name_str == "test-wrapped");
        if strict_match || named_match {
            let anchor = dir.to_string_lossy().into_owned();
            let fixture_root = dir.join("target").to_string_lossy().into_owned();
            return Resolved { anchor, fixture_root };
        }
        if !dir.pop() {
            break;
        }
    }

    panic!(
        "Could not locate cross-language fixture root. Walked up from \"{}\" looking for either: (a) a directory containing \"target/\" and at least one \"*-stub/\" sibling, or (b) a directory named \"test-regular\" or \"test-wrapped\" containing at least one \"*-stub/\" sibling. Set BABOON_CROSS_LANG_FIXTURE_ROOT to override.",
        start_dir.to_string_lossy()
    );
}

pub fn cross_language_fixture_root() -> String {
    resolve().fixture_root.clone()
}

pub fn cross_language_fixture_path(lang: &str, tpe: &str, format: &str) -> String {
    let root = &resolve().fixture_root;
    format!("{}/{}/{}/{}", root, lang, format, tpe)
}

// Sanity check intended to be called as the first statement of every
// generated test function. Unconditionally asserts the anchor (walk-up
// matched directory, or env-var path) exists on disk and is a directory.
// The anchor is guaranteed to exist by the mdl action layout. If env-var
// was set but pointed to a non-existent path, this catches it. Either way:
// loud-fail at bootstrap rather than silent skip.
pub fn assert_cross_language_fixture_root_exists() {
    let anchor = &resolve().anchor;
    if !Path::new(anchor).is_dir() {
        panic!(
            "Cross-language fixture anchor does not exist: \"{}\". Either the path resolution is wrong, or the anchor was not created by the build. Set BABOON_CROSS_LANG_FIXTURE_ROOT to override.",
            anchor
        );
    }
}
