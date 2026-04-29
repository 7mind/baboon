"""Cross-language fixture path resolution for generated tests.

Layout (per mdl test-gen-{regular,wrapped}-adt actions):
  <repoRoot>/target/test-{regular,wrapped}/         <-- "test isolation root"
    |-- <lang>-stub/                                <-- per-language stub dir
    |     |-- ...                                   <-- pytest/unittest runs from here
    |-- target/                                     <-- "fixture root"
          |-- <lang>/<format>/<type>                <-- cross-language fixture files

Resolution:
 1. BABOON_CROSS_LANG_FIXTURE_ROOT env var, if set, used verbatim as the
    fixture root.
 2. Walk up from cwd. The walk has two layered sentinel rules; the first
    ancestor matching either rule wins:
       a. STRICT: directory D contains a literal "target/" subdirectory
          AND at least one "*-stub/" sibling. (Works whenever any peer
          language has already populated <isolation>/target/<lang>/...)
       b. NAMED: directory D's name starts with "test-" AND D contains
          at least one "*-stub/" sibling. Covers test-regular,
          test-wrapped, test-<lang>-wiring-*, and any future mdl
          test-isolation conventions of the same shape. (Works on the
          very first language run, before any peer has created
          <isolation>/target/.)
    Both rules identify D = <isolation>; the fixture root is D/target.
 3. If neither succeeds, raise RuntimeError -- never silently fall back.

The assertion validates the "anchor" -- D itself in walk-up mode, or the
env-var path in override mode. The anchor is always guaranteed to exist
(mdl creates <isolation> before launching tests; env-var users supply a
real path). The fixture root D/target may not yet exist if no peer
language has written into it -- per-test existence guards handle that.
"""

import os
import threading

_lock = threading.Lock()
_cached_anchor = None
_cached_fixture_root = None


def _has_stub_sibling(dir_):
    try:
        with os.scandir(dir_) as it:
            for entry in it:
                if entry.is_dir() and entry.name.endswith("-stub"):
                    return True
    except OSError:
        pass
    return False


def _resolve_once():
    global _cached_anchor
    global _cached_fixture_root
    with _lock:
        if _cached_fixture_root is not None:
            return

        env_root = os.environ.get("BABOON_CROSS_LANG_FIXTURE_ROOT")
        if env_root is not None:
            _cached_anchor = env_root
            _cached_fixture_root = env_root
            return

        start_dir = os.getcwd()
        dir_ = start_dir
        while True:
            stub = _has_stub_sibling(dir_)
            strict_match = stub and os.path.isdir(os.path.join(dir_, "target"))
            name = os.path.basename(dir_)
            named_match = stub and name.startswith("test-")
            if strict_match or named_match:
                _cached_anchor = dir_
                _cached_fixture_root = os.path.join(dir_, "target")
                return
            parent = os.path.dirname(dir_)
            if parent == dir_:
                break
            dir_ = parent

        raise RuntimeError(
            'Could not locate cross-language fixture root. Walked up from "{}" '
            'looking for either: (a) a directory containing "target/" and at '
            'least one "*-stub/" sibling, or (b) a directory whose name starts '
            'with "test-" and that contains at least one "*-stub/" sibling. '
            "Set BABOON_CROSS_LANG_FIXTURE_ROOT to override.".format(start_dir)
        )


def cross_language_fixture_root():
    _resolve_once()
    return _cached_fixture_root


def cross_language_fixture_path(lang, tpe, format):
    root = cross_language_fixture_root()
    return "{}/{}/{}/{}".format(root, lang, format, tpe)


def assert_cross_language_fixture_root_exists():
    """Sanity check intended for unittest setUpClass.

    Unconditionally asserts the anchor (walk-up matched directory, or
    env-var path) exists on disk and is a directory. The anchor is
    guaranteed to exist by the mdl action layout. If env-var was set but
    pointed to a non-existent path, this catches it. Either way:
    loud-fail at bootstrap rather than silent skip.
    """
    _resolve_once()
    if not os.path.isdir(_cached_anchor):
        raise RuntimeError(
            'Cross-language fixture anchor does not exist: "{}". Either '
            "the path resolution is wrong, or the anchor was not created "
            "by the build. Set BABOON_CROSS_LANG_FIXTURE_ROOT to "
            "override.".format(_cached_anchor)
        )
