package baboon.runtime.shared

import java.nio.file.{Files, Path, Paths}

// Cross-language fixture path resolution for generated tests.
//
// Layout (per mdl test-gen-{regular,wrapped}-adt actions):
//   <repoRoot>/target/test-{regular,wrapped}/         <-- "test isolation root"
//     |-- <lang>-stub/                                <-- per-language stub dir
//     |     |-- ...                                   <-- sbt/cargo/etc test runs from here
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
//        b. NAMED: directory D's name starts with "test-" AND D contains
//           at least one "*-stub/" sibling. Covers test-regular,
//           test-wrapped, test-<lang>-wiring-*, and any future mdl
//           test-isolation conventions of the same shape. (Works on the
//           very first language run, before any peer has created
//           <isolation>/target/.)
//     Both rules identify D = <isolation>; the fixture root is D/target.
//  3. If neither succeeds, throw IllegalStateException -- never silently
//     fall back.
//
// The assertion validates the "anchor" -- D itself in walk-up mode, or the
// env-var path in override mode. The anchor is always guaranteed to exist
// (mdl creates <isolation> before launching tests; env-var users supply a
// real path). The fixture root D/target may not yet exist if no peer
// language has written into it -- per-test existence guards handle that.

object CrossLanguageFixturePath {
  private var cachedAnchor:      Option[String] = None
  private var cachedFixtureRoot: Option[String] = None

  private def resolve(): Unit = synchronized {
    if (cachedFixtureRoot.isDefined) return

    sys.env.get("BABOON_CROSS_LANG_FIXTURE_ROOT") match {
      case Some(envRoot) =>
        cachedAnchor      = Some(envRoot)
        cachedFixtureRoot = Some(envRoot)
      case None =>
        val anchor = walkUpFromCwd()
        cachedAnchor      = Some(anchor)
        cachedFixtureRoot = Some(Paths.get(anchor, "target").toString)
    }
  }

  private def hasStubSibling(dir: Path): Boolean = {
    var found = false
    try {
      val stream = Files.list(dir)
      try {
        val it = stream.iterator()
        while (it.hasNext && !found) {
          val entry = it.next()
          if (Files.isDirectory(entry) && entry.getFileName.toString.endsWith("-stub")) {
            found = true
          }
        }
      } finally {
        stream.close()
      }
    } catch {
      case _: java.io.IOException => // unreadable, skip
    }
    found
  }

  private def walkUpFromCwd(): String = {
    val startDir = Paths.get("").toAbsolutePath.normalize()
    var dir: Path = startDir
    while (dir != null) {
      val stub         = hasStubSibling(dir)
      val strictMatch  = stub && Files.isDirectory(dir.resolve("target"))
      val name         = Option(dir.getFileName).map(_.toString).getOrElse("")
      val namedMatch   = stub && name.startsWith("test-")
      if (strictMatch || namedMatch) {
        return dir.toString
      }
      val parent = dir.getParent
      if (parent == null || parent == dir) {
        throw new IllegalStateException(
          s"""Could not locate cross-language fixture root. Walked up from "$startDir" looking for either: (a) a directory containing "target/" and at least one "*-stub/" sibling, or (b) a directory whose name starts with "test-" and that contains at least one "*-stub/" sibling. Set BABOON_CROSS_LANG_FIXTURE_ROOT to override."""
        )
      }
      dir = parent
    }
    throw new IllegalStateException("unreachable")
  }

  def crossLanguageFixtureRoot(): String = {
    resolve()
    cachedFixtureRoot.get
  }

  def crossLanguageFixturePath(lang: String, tpe: String, format: String): String = {
    val root = crossLanguageFixtureRoot()
    s"$root/$lang/$format/$tpe"
  }

  // Sanity check intended for setUpAll/beforeAll. Unconditionally asserts the
  // anchor (walk-up matched directory, or env-var path) exists on disk and is
  // a directory. The anchor is guaranteed to exist by the mdl action layout
  // (the isolation root <repoRoot>/target/test-{regular,wrapped}/ is created
  // before tests launch). If walk-up failed it has already thrown; if env-var
  // pointed to a non-existent path, this catches it. Either way: loud-fail at
  // bootstrap rather than silent skip.
  def assertCrossLanguageFixtureRootExists(): Unit = {
    resolve()
    val anchor = cachedAnchor.get
    if (!Files.isDirectory(Paths.get(anchor))) {
      throw new IllegalStateException(
        s"""Cross-language fixture anchor does not exist: "$anchor". Either the path resolution is wrong, or the anchor was not created by the build. Set BABOON_CROSS_LANG_FIXTURE_ROOT to override."""
      )
    }
  }
}
