// NOTE: This test references generated symbols from pkg0/pkg01.baboon (v1.0.0,
// non-latest). The v1.0.0 emission of `ObscureInt_KeyCodec` (Custom non-stringy
// foreign) is annotated `@deprecated` because v1.0.0 != latest. PR-I-D04
// regression: prior to PR-26.3, only the trait carried the annotation; the
// companion `object` did not. Both must now carry it.
//
// Generated only by `mdl :build :test-gen-regular-adt`; running `sbt test`
// from the source tree fails with missing symbols. Run from the codegen'd
// copy under target/test-regular/sc-stub/.
//
// We use scala.reflect.runtime to inspect Symbol annotations because Scala's
// `@scala.deprecated` lowers to the legacy classfile `Deprecated` attribute,
// NOT the runtime `java.lang.Deprecated` annotation — so Java's
// `Class.isAnnotationPresent(classOf[java.lang.Deprecated])` returns false
// even when the symbol IS annotated. ScalaSignature carries the annotation
// metadata that Scala-side reflection can see. (scala-reflect is on the test
// classpath transitively via scalatest-core.)
package runtime

import org.scalatest.funsuite.AnyFunSuite

import scala.reflect.runtime.universe.*

class ForeignKeyCodecDeprecationSpec extends AnyFunSuite {

  // The v1.0.0 fixture: testpkg.pkg0 v1 emits ObscureInt_KeyCodec because
  // ObscureInt is a Custom non-stringy foreign (java.lang.Integer). v1.0.0
  // is non-latest (latest is v3.0.0), so obsoletePrevious annotates emitted
  // declarations with @deprecated.
  private val mirror = runtimeMirror(getClass.getClassLoader)

  private def hasDeprecated(sym: Symbol): Boolean = {
    val deprecatedTpe = typeOf[scala.deprecated]
    sym.annotations.exists(_.tree.tpe =:= deprecatedTpe)
  }

  test("PR-I-D04: trait <Foreign>_KeyCodec on a non-latest version carries @deprecated") {
    val traitSym = mirror.staticClass("testpkg.pkg0.v1_0_0.ObscureInt_KeyCodec")
    // Trigger lazy initialization of annotations.
    traitSym.info
    assert(
      hasDeprecated(traitSym),
      s"$traitSym should carry @deprecated on non-latest version (v1.0.0 of testpkg.pkg0); " +
      s"annotations seen: ${traitSym.annotations}",
    )
  }

  test("PR-I-D04: companion object <Foreign>_KeyCodec on a non-latest version also carries @deprecated") {
    val moduleSym = mirror.staticModule("testpkg.pkg0.v1_0_0.ObscureInt_KeyCodec")
    // Trigger lazy initialization of annotations.
    moduleSym.info
    assert(
      hasDeprecated(moduleSym),
      s"$moduleSym should ALSO carry @deprecated on non-latest version " +
      s"(PR-I-D04: pre-fix, only the trait was annotated; the companion was not); " +
      s"annotations seen: ${moduleSym.annotations}",
    )
  }
}
