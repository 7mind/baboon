// NOTE: This test references generated symbols (my.ok.Inner, my.ok.Holder,
// my.ok.BaboonMetadata) which are copied into this stub only by
// `mdl :build :test-gen-regular-adt` (rsync + codegen into target/test-regular/sc-stub/).
// Running `sbt test` directly from the source tree will fail with missing symbols;
// run the test suite from the codegen'd copy.
package runtime

import baboon.runtime.shared._
import baboon.runtime.shared.BaboonExt._
import org.scalatest.funsuite.AnyFunSuite

class BaboonExtSpec extends AnyFunSuite {

  private val inner: my.ok.Inner = my.ok.Inner(7)

  test("BaboonGeneratedExt.baboonUnmodifiedSinceVersion returns head of baboonSameInVersions") {
    val result = inner.baboonUnmodifiedSinceVersion
    assert(result == inner.baboonSameInVersions.head)
  }

  test("BaboonGenerated.domainVersion returns BaboonDomainVersion(identifier, version)") {
    val dv = inner.domainVersion
    assert(dv == BaboonDomainVersion(inner.baboonDomainIdentifier, inner.baboonDomainVersion))
  }

  test("BaboonMetaExt.unmodifiedSinceVersion returns head of meta.sameInVersions(typeId)") {
    val meta   = my.ok.BaboonMetadata
    val typeId = inner.baboonTypeIdentifier
    val result = meta.unmodifiedSinceVersion(typeId)
    assert(result == meta.sameInVersions(typeId).head)
  }
}
