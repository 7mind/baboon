package runtime

import baboon.runtime.shared.Lazy
import org.scalatest.funsuite.AnyFunSuite

class GeneratedMetadataReuseSpec extends AnyFunSuite {
  test("generated codecs retain one deferred singleton wrapper") {
    List(my.ok.Inner_JsonCodec, my.ok.Inner_UEBACodec).foreach {
      codec =>
        val accessor = codec.getClass.getDeclaredMethod("LazyInstance")
        accessor.setAccessible(true)
        val first  = accessor.invoke(codec).asInstanceOf[Lazy[AnyRef]]
        val second = accessor.invoke(codec).asInstanceOf[Lazy[AnyRef]]
        assert(first eq second)
        assert(first.value eq codec)
    }
  }

  test("metadata methods retain immutable collections and instance forwarding") {
    val value = my.ok.Inner(7)
    assert(my.ok.Inner.baboonSameInVersions eq my.ok.Inner.baboonSameInVersions)
    assert(my.ok.Inner.baboonForwardReadable eq my.ok.Inner.baboonForwardReadable)
    assert(my.ok.Inner.baboonMinReaderVersions eq my.ok.Inner.baboonMinReaderVersions)
    assert(value.baboonSameInVersions eq my.ok.Inner.baboonSameInVersions)
    assert(value.baboonForwardReadable eq my.ok.Inner.baboonForwardReadable)
    assert(value.baboonMinReaderVersions eq my.ok.Inner.baboonMinReaderVersions)
    assert(value.baboonSameInVersions.contains(value.baboonDomainVersion))
  }
}
