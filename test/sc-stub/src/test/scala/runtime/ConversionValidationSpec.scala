package runtime

import baboon.runtime.shared._
import org.scalatest.funsuite.AnyFunSuite

class ConversionValidationSpec extends AnyFunSuite {
  private class Value(val baboonTypeIdentifier: String) extends BaboonGenerated {
    override val baboonDomainVersion     = "1.0.0"
    override val baboonDomainIdentifier  = "test"
    override val baboonSameInVersions    = List("1.0.0")
    override val baboonForwardReadable   = Map.empty[String, String]
    override val baboonMinReaderVersions = Map.empty[String, String]
  }
  private class Branch(id: String, val baboonAdtTypeIdentifier: String) extends Value(id) with BaboonAdtMemberMeta {
    override val baboonAdtType: Class[?] = classOf[Value]
  }
  private val conversions = new AbstractBaboonConversions {
    override val versionsFrom = List("1.0.0")
    override val versionTo    = "2.0.0"
  }
  private class Legacy(val typeId: String, result: Value) extends AbstractConversion[Value, Value] {
    var calls                = 0
    override val versionFrom = "1.0.0"
    override val versionTo   = "2.0.0"
    override def doConvert[C](context: Option[C], conversions: AbstractBaboonConversions, from: Value): Value = {
      calls += 1
      result
    }
  }
  private class Renamed(source: String, target: String, result: Value) extends Legacy(source, result) {
    var targetReads                             = 0
    override protected def targetTypeId: String = { targetReads += 1; target }
  }

  test("legacy converters retain same-identifier validation and call ordering") {
    val good = new Legacy("same", new Value("same"))
    assert(good.convert[Unit](None, conversions, new Value("same")).baboonTypeIdentifier == "same")
    val wrongSource = new Legacy("same", new Value("same"))
    intercept[IllegalArgumentException](wrongSource.convert[Unit](None, conversions, new Value("wrong")))
    assert(wrongSource.calls == 0)
    val wrongResult = new Legacy("same", new Value("wrong"))
    intercept[IllegalArgumentException](wrongResult.convert[Unit](None, conversions, new Value("same")))
    assert(wrongResult.calls == 1)
  }

  test("renamed result uses target identifier without eager hook evaluation") {
    val conversion = new Renamed("old", "new", new Value("new"))
    assert(conversion.targetReads == 0)
    intercept[IllegalArgumentException](conversion.convert[Unit](None, conversions, new Value("wrong")))
    assert(conversion.calls == 0 && conversion.targetReads == 0)
    assert(conversion.convert[Unit](None, conversions, new Value("old")).baboonTypeIdentifier == "new")
    assert(conversion.calls == 1 && conversion.targetReads > 0)
  }

  test("renamed result rejects source and unrelated identifiers") {
    List("old", "unrelated").foreach {
      id =>
        val conversion = new Renamed("old", "new", new Value(id))
        intercept[IllegalArgumentException](conversion.convert[Unit](None, conversions, new Value("old")))
        assert(conversion.calls == 1)
    }
  }

  test("ADT validation accepts parent or exact branch and rejects neither") {
    val parent = new Renamed("oldParent", "newParent", new Branch("newBranch", "newParent"))
    assert(parent.convert[Unit](None, conversions, new Branch("oldBranch", "oldParent")).isInstanceOf[Branch])
    val exact = new Renamed("oldBranch", "newBranch", new Branch("newBranch", "otherParent"))
    assert(exact.convert[Unit](None, conversions, new Branch("oldBranch", "otherParent")).isInstanceOf[Branch])
    val wrong = new Renamed("oldParent", "newParent", new Branch("wrongBranch", "wrongParent"))
    intercept[IllegalArgumentException](wrong.convert[Unit](None, conversions, new Branch("oldBranch", "oldParent")))
  }

  test("foreign conversions do not evaluate metadata hooks") {
    val conversion = new AbstractConversion[String, String] {
      override def typeId: String                 = throw new AssertionError("source metadata must not be read")
      override protected def targetTypeId: String = throw new AssertionError("target metadata must not be read")
      override val versionFrom                    = "1.0.0"
      override val versionTo                      = "2.0.0"
      override def doConvert[C](context: Option[C], conversions: AbstractBaboonConversions, from: String): String = from.toUpperCase(java.util.Locale.ROOT)
    }
    assert(conversion.convert[Unit](None, conversions, "value") == "VALUE")
  }
}
