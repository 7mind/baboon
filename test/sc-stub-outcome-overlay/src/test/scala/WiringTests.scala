import baboon.runtime.shared._
import containers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MockI1Outcome extends testpkg.pkg0.I1 {
  def testCall(arg: testpkg.pkg0.i1.testcall.In): Outcome[testpkg.pkg0.i1.testcall.Out] =
    Outcome.Success(testpkg.pkg0.i1.testcall.Out(i00 = 42))

  def testCall2(arg: testpkg.pkg0.T7_Empty): Outcome[testpkg.pkg0.T7_Empty] =
    Outcome.Success(testpkg.pkg0.T7_Empty())
}

class FailingI1Outcome extends testpkg.pkg0.I1 {
  def testCall(arg: testpkg.pkg0.i1.testcall.In): Outcome[testpkg.pkg0.i1.testcall.Out] =
    Outcome.Failure(testpkg.pkg0.i1.testcall.Err(msg = "domain error"))

  def testCall2(arg: testpkg.pkg0.T7_Empty): Outcome[testpkg.pkg0.T7_Empty] =
    Outcome.Failure(testpkg.pkg0.T7_Empty())
}

class ThrowingI1Outcome extends testpkg.pkg0.I1 {
  def testCall(arg: testpkg.pkg0.i1.testcall.In): Outcome[testpkg.pkg0.i1.testcall.Out] =
    throw new RuntimeException("service error")

  def testCall2(arg: testpkg.pkg0.T7_Empty): Outcome[testpkg.pkg0.T7_Empty] =
    throw new RuntimeException("service error")
}

class MockI2Outcome extends testpkg.pkg0.I2 {
  def noErrCall(arg: testpkg.pkg0.i2.noerrcall.In): testpkg.pkg0.i2.noerrcall.Out =
    testpkg.pkg0.i2.noerrcall.Out(result = "result_" + arg.value.toString)
}

class OutcomeWiringTests extends AnyFlatSpec with Matchers {

  val ctx: BaboonCodecContext           = BaboonCodecContext.Default
  val rt: testpkg.pkg0.IBaboonServiceRt = OutcomeServiceRt.instance

  // ==================== I1 JSON Tests ====================

  "I1Wiring.invokeJson testCall" should "return Success for success" in {
    val impl   = new MockI1Outcome()
    val method = BaboonMethodId("I1", "testCall")
    val inputJson = testpkg.pkg0.i1.testcall.In_JsonCodec.instance
      .encode(ctx, testpkg.pkg0.i1.testcall.In()).noSpaces

    val result = testpkg.pkg0.I1Wiring.invokeJson(method, inputJson, impl, rt, ctx)

    result match {
      case Outcome.Success(jsonStr) =>
        val wire = io.circe.parser.parse(jsonStr).fold(throw _, identity)
        val decoded = testpkg.pkg0.i1.testcall.Out_JsonCodec.instance
          .decode(ctx, wire)
          .fold(ex => fail(ex.toString), identity)
        decoded.i00 shouldBe 42
      case Outcome.Failure(err) => fail(s"Expected Success, got Failure($err)")
    }
  }

  "I1Wiring.invokeJson testCall2" should "return Success for success" in {
    val impl   = new MockI1Outcome()
    val method = BaboonMethodId("I1", "testCall2")
    val inputJson = testpkg.pkg0.T7_Empty_JsonCodec.instance
      .encode(ctx, testpkg.pkg0.T7_Empty()).noSpaces

    val result = testpkg.pkg0.I1Wiring.invokeJson(method, inputJson, impl, rt, ctx)

    result shouldBe a[Outcome.Success[_]]
  }

  "I1Wiring.invokeJson" should "return Failure(CallFailed) for domain error" in {
    val impl   = new FailingI1Outcome()
    val method = BaboonMethodId("I1", "testCall")
    val inputJson = testpkg.pkg0.i1.testcall.In_JsonCodec.instance
      .encode(ctx, testpkg.pkg0.i1.testcall.In()).noSpaces

    val result = testpkg.pkg0.I1Wiring.invokeJson(method, inputJson, impl, rt, ctx)

    result match {
      case Outcome.Failure(err) => err shouldBe a[BaboonWiringError.CallFailed]
      case Outcome.Success(_)   => fail("Expected Failure(CallFailed)")
    }
  }

  "I1Wiring.invokeJson" should "return Failure(NoMatchingMethod) for unknown method" in {
    val impl   = new MockI1Outcome()
    val method = BaboonMethodId("I1", "nonexistent")

    val result = testpkg.pkg0.I1Wiring.invokeJson(method, "{}", impl, rt, ctx)

    result match {
      case Outcome.Failure(err) => err shouldBe a[BaboonWiringError.NoMatchingMethod]
      case Outcome.Success(_)   => fail("Expected Failure(NoMatchingMethod)")
    }
  }

  "I1Wiring.invokeJson" should "return Failure(DecoderFailed) for bad input" in {
    val impl   = new MockI1Outcome()
    val method = BaboonMethodId("I1", "testCall")

    val result = testpkg.pkg0.I1Wiring.invokeJson(method, "not valid json!!", impl, rt, ctx)

    result match {
      case Outcome.Failure(err) => err shouldBe a[BaboonWiringError.DecoderFailed]
      case Outcome.Success(_)   => fail("Expected Failure(DecoderFailed)")
    }
  }

  "I1Wiring.invokeJson" should "return Failure(CallFailed) when service throws" in {
    val impl   = new ThrowingI1Outcome()
    val method = BaboonMethodId("I1", "testCall")
    val inputJson = testpkg.pkg0.i1.testcall.In_JsonCodec.instance
      .encode(ctx, testpkg.pkg0.i1.testcall.In()).noSpaces

    val result = testpkg.pkg0.I1Wiring.invokeJson(method, inputJson, impl, rt, ctx)

    result match {
      case Outcome.Failure(err) => err shouldBe a[BaboonWiringError.CallFailed]
      case Outcome.Success(_)   => fail("Expected Failure(CallFailed)")
    }
  }

  // ==================== I1 UEBA Tests ====================

  "I1Wiring.invokeUeba testCall" should "return Success for success" in {
    val impl   = new MockI1Outcome()
    val method = BaboonMethodId("I1", "testCall")

    val oms = new java.io.ByteArrayOutputStream()
    val bw  = new baboon.runtime.shared.LEDataOutputStream(oms)
    testpkg.pkg0.i1.testcall.In_BinCodec.instance.encode(ctx, bw, testpkg.pkg0.i1.testcall.In())
    bw.flush()
    val inputBytes = oms.toByteArray

    val result = testpkg.pkg0.I1Wiring.invokeUeba(method, inputBytes, impl, rt, ctx)

    result match {
      case Outcome.Success(outputBytes) =>
        val ims = new java.io.ByteArrayInputStream(outputBytes)
        val br  = new baboon.runtime.shared.LEDataInputStream(ims)
        val decoded = testpkg.pkg0.i1.testcall.Out_BinCodec.instance
          .decode(ctx, br)
          .fold(ex => fail(ex.toString), identity)
        decoded.i00 shouldBe 42
      case Outcome.Failure(err) => fail(s"Expected Success, got Failure($err)")
    }
  }

  "I1Wiring.invokeUeba" should "return Failure(NoMatchingMethod) for unknown method" in {
    val impl   = new MockI1Outcome()
    val method = BaboonMethodId("I1", "nonexistent")

    val result = testpkg.pkg0.I1Wiring.invokeUeba(method, Array.emptyByteArray, impl, rt, ctx)

    result match {
      case Outcome.Failure(err) => err shouldBe a[BaboonWiringError.NoMatchingMethod]
      case Outcome.Success(_)   => fail("Expected Failure(NoMatchingMethod)")
    }
  }

  "I1Wiring.invokeUeba" should "return Failure(CallFailed) when service throws" in {
    val impl   = new ThrowingI1Outcome()
    val method = BaboonMethodId("I1", "testCall")

    val oms = new java.io.ByteArrayOutputStream()
    val bw  = new baboon.runtime.shared.LEDataOutputStream(oms)
    testpkg.pkg0.i1.testcall.In_BinCodec.instance.encode(ctx, bw, testpkg.pkg0.i1.testcall.In())
    bw.flush()
    val inputBytes = oms.toByteArray

    val result = testpkg.pkg0.I1Wiring.invokeUeba(method, inputBytes, impl, rt, ctx)

    result match {
      case Outcome.Failure(err) => err shouldBe a[BaboonWiringError.CallFailed]
      case Outcome.Success(_)   => fail("Expected Failure(CallFailed)")
    }
  }

  // ==================== I2 Tests (no err type) ====================

  "I2Wiring.invokeJson noErrCall" should "return Success for success" in {
    val impl   = new MockI2Outcome()
    val method = BaboonMethodId("I2", "noErrCall")
    val inputJson = testpkg.pkg0.i2.noerrcall.In_JsonCodec.instance
      .encode(ctx, testpkg.pkg0.i2.noerrcall.In(value = 123)).noSpaces

    val result = testpkg.pkg0.I2Wiring.invokeJson(method, inputJson, impl, rt, ctx)

    result match {
      case Outcome.Success(jsonStr) =>
        val wire = io.circe.parser.parse(jsonStr).fold(throw _, identity)
        val decoded = testpkg.pkg0.i2.noerrcall.Out_JsonCodec.instance
          .decode(ctx, wire)
          .fold(ex => fail(ex.toString), identity)
        decoded.result shouldBe "result_123"
      case Outcome.Failure(err) => fail(s"Expected Success, got Failure($err)")
    }
  }

  "I2Wiring.invokeUeba noErrCall" should "return Success for success" in {
    val impl   = new MockI2Outcome()
    val method = BaboonMethodId("I2", "noErrCall")

    val oms = new java.io.ByteArrayOutputStream()
    val bw  = new baboon.runtime.shared.LEDataOutputStream(oms)
    testpkg.pkg0.i2.noerrcall.In_BinCodec.instance.encode(ctx, bw, testpkg.pkg0.i2.noerrcall.In(value = 456))
    bw.flush()
    val inputBytes = oms.toByteArray

    val result = testpkg.pkg0.I2Wiring.invokeUeba(method, inputBytes, impl, rt, ctx)

    result match {
      case Outcome.Success(outputBytes) =>
        val ims = new java.io.ByteArrayInputStream(outputBytes)
        val br  = new baboon.runtime.shared.LEDataInputStream(ims)
        val decoded = testpkg.pkg0.i2.noerrcall.Out_BinCodec.instance
          .decode(ctx, br)
          .fold(ex => fail(ex.toString), identity)
        decoded.result shouldBe "result_456"
      case Outcome.Failure(err) => fail(s"Expected Success, got Failure($err)")
    }
  }
}
