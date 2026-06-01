import baboon.runtime.shared._
import containers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MockI1Result extends testpkg.pkg0.I1 {
  def testCall(arg: testpkg.pkg0.i1.testcall.In): Result[testpkg.pkg0.i1.testcall.Out, testpkg.pkg0.i1.testcall.Err] =
    Result.Success(testpkg.pkg0.i1.testcall.Out(i00 = 42))

  def testCall2(arg: testpkg.pkg0.T7_Empty): Result[testpkg.pkg0.T7_Empty, testpkg.pkg0.T7_Empty] =
    Result.Success(testpkg.pkg0.T7_Empty())
}

class FailingI1Result extends testpkg.pkg0.I1 {
  def testCall(arg: testpkg.pkg0.i1.testcall.In): Result[testpkg.pkg0.i1.testcall.Out, testpkg.pkg0.i1.testcall.Err] =
    Result.Failure(testpkg.pkg0.i1.testcall.Err(msg = "domain error"))

  def testCall2(arg: testpkg.pkg0.T7_Empty): Result[testpkg.pkg0.T7_Empty, testpkg.pkg0.T7_Empty] =
    Result.Failure(testpkg.pkg0.T7_Empty())
}

class ThrowingI1Result extends testpkg.pkg0.I1 {
  def testCall(arg: testpkg.pkg0.i1.testcall.In): Result[testpkg.pkg0.i1.testcall.Out, testpkg.pkg0.i1.testcall.Err] =
    throw new RuntimeException("service error")

  def testCall2(arg: testpkg.pkg0.T7_Empty): Result[testpkg.pkg0.T7_Empty, testpkg.pkg0.T7_Empty] =
    throw new RuntimeException("service error")
}

class MockI2Result extends testpkg.pkg0.I2 {
  def noErrCall(arg: testpkg.pkg0.i2.noerrcall.In): testpkg.pkg0.i2.noerrcall.Out =
    testpkg.pkg0.i2.noerrcall.Out(result = "result_" + arg.value.toString)
}

class ResultWiringTests extends AnyFlatSpec with Matchers {

  val ctx: BaboonCodecContext           = BaboonCodecContext.Default
  val rt: testpkg.pkg0.IBaboonServiceRt = ResultServiceRt.instance

  // ==================== I1 JSON Tests ====================

  "I1Wiring.invokeJson testCall" should "return Success for success" in {
    val impl   = new MockI1Result()
    val method = BaboonMethodId("I1", "testCall")
    val inputJson = testpkg.pkg0.i1.testcall.In_JsonCodec.instance
      .encode(ctx, testpkg.pkg0.i1.testcall.In()).noSpaces

    val result = testpkg.pkg0.I1Wiring.invokeJson(method, inputJson, impl, rt, ctx)

    result match {
      case Result.Success(jsonStr) =>
        val wire = io.circe.parser.parse(jsonStr).fold(throw _, identity)
        val decoded = testpkg.pkg0.i1.testcall.Out_JsonCodec.instance
          .decode(ctx, wire)
          .fold(ex => fail(ex.toString), identity)
        decoded.i00 shouldBe 42
      case Result.Failure(err) => fail(s"Expected Success, got Failure($err)")
    }
  }

  "I1Wiring.invokeJson testCall2" should "return Success for success" in {
    val impl   = new MockI1Result()
    val method = BaboonMethodId("I1", "testCall2")
    val inputJson = testpkg.pkg0.T7_Empty_JsonCodec.instance
      .encode(ctx, testpkg.pkg0.T7_Empty()).noSpaces

    val result = testpkg.pkg0.I1Wiring.invokeJson(method, inputJson, impl, rt, ctx)

    result shouldBe a[Result.Success[_, _]]
  }

  "I1Wiring.invokeJson" should "return Failure(CallFailed) for domain error" in {
    val impl   = new FailingI1Result()
    val method = BaboonMethodId("I1", "testCall")
    val inputJson = testpkg.pkg0.i1.testcall.In_JsonCodec.instance
      .encode(ctx, testpkg.pkg0.i1.testcall.In()).noSpaces

    val result = testpkg.pkg0.I1Wiring.invokeJson(method, inputJson, impl, rt, ctx)

    result match {
      case Result.Failure(err) => err shouldBe a[BaboonWiringError.CallFailed]
      case Result.Success(_)   => fail("Expected Failure(CallFailed)")
    }
  }

  "I1Wiring.invokeJson" should "return Failure(NoMatchingMethod) for unknown method" in {
    val impl   = new MockI1Result()
    val method = BaboonMethodId("I1", "nonexistent")

    val result = testpkg.pkg0.I1Wiring.invokeJson(method, "{}", impl, rt, ctx)

    result match {
      case Result.Failure(err) => err shouldBe a[BaboonWiringError.NoMatchingMethod]
      case Result.Success(_)   => fail("Expected Failure(NoMatchingMethod)")
    }
  }

  "I1Wiring.invokeJson" should "return Failure(DecoderFailed) for bad input" in {
    val impl   = new MockI1Result()
    val method = BaboonMethodId("I1", "testCall")

    val result = testpkg.pkg0.I1Wiring.invokeJson(method, "not valid json!!", impl, rt, ctx)

    result match {
      case Result.Failure(err) => err shouldBe a[BaboonWiringError.DecoderFailed]
      case Result.Success(_)   => fail("Expected Failure(DecoderFailed)")
    }
  }

  "I1Wiring.invokeJson" should "return Failure(CallFailed) when service throws" in {
    val impl   = new ThrowingI1Result()
    val method = BaboonMethodId("I1", "testCall")
    val inputJson = testpkg.pkg0.i1.testcall.In_JsonCodec.instance
      .encode(ctx, testpkg.pkg0.i1.testcall.In()).noSpaces

    val result = testpkg.pkg0.I1Wiring.invokeJson(method, inputJson, impl, rt, ctx)

    result match {
      case Result.Failure(err) => err shouldBe a[BaboonWiringError.CallFailed]
      case Result.Success(_)   => fail("Expected Failure(CallFailed)")
    }
  }

  // ==================== I1 UEBA Tests ====================

  "I1Wiring.invokeUeba testCall" should "return Success for success" in {
    val impl   = new MockI1Result()
    val method = BaboonMethodId("I1", "testCall")

    val oms = new java.io.ByteArrayOutputStream()
    val bw  = new baboon.runtime.shared.LEDataOutputStream(oms)
    testpkg.pkg0.i1.testcall.In_UEBACodec.instance.encode(ctx, bw, testpkg.pkg0.i1.testcall.In())
    bw.flush()
    val inputBytes = oms.toByteArray

    val result = testpkg.pkg0.I1Wiring.invokeUeba(method, inputBytes, impl, rt, ctx)

    result match {
      case Result.Success(outputBytes) =>
        val ims = new java.io.ByteArrayInputStream(outputBytes)
        val br  = new baboon.runtime.shared.LEDataInputStream(ims)
        val decoded = testpkg.pkg0.i1.testcall.Out_UEBACodec.instance
          .decode(ctx, br)
          .fold(ex => fail(ex.toString), identity)
        decoded.i00 shouldBe 42
      case Result.Failure(err) => fail(s"Expected Success, got Failure($err)")
    }
  }

  "I1Wiring.invokeUeba" should "return Failure(NoMatchingMethod) for unknown method" in {
    val impl   = new MockI1Result()
    val method = BaboonMethodId("I1", "nonexistent")

    val result = testpkg.pkg0.I1Wiring.invokeUeba(method, Array.emptyByteArray, impl, rt, ctx)

    result match {
      case Result.Failure(err) => err shouldBe a[BaboonWiringError.NoMatchingMethod]
      case Result.Success(_)   => fail("Expected Failure(NoMatchingMethod)")
    }
  }

  "I1Wiring.invokeUeba" should "return Failure(CallFailed) when service throws" in {
    val impl   = new ThrowingI1Result()
    val method = BaboonMethodId("I1", "testCall")

    val oms = new java.io.ByteArrayOutputStream()
    val bw  = new baboon.runtime.shared.LEDataOutputStream(oms)
    testpkg.pkg0.i1.testcall.In_UEBACodec.instance.encode(ctx, bw, testpkg.pkg0.i1.testcall.In())
    bw.flush()
    val inputBytes = oms.toByteArray

    val result = testpkg.pkg0.I1Wiring.invokeUeba(method, inputBytes, impl, rt, ctx)

    result match {
      case Result.Failure(err) => err shouldBe a[BaboonWiringError.CallFailed]
      case Result.Success(_)   => fail("Expected Failure(CallFailed)")
    }
  }

  // ==================== I2 Tests (no err type) ====================

  "I2Wiring.invokeJson noErrCall" should "return Success for success" in {
    val impl   = new MockI2Result()
    val method = BaboonMethodId("I2", "noErrCall")
    val inputJson = testpkg.pkg0.i2.noerrcall.In_JsonCodec.instance
      .encode(ctx, testpkg.pkg0.i2.noerrcall.In(value = 123)).noSpaces

    val result = testpkg.pkg0.I2Wiring.invokeJson(method, inputJson, impl, rt, ctx)

    result match {
      case Result.Success(jsonStr) =>
        val wire = io.circe.parser.parse(jsonStr).fold(throw _, identity)
        val decoded = testpkg.pkg0.i2.noerrcall.Out_JsonCodec.instance
          .decode(ctx, wire)
          .fold(ex => fail(ex.toString), identity)
        decoded.result shouldBe "result_123"
      case Result.Failure(err) => fail(s"Expected Success, got Failure($err)")
    }
  }

  "I2Wiring.invokeUeba noErrCall" should "return Success for success" in {
    val impl   = new MockI2Result()
    val method = BaboonMethodId("I2", "noErrCall")

    val oms = new java.io.ByteArrayOutputStream()
    val bw  = new baboon.runtime.shared.LEDataOutputStream(oms)
    testpkg.pkg0.i2.noerrcall.In_UEBACodec.instance.encode(ctx, bw, testpkg.pkg0.i2.noerrcall.In(value = 456))
    bw.flush()
    val inputBytes = oms.toByteArray

    val result = testpkg.pkg0.I2Wiring.invokeUeba(method, inputBytes, impl, rt, ctx)

    result match {
      case Result.Success(outputBytes) =>
        val ims = new java.io.ByteArrayInputStream(outputBytes)
        val br  = new baboon.runtime.shared.LEDataInputStream(ims)
        val decoded = testpkg.pkg0.i2.noerrcall.Out_UEBACodec.instance
          .decode(ctx, br)
          .fold(ex => fail(ex.toString), identity)
        decoded.result shouldBe "result_456"
      case Result.Failure(err) => fail(s"Expected Success, got Failure($err)")
    }
  }

  // ==================== Client round-trip (concrete Result container) ====================
  // The generated ${Svc}Client decodes the transport response into
  // Result[Out, BaboonWiringError] via the same IBaboonServiceRt the server
  // wiring uses. The container has no HKT, so the client class is not generic.

  private def jsonTransport(impl: testpkg.pkg0.I1): (String, String, String) => String =
    (svc, method, data) =>
      testpkg.pkg0.I1Wiring.invokeJson(BaboonMethodId(svc, method), data, impl, rt, ctx) match {
        case Result.Success(wire) => wire
        case Result.Failure(err)  => throw new BaboonWiringException(err.asInstanceOf[BaboonWiringError])
      }

  private def uebaTransport(impl: testpkg.pkg0.I1): (String, String, Array[Byte]) => Array[Byte] =
    (svc, method, data) =>
      testpkg.pkg0.I1Wiring.invokeUeba(BaboonMethodId(svc, method), data, impl, rt, ctx) match {
        case Result.Success(wire) => wire
        case Result.Failure(err)  => throw new BaboonWiringException(err.asInstanceOf[BaboonWiringError])
      }

  "I1Client.testCallJson" should "round-trip into Result.Success" in {
    val impl   = new MockI1Result()
    val client = new testpkg.pkg0.I1Client(uebaTransport(impl), jsonTransport(impl), rt, ctx)

    client.testCallJson(testpkg.pkg0.i1.testcall.In()) match {
      case Result.Success(out) => out.i00 shouldBe 42
      case Result.Failure(err) => fail(s"Expected Success, got Failure($err)")
    }
  }

  "I1Client.testCall (UEBA)" should "round-trip into Result.Success" in {
    val impl   = new MockI1Result()
    val client = new testpkg.pkg0.I1Client(uebaTransport(impl), jsonTransport(impl), rt, ctx)

    client.testCall(testpkg.pkg0.i1.testcall.In()) match {
      case Result.Success(out) => out.i00 shouldBe 42
      case Result.Failure(err) => fail(s"Expected Success, got Failure($err)")
    }
  }
}
