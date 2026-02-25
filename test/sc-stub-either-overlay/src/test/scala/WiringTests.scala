import baboon.runtime.shared._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MockI1Either extends testpkg.pkg0.I1 {
  def testCall(arg: testpkg.pkg0.i1.testcall.In): Either[testpkg.pkg0.i1.testcall.Err, testpkg.pkg0.i1.testcall.Out] =
    Right(testpkg.pkg0.i1.testcall.Out(i00 = 42))

  def testCall2(arg: testpkg.pkg0.T7_Empty): Either[testpkg.pkg0.T7_Empty, testpkg.pkg0.T7_Empty] =
    Right(testpkg.pkg0.T7_Empty())
}

class FailingI1Either extends testpkg.pkg0.I1 {
  def testCall(arg: testpkg.pkg0.i1.testcall.In): Either[testpkg.pkg0.i1.testcall.Err, testpkg.pkg0.i1.testcall.Out] =
    Left(testpkg.pkg0.i1.testcall.Err(msg = "domain error"))

  def testCall2(arg: testpkg.pkg0.T7_Empty): Either[testpkg.pkg0.T7_Empty, testpkg.pkg0.T7_Empty] =
    Left(testpkg.pkg0.T7_Empty())
}

class ThrowingI1Either extends testpkg.pkg0.I1 {
  def testCall(arg: testpkg.pkg0.i1.testcall.In): Either[testpkg.pkg0.i1.testcall.Err, testpkg.pkg0.i1.testcall.Out] =
    throw new RuntimeException("service error")

  def testCall2(arg: testpkg.pkg0.T7_Empty): Either[testpkg.pkg0.T7_Empty, testpkg.pkg0.T7_Empty] =
    throw new RuntimeException("service error")
}

class MockI2Either extends testpkg.pkg0.I2 {
  def noErrCall(arg: testpkg.pkg0.i2.noerrcall.In): testpkg.pkg0.i2.noerrcall.Out =
    testpkg.pkg0.i2.noerrcall.Out(result = "result_" + arg.value.toString)
}

class EitherWiringTests extends AnyFlatSpec with Matchers {

  val ctx: BaboonCodecContext           = BaboonCodecContext.Default
  val rt: testpkg.pkg0.IBaboonServiceRt = testpkg.pkg0.BaboonServiceRtDefault

  // ==================== I1 JSON Tests ====================

  "I1Wiring.invokeJson testCall" should "return Right for success" in {
    val impl   = new MockI1Either()
    val method = BaboonMethodId("I1", "testCall")
    val inputJson = testpkg.pkg0.i1.testcall.In_JsonCodec.instance
      .encode(ctx, testpkg.pkg0.i1.testcall.In()).noSpaces

    val result = testpkg.pkg0.I1Wiring.invokeJson(method, inputJson, impl, rt, ctx)

    result match {
      case Right(jsonStr) =>
        val wire = io.circe.parser.parse(jsonStr).fold(throw _, identity)
        val decoded = testpkg.pkg0.i1.testcall.Out_JsonCodec.instance
          .decode(ctx, wire)
          .fold(ex => fail(ex.toString), identity)
        decoded.i00 shouldBe 42
      case Left(err) => fail(s"Expected Right, got Left($err)")
    }
  }

  "I1Wiring.invokeJson testCall2" should "return Right for success" in {
    val impl   = new MockI1Either()
    val method = BaboonMethodId("I1", "testCall2")
    val inputJson = testpkg.pkg0.T7_Empty_JsonCodec.instance
      .encode(ctx, testpkg.pkg0.T7_Empty()).noSpaces

    val result = testpkg.pkg0.I1Wiring.invokeJson(method, inputJson, impl, rt, ctx)

    result shouldBe a[Right[_, _]]
  }

  "I1Wiring.invokeJson" should "return Left(CallFailed) for domain error" in {
    val impl   = new FailingI1Either()
    val method = BaboonMethodId("I1", "testCall")
    val inputJson = testpkg.pkg0.i1.testcall.In_JsonCodec.instance
      .encode(ctx, testpkg.pkg0.i1.testcall.In()).noSpaces

    val result = testpkg.pkg0.I1Wiring.invokeJson(method, inputJson, impl, rt, ctx)

    result match {
      case Left(err) => err shouldBe a[BaboonWiringError.CallFailed]
      case Right(_)  => fail("Expected Left(CallFailed)")
    }
  }

  "I1Wiring.invokeJson" should "return Left(NoMatchingMethod) for unknown method" in {
    val impl   = new MockI1Either()
    val method = BaboonMethodId("I1", "nonexistent")

    val result = testpkg.pkg0.I1Wiring.invokeJson(method, "{}", impl, rt, ctx)

    result match {
      case Left(err) => err shouldBe a[BaboonWiringError.NoMatchingMethod]
      case Right(_)  => fail("Expected Left(NoMatchingMethod)")
    }
  }

  "I1Wiring.invokeJson" should "return Left(DecoderFailed) for bad input" in {
    val impl   = new MockI1Either()
    val method = BaboonMethodId("I1", "testCall")

    val result = testpkg.pkg0.I1Wiring.invokeJson(method, "not valid json!!", impl, rt, ctx)

    result match {
      case Left(err) => err shouldBe a[BaboonWiringError.DecoderFailed]
      case Right(_)  => fail("Expected Left(DecoderFailed)")
    }
  }

  "I1Wiring.invokeJson" should "return Left(CallFailed) when service throws" in {
    val impl   = new ThrowingI1Either()
    val method = BaboonMethodId("I1", "testCall")
    val inputJson = testpkg.pkg0.i1.testcall.In_JsonCodec.instance
      .encode(ctx, testpkg.pkg0.i1.testcall.In()).noSpaces

    val result = testpkg.pkg0.I1Wiring.invokeJson(method, inputJson, impl, rt, ctx)

    result match {
      case Left(err) => err shouldBe a[BaboonWiringError.CallFailed]
      case Right(_)  => fail("Expected Left(CallFailed)")
    }
  }

  // ==================== I1 UEBA Tests ====================

  "I1Wiring.invokeUeba testCall" should "return Right for success" in {
    val impl   = new MockI1Either()
    val method = BaboonMethodId("I1", "testCall")

    val oms = new java.io.ByteArrayOutputStream()
    val bw  = new baboon.runtime.shared.LEDataOutputStream(oms)
    testpkg.pkg0.i1.testcall.In_BinCodec.instance.encode(ctx, bw, testpkg.pkg0.i1.testcall.In())
    bw.flush()
    val inputBytes = oms.toByteArray

    val result = testpkg.pkg0.I1Wiring.invokeUeba(method, inputBytes, impl, rt, ctx)

    result match {
      case Right(outputBytes) =>
        val ims = new java.io.ByteArrayInputStream(outputBytes)
        val br  = new baboon.runtime.shared.LEDataInputStream(ims)
        val decoded = testpkg.pkg0.i1.testcall.Out_BinCodec.instance
          .decode(ctx, br)
          .fold(ex => fail(ex.toString), identity)
        decoded.i00 shouldBe 42
      case Left(err) => fail(s"Expected Right, got Left($err)")
    }
  }

  "I1Wiring.invokeUeba" should "return Left(NoMatchingMethod) for unknown method" in {
    val impl   = new MockI1Either()
    val method = BaboonMethodId("I1", "nonexistent")

    val result = testpkg.pkg0.I1Wiring.invokeUeba(method, Array.emptyByteArray, impl, rt, ctx)

    result match {
      case Left(err) => err shouldBe a[BaboonWiringError.NoMatchingMethod]
      case Right(_)  => fail("Expected Left(NoMatchingMethod)")
    }
  }

  "I1Wiring.invokeUeba" should "return Left(CallFailed) when service throws" in {
    val impl   = new ThrowingI1Either()
    val method = BaboonMethodId("I1", "testCall")

    val oms = new java.io.ByteArrayOutputStream()
    val bw  = new baboon.runtime.shared.LEDataOutputStream(oms)
    testpkg.pkg0.i1.testcall.In_BinCodec.instance.encode(ctx, bw, testpkg.pkg0.i1.testcall.In())
    bw.flush()
    val inputBytes = oms.toByteArray

    val result = testpkg.pkg0.I1Wiring.invokeUeba(method, inputBytes, impl, rt, ctx)

    result match {
      case Left(err) => err shouldBe a[BaboonWiringError.CallFailed]
      case Right(_)  => fail("Expected Left(CallFailed)")
    }
  }

  // ==================== I2 Tests (no err type) ====================

  "I2Wiring.invokeJson noErrCall" should "return Right for success" in {
    val impl   = new MockI2Either()
    val method = BaboonMethodId("I2", "noErrCall")
    val inputJson = testpkg.pkg0.i2.noerrcall.In_JsonCodec.instance
      .encode(ctx, testpkg.pkg0.i2.noerrcall.In(value = 123)).noSpaces

    val result = testpkg.pkg0.I2Wiring.invokeJson(method, inputJson, impl, rt, ctx)

    result match {
      case Right(jsonStr) =>
        val wire = io.circe.parser.parse(jsonStr).fold(throw _, identity)
        val decoded = testpkg.pkg0.i2.noerrcall.Out_JsonCodec.instance
          .decode(ctx, wire)
          .fold(ex => fail(ex.toString), identity)
        decoded.result shouldBe "result_123"
      case Left(err) => fail(s"Expected Right, got Left($err)")
    }
  }

  "I2Wiring.invokeUeba noErrCall" should "return Right for success" in {
    val impl   = new MockI2Either()
    val method = BaboonMethodId("I2", "noErrCall")

    val oms = new java.io.ByteArrayOutputStream()
    val bw  = new baboon.runtime.shared.LEDataOutputStream(oms)
    testpkg.pkg0.i2.noerrcall.In_BinCodec.instance.encode(ctx, bw, testpkg.pkg0.i2.noerrcall.In(value = 456))
    bw.flush()
    val inputBytes = oms.toByteArray

    val result = testpkg.pkg0.I2Wiring.invokeUeba(method, inputBytes, impl, rt, ctx)

    result match {
      case Right(outputBytes) =>
        val ims = new java.io.ByteArrayInputStream(outputBytes)
        val br  = new baboon.runtime.shared.LEDataInputStream(ims)
        val decoded = testpkg.pkg0.i2.noerrcall.Out_BinCodec.instance
          .decode(ctx, br)
          .fold(ex => fail(ex.toString), identity)
        decoded.result shouldBe "result_456"
      case Left(err) => fail(s"Expected Right, got Left($err)")
    }
  }
}
