import baboon.runtime.shared._
import custom._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MockI1Hkt extends testpkg.pkg0.I1[MyBi] {
  def testCall(arg: testpkg.pkg0.i1.testcall.In): MyBi[testpkg.pkg0.i1.testcall.Err, testpkg.pkg0.i1.testcall.Out] =
    MyBi.Good(testpkg.pkg0.i1.testcall.Out(i00 = 42))

  def testCall2(arg: testpkg.pkg0.T7_Empty): MyBi[testpkg.pkg0.T7_Empty, testpkg.pkg0.T7_Empty] =
    MyBi.Good(testpkg.pkg0.T7_Empty())
}

class FailingI1Hkt extends testpkg.pkg0.I1[MyBi] {
  def testCall(arg: testpkg.pkg0.i1.testcall.In): MyBi[testpkg.pkg0.i1.testcall.Err, testpkg.pkg0.i1.testcall.Out] =
    MyBi.Bad(testpkg.pkg0.i1.testcall.Err(msg = "domain error"))

  def testCall2(arg: testpkg.pkg0.T7_Empty): MyBi[testpkg.pkg0.T7_Empty, testpkg.pkg0.T7_Empty] =
    MyBi.Bad(testpkg.pkg0.T7_Empty())
}

class ThrowingI1Hkt extends testpkg.pkg0.I1[MyBi] {
  def testCall(arg: testpkg.pkg0.i1.testcall.In): MyBi[testpkg.pkg0.i1.testcall.Err, testpkg.pkg0.i1.testcall.Out] =
    throw new RuntimeException("service error")

  def testCall2(arg: testpkg.pkg0.T7_Empty): MyBi[testpkg.pkg0.T7_Empty, testpkg.pkg0.T7_Empty] =
    throw new RuntimeException("service error")
}

class MockI2Hkt extends testpkg.pkg0.I2[MyBi] {
  def noErrCall(arg: testpkg.pkg0.i2.noerrcall.In): testpkg.pkg0.i2.noerrcall.Out =
    testpkg.pkg0.i2.noerrcall.Out(result = "result_" + arg.value.toString)
}

class HktWiringTests extends AnyFlatSpec with Matchers {

  val ctx: BaboonCodecContext                 = BaboonCodecContext.Default
  val rt: testpkg.pkg0.IBaboonServiceRt[MyBi] = MyBiServiceRt.instance

  // ==================== I1 JSON Tests ====================

  "I1Wiring.invokeJson testCall" should "return Good for success" in {
    val impl   = new MockI1Hkt()
    val method = BaboonMethodId("I1", "testCall")
    val inputJson = testpkg.pkg0.i1.testcall.In_JsonCodec.instance
      .encode(ctx, testpkg.pkg0.i1.testcall.In()).noSpaces

    val result = testpkg.pkg0.I1Wiring.invokeJson(method, inputJson, impl, rt, ctx)

    result match {
      case MyBi.Good(jsonStr) =>
        val wire = io.circe.parser.parse(jsonStr).fold(throw _, identity)
        val decoded = testpkg.pkg0.i1.testcall.Out_JsonCodec.instance
          .decode(ctx, wire)
          .fold(ex => fail(ex.toString), identity)
        decoded.i00 shouldBe 42
      case MyBi.Bad(err) => fail(s"Expected Good, got Bad($err)")
    }
  }

  "I1Wiring.invokeJson testCall2" should "return Good for success" in {
    val impl   = new MockI1Hkt()
    val method = BaboonMethodId("I1", "testCall2")
    val inputJson = testpkg.pkg0.T7_Empty_JsonCodec.instance
      .encode(ctx, testpkg.pkg0.T7_Empty()).noSpaces

    val result = testpkg.pkg0.I1Wiring.invokeJson(method, inputJson, impl, rt, ctx)

    result shouldBe a[MyBi.Good[_, _]]
  }

  "I1Wiring.invokeJson" should "return Bad(CallFailed) for domain error" in {
    val impl   = new FailingI1Hkt()
    val method = BaboonMethodId("I1", "testCall")
    val inputJson = testpkg.pkg0.i1.testcall.In_JsonCodec.instance
      .encode(ctx, testpkg.pkg0.i1.testcall.In()).noSpaces

    val result = testpkg.pkg0.I1Wiring.invokeJson(method, inputJson, impl, rt, ctx)

    result match {
      case MyBi.Bad(err) => err shouldBe a[BaboonWiringError.CallFailed]
      case MyBi.Good(_)  => fail("Expected Bad(CallFailed)")
    }
  }

  "I1Wiring.invokeJson" should "return Bad(NoMatchingMethod) for unknown method" in {
    val impl   = new MockI1Hkt()
    val method = BaboonMethodId("I1", "nonexistent")

    val result = testpkg.pkg0.I1Wiring.invokeJson(method, "{}", impl, rt, ctx)

    result match {
      case MyBi.Bad(err) => err shouldBe a[BaboonWiringError.NoMatchingMethod]
      case MyBi.Good(_)  => fail("Expected Bad(NoMatchingMethod)")
    }
  }

  "I1Wiring.invokeJson" should "return Bad(DecoderFailed) for bad input" in {
    val impl   = new MockI1Hkt()
    val method = BaboonMethodId("I1", "testCall")

    val result = testpkg.pkg0.I1Wiring.invokeJson(method, "not valid json!!", impl, rt, ctx)

    result match {
      case MyBi.Bad(err) => err shouldBe a[BaboonWiringError.DecoderFailed]
      case MyBi.Good(_)  => fail("Expected Bad(DecoderFailed)")
    }
  }

  "I1Wiring.invokeJson" should "return Bad(CallFailed) when service throws" in {
    val impl   = new ThrowingI1Hkt()
    val method = BaboonMethodId("I1", "testCall")
    val inputJson = testpkg.pkg0.i1.testcall.In_JsonCodec.instance
      .encode(ctx, testpkg.pkg0.i1.testcall.In()).noSpaces

    val result = testpkg.pkg0.I1Wiring.invokeJson(method, inputJson, impl, rt, ctx)

    result match {
      case MyBi.Bad(err) => err shouldBe a[BaboonWiringError.CallFailed]
      case MyBi.Good(_)  => fail("Expected Bad(CallFailed)")
    }
  }

  // ==================== I1 UEBA Tests ====================

  "I1Wiring.invokeUeba testCall" should "return Good for success" in {
    val impl   = new MockI1Hkt()
    val method = BaboonMethodId("I1", "testCall")

    val oms = new java.io.ByteArrayOutputStream()
    val bw  = new baboon.runtime.shared.LEDataOutputStream(oms)
    testpkg.pkg0.i1.testcall.In_UEBACodec.instance.encode(ctx, bw, testpkg.pkg0.i1.testcall.In())
    bw.flush()
    val inputBytes = oms.toByteArray

    val result = testpkg.pkg0.I1Wiring.invokeUeba(method, inputBytes, impl, rt, ctx)

    result match {
      case MyBi.Good(outputBytes) =>
        val ims = new java.io.ByteArrayInputStream(outputBytes)
        val br  = new baboon.runtime.shared.LEDataInputStream(ims)
        val decoded = testpkg.pkg0.i1.testcall.Out_UEBACodec.instance
          .decode(ctx, br)
          .fold(ex => fail(ex.toString), identity)
        decoded.i00 shouldBe 42
      case MyBi.Bad(err) => fail(s"Expected Good, got Bad($err)")
    }
  }

  "I1Wiring.invokeUeba" should "return Bad(NoMatchingMethod) for unknown method" in {
    val impl   = new MockI1Hkt()
    val method = BaboonMethodId("I1", "nonexistent")

    val result = testpkg.pkg0.I1Wiring.invokeUeba(method, Array.emptyByteArray, impl, rt, ctx)

    result match {
      case MyBi.Bad(err) => err shouldBe a[BaboonWiringError.NoMatchingMethod]
      case MyBi.Good(_)  => fail("Expected Bad(NoMatchingMethod)")
    }
  }

  "I1Wiring.invokeUeba" should "return Bad(CallFailed) when service throws" in {
    val impl   = new ThrowingI1Hkt()
    val method = BaboonMethodId("I1", "testCall")

    val oms = new java.io.ByteArrayOutputStream()
    val bw  = new baboon.runtime.shared.LEDataOutputStream(oms)
    testpkg.pkg0.i1.testcall.In_UEBACodec.instance.encode(ctx, bw, testpkg.pkg0.i1.testcall.In())
    bw.flush()
    val inputBytes = oms.toByteArray

    val result = testpkg.pkg0.I1Wiring.invokeUeba(method, inputBytes, impl, rt, ctx)

    result match {
      case MyBi.Bad(err) => err shouldBe a[BaboonWiringError.CallFailed]
      case MyBi.Good(_)  => fail("Expected Bad(CallFailed)")
    }
  }

  // ==================== I2 Tests (no err type) ====================

  "I2Wiring.invokeJson noErrCall" should "return Good for success" in {
    val impl   = new MockI2Hkt()
    val method = BaboonMethodId("I2", "noErrCall")
    val inputJson = testpkg.pkg0.i2.noerrcall.In_JsonCodec.instance
      .encode(ctx, testpkg.pkg0.i2.noerrcall.In(value = 123)).noSpaces

    val result = testpkg.pkg0.I2Wiring.invokeJson(method, inputJson, impl, rt, ctx)

    result match {
      case MyBi.Good(jsonStr) =>
        val wire = io.circe.parser.parse(jsonStr).fold(throw _, identity)
        val decoded = testpkg.pkg0.i2.noerrcall.Out_JsonCodec.instance
          .decode(ctx, wire)
          .fold(ex => fail(ex.toString), identity)
        decoded.result shouldBe "result_123"
      case MyBi.Bad(err) => fail(s"Expected Good, got Bad($err)")
    }
  }

  "I2Wiring.invokeUeba noErrCall" should "return Good for success" in {
    val impl   = new MockI2Hkt()
    val method = BaboonMethodId("I2", "noErrCall")

    val oms = new java.io.ByteArrayOutputStream()
    val bw  = new baboon.runtime.shared.LEDataOutputStream(oms)
    testpkg.pkg0.i2.noerrcall.In_UEBACodec.instance.encode(ctx, bw, testpkg.pkg0.i2.noerrcall.In(value = 456))
    bw.flush()
    val inputBytes = oms.toByteArray

    val result = testpkg.pkg0.I2Wiring.invokeUeba(method, inputBytes, impl, rt, ctx)

    result match {
      case MyBi.Good(outputBytes) =>
        val ims = new java.io.ByteArrayInputStream(outputBytes)
        val br  = new baboon.runtime.shared.LEDataInputStream(ims)
        val decoded = testpkg.pkg0.i2.noerrcall.Out_UEBACodec.instance
          .decode(ctx, br)
          .fold(ex => fail(ex.toString), identity)
        decoded.result shouldBe "result_456"
      case MyBi.Bad(err) => fail(s"Expected Good, got Bad($err)")
    }
  }

  // ==================== Client round-trip (HKT result container) ====================
  // The generated ${Svc}Client is generic over the user's HKT F and decodes
  // the transport response into F[BaboonWiringError, Out] via the same
  // IBaboonServiceRt the server wiring uses. The in-process transport routes
  // into ${Svc}Wiring and unwraps the MyBi.Good success wire.

  private def jsonTransport(impl: testpkg.pkg0.I1[MyBi]): (String, String, String) => String =
    (svc, method, data) =>
      testpkg.pkg0.I1Wiring.invokeJson(BaboonMethodId(svc, method), data, impl, rt, ctx) match {
        case MyBi.Good(wire) => wire
        case MyBi.Bad(err)   => throw new BaboonWiringException(err.asInstanceOf[BaboonWiringError])
      }

  private def uebaTransport(impl: testpkg.pkg0.I1[MyBi]): (String, String, Array[Byte]) => Array[Byte] =
    (svc, method, data) =>
      testpkg.pkg0.I1Wiring.invokeUeba(BaboonMethodId(svc, method), data, impl, rt, ctx) match {
        case MyBi.Good(wire) => wire
        case MyBi.Bad(err)   => throw new BaboonWiringException(err.asInstanceOf[BaboonWiringError])
      }

  "I1Client.testCallJson" should "round-trip into MyBi.Good" in {
    val impl   = new MockI1Hkt()
    val client = new testpkg.pkg0.I1Client[MyBi](uebaTransport(impl), jsonTransport(impl), rt, ctx)

    client.testCallJson(testpkg.pkg0.i1.testcall.In()) match {
      case MyBi.Good(out) => out.i00 shouldBe 42
      case MyBi.Bad(err)  => fail(s"Expected Good, got Bad($err)")
    }
  }

  "I1Client.testCall (UEBA)" should "round-trip into MyBi.Good" in {
    val impl   = new MockI1Hkt()
    val client = new testpkg.pkg0.I1Client[MyBi](uebaTransport(impl), jsonTransport(impl), rt, ctx)

    client.testCall(testpkg.pkg0.i1.testcall.In()) match {
      case MyBi.Good(out) => out.i00 shouldBe 42
      case MyBi.Bad(err)  => fail(s"Expected Good, got Bad($err)")
    }
  }

  "I1Client" should "surface a DecoderFailed (MyBi.Bad) when the transport returns garbage" in {
    val impl   = new MockI1Hkt()
    val badTransport: (String, String, String) => String = (_, _, _) => "not valid json!!"
    val client = new testpkg.pkg0.I1Client[MyBi](uebaTransport(impl), badTransport, rt, ctx)

    client.testCallJson(testpkg.pkg0.i1.testcall.In()) match {
      case MyBi.Bad(err) => err shouldBe a[BaboonWiringError.DecoderFailed]
      case MyBi.Good(_)  => fail("Expected Bad(DecoderFailed)")
    }
  }
}
