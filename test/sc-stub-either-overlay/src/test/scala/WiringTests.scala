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
    testpkg.pkg0.i1.testcall.In_UEBACodec.instance.encode(ctx, bw, testpkg.pkg0.i1.testcall.In())
    bw.flush()
    val inputBytes = oms.toByteArray

    val result = testpkg.pkg0.I1Wiring.invokeUeba(method, inputBytes, impl, rt, ctx)

    result match {
      case Right(outputBytes) =>
        val ims = new java.io.ByteArrayInputStream(outputBytes)
        val br  = new baboon.runtime.shared.LEDataInputStream(ims)
        val decoded = testpkg.pkg0.i1.testcall.Out_UEBACodec.instance
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
    testpkg.pkg0.i1.testcall.In_UEBACodec.instance.encode(ctx, bw, testpkg.pkg0.i1.testcall.In())
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
    testpkg.pkg0.i2.noerrcall.In_UEBACodec.instance.encode(ctx, bw, testpkg.pkg0.i2.noerrcall.In(value = 456))
    bw.flush()
    val inputBytes = oms.toByteArray

    val result = testpkg.pkg0.I2Wiring.invokeUeba(method, inputBytes, impl, rt, ctx)

    result match {
      case Right(outputBytes) =>
        val ims = new java.io.ByteArrayInputStream(outputBytes)
        val br  = new baboon.runtime.shared.LEDataInputStream(ims)
        val decoded = testpkg.pkg0.i2.noerrcall.Out_UEBACodec.instance
          .decode(ctx, br)
          .fold(ex => fail(ex.toString), identity)
        decoded.result shouldBe "result_456"
      case Left(err) => fail(s"Expected Right, got Left($err)")
    }
  }

  // ==================== Cross-domain Muxer ====================
  // A single muxer composes the I1 (errors mode) and I2 (no-err mode)
  // services and routes each call by method.serviceName.

  def newJsonMuxer(): JsonMuxer[Either[BaboonWiringError, String]] =
    new JsonMuxer[Either[BaboonWiringError, String]](
      new testpkg.pkg0.I1JsonService(new MockI1Either(), rt),
      new testpkg.pkg0.I2JsonService(new MockI2Either(), rt),
    )

  def newUebaMuxer(): UebaMuxer[Either[BaboonWiringError, Array[Byte]]] =
    new UebaMuxer[Either[BaboonWiringError, Array[Byte]]](
      new testpkg.pkg0.I1UebaService(new MockI1Either(), rt),
      new testpkg.pkg0.I2UebaService(new MockI2Either(), rt),
    )

  "JsonMuxer" should "route to I1" in {
    val method = BaboonMethodId("I1", "testCall")
    val inputJson = testpkg.pkg0.i1.testcall.In_JsonCodec.instance
      .encode(ctx, testpkg.pkg0.i1.testcall.In()).noSpaces

    newJsonMuxer().invoke(method, inputJson, ctx) match {
      case Right(jsonStr) =>
        val wire = io.circe.parser.parse(jsonStr).fold(throw _, identity)
        val decoded = testpkg.pkg0.i1.testcall.Out_JsonCodec.instance
          .decode(ctx, wire).fold(ex => fail(ex.toString), identity)
        decoded.i00 shouldBe 42
      case Left(err) => fail(s"Expected Right, got Left($err)")
    }
  }

  "JsonMuxer" should "route to I2" in {
    val method = BaboonMethodId("I2", "noErrCall")
    val inputJson = testpkg.pkg0.i2.noerrcall.In_JsonCodec.instance
      .encode(ctx, testpkg.pkg0.i2.noerrcall.In(value = 123)).noSpaces

    newJsonMuxer().invoke(method, inputJson, ctx) match {
      case Right(jsonStr) =>
        val wire = io.circe.parser.parse(jsonStr).fold(throw _, identity)
        val decoded = testpkg.pkg0.i2.noerrcall.Out_JsonCodec.instance
          .decode(ctx, wire).fold(ex => fail(ex.toString), identity)
        decoded.result shouldBe "result_123"
      case Left(err) => fail(s"Expected Right, got Left($err)")
    }
  }

  "JsonMuxer" should "throw NoMatchingService for an unregistered service" in {
    val method = BaboonMethodId("Nonexistent", "x")
    val ex = intercept[BaboonWiringException](newJsonMuxer().invoke(method, "{}", ctx))
    ex.error shouldBe a[BaboonWiringError.NoMatchingService]
  }

  "JsonMuxer" should "throw DuplicateService on duplicate registration" in {
    val ex = intercept[BaboonWiringException](
      new JsonMuxer[Either[BaboonWiringError, String]](
        new testpkg.pkg0.I1JsonService(new MockI1Either(), rt),
        new testpkg.pkg0.I1JsonService(new MockI1Either(), rt),
      )
    )
    ex.error shouldBe a[BaboonWiringError.DuplicateService]
  }

  "UebaMuxer" should "route to I1" in {
    val method = BaboonMethodId("I1", "testCall")
    val oms = new java.io.ByteArrayOutputStream()
    val bw  = new baboon.runtime.shared.LEDataOutputStream(oms)
    testpkg.pkg0.i1.testcall.In_UEBACodec.instance.encode(ctx, bw, testpkg.pkg0.i1.testcall.In())
    bw.flush()

    newUebaMuxer().invoke(method, oms.toByteArray, ctx) match {
      case Right(outputBytes) =>
        val br = new baboon.runtime.shared.LEDataInputStream(new java.io.ByteArrayInputStream(outputBytes))
        val decoded = testpkg.pkg0.i1.testcall.Out_UEBACodec.instance
          .decode(ctx, br).fold(ex => fail(ex.toString), identity)
        decoded.i00 shouldBe 42
      case Left(err) => fail(s"Expected Right, got Left($err)")
    }
  }

  "UebaMuxer" should "route to I2" in {
    val method = BaboonMethodId("I2", "noErrCall")
    val oms = new java.io.ByteArrayOutputStream()
    val bw  = new baboon.runtime.shared.LEDataOutputStream(oms)
    testpkg.pkg0.i2.noerrcall.In_UEBACodec.instance.encode(ctx, bw, testpkg.pkg0.i2.noerrcall.In(value = 456))
    bw.flush()

    newUebaMuxer().invoke(method, oms.toByteArray, ctx) match {
      case Right(outputBytes) =>
        val br = new baboon.runtime.shared.LEDataInputStream(new java.io.ByteArrayInputStream(outputBytes))
        val decoded = testpkg.pkg0.i2.noerrcall.Out_UEBACodec.instance
          .decode(ctx, br).fold(ex => fail(ex.toString), identity)
        decoded.result shouldBe "result_456"
      case Left(err) => fail(s"Expected Right, got Left($err)")
    }
  }

  "UebaMuxer" should "throw NoMatchingService for an unregistered service" in {
    val method = BaboonMethodId("Nonexistent", "x")
    val ex = intercept[BaboonWiringException](newUebaMuxer().invoke(method, Array.emptyByteArray, ctx))
    ex.error shouldBe a[BaboonWiringError.NoMatchingService]
  }

  // ==================== Client round-trip ====================
  // The generated ${Svc}Client encodes the request, hands it to a transport
  // callback, and decodes the response into the same result container the
  // server wiring uses (here: Either[BaboonWiringError, Out]). The in-process
  // transport routes straight into ${Svc}Wiring.invoke{Json,Ueba} and returns
  // the success wire, exercising the result-container path end-to-end.

  private def jsonTransport(impl: testpkg.pkg0.I1): (String, String, String) => String =
    (svc, method, data) =>
      testpkg.pkg0.I1Wiring.invokeJson(BaboonMethodId(svc, method), data, impl, rt, ctx) match {
        case Right(wire) => wire
        case Left(err)   => throw new BaboonWiringException(err)
      }

  private def uebaTransport(impl: testpkg.pkg0.I1): (String, String, Array[Byte]) => Array[Byte] =
    (svc, method, data) =>
      testpkg.pkg0.I1Wiring.invokeUeba(BaboonMethodId(svc, method), data, impl, rt, ctx) match {
        case Right(wire) => wire
        case Left(err)   => throw new BaboonWiringException(err)
      }

  "I1Client.testCallJson" should "round-trip through the server wiring" in {
    val impl   = new MockI1Either()
    val client = new testpkg.pkg0.I1Client(uebaTransport(impl), jsonTransport(impl), rt, ctx)

    client.testCallJson(testpkg.pkg0.i1.testcall.In()) match {
      case Right(out) => out.i00 shouldBe 42
      case Left(err)  => fail(s"Expected Right, got Left($err)")
    }
  }

  "I1Client.testCall (UEBA)" should "round-trip through the server wiring" in {
    val impl   = new MockI1Either()
    val client = new testpkg.pkg0.I1Client(uebaTransport(impl), jsonTransport(impl), rt, ctx)

    client.testCall(testpkg.pkg0.i1.testcall.In()) match {
      case Right(out) => out.i00 shouldBe 42
      case Left(err)  => fail(s"Expected Right, got Left($err)")
    }
  }

  "I1Client" should "surface a DecoderFailed when the transport returns garbage" in {
    val impl   = new MockI1Either()
    val badTransport: (String, String, String) => String = (_, _, _) => "not valid json!!"
    val client = new testpkg.pkg0.I1Client(uebaTransport(impl), badTransport, rt, ctx)

    client.testCallJson(testpkg.pkg0.i1.testcall.In()) match {
      case Left(err) => err shouldBe a[BaboonWiringError.DecoderFailed]
      case Right(_)  => fail("Expected Left(DecoderFailed)")
    }
  }

  "I2Client.noErrCallJson" should "round-trip through the server wiring" in {
    val impl = new MockI2Either()
    val jsonT: (String, String, String) => String =
      (svc, method, data) =>
        testpkg.pkg0.I2Wiring.invokeJson(BaboonMethodId(svc, method), data, impl, rt, ctx) match {
          case Right(wire) => wire
          case Left(err)   => throw new BaboonWiringException(err)
        }
    val uebaT: (String, String, Array[Byte]) => Array[Byte] =
      (svc, method, data) =>
        testpkg.pkg0.I2Wiring.invokeUeba(BaboonMethodId(svc, method), data, impl, rt, ctx) match {
          case Right(wire) => wire
          case Left(err)   => throw new BaboonWiringException(err)
        }
    val client = new testpkg.pkg0.I2Client(uebaT, jsonT, rt, ctx)

    client.noErrCallJson(testpkg.pkg0.i2.noerrcall.In(value = 7)) match {
      case Right(out) => out.result shouldBe "result_7"
      case Left(err)  => fail(s"Expected Right, got Left($err)")
    }
  }
}
