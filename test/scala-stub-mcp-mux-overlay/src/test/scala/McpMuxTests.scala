/**
 * T107 — Scala MCP muxer round-trip overlay test.
 *
 * Exercises `AbstractMcpMuxer[Ctx]` by composing two FRESHLY GENERATED
 * `<Service>McpServer` instances produced from the T102 `mcp-mux-stub-ok/`
 * model (UserService + OrderService). Composition is done strictly through
 * the public T114 routable surface:
 *   - the muxer ctor takes `IBaboonRoutableMcpServer[Ctx]` members,
 *   - the test NEVER subclasses a `<Service>McpServer`,
 *   - the test NEVER calls a member server's own `handle()`.
 *
 * Generated code lands in `src/main/scala/generated-main/` (the isolated
 * dir set by the `test-gen-scala-mcp-mux` mdl action). No committed
 * generated fixtures.
 *
 * Four asserted muxer behaviours (T107 acceptance):
 *   1. tools/list  -> UNION of both services' tools in registration-then-
 *                    declaration order;
 *   2. tools/call  -> routes the flat tool name to the correct owning
 *                    service — proven for a tool of EACH service:
 *                    UserService_getUser (Channel-A Right, isError=false),
 *                    OrderService_cancelOrder (Channel-A Right, isError=false);
 *   3. register a server with a colliding tool name -> throws
 *      BaboonMcpWiringException{DuplicateTool};
 *   4. unknown tool name -> -32602 "unknown tool" response (NoMatchingTool).
 *
 * Assertion discipline (T7 §5.1):
 *   All assertions are ScalaTest `assert(...)` and `fail(...)` which throw
 *   unconditionally on failure. No conditional guards around assertions.
 */

import baboon.runtime.shared._
import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import mcp.mux.stub.{UserService, UserServiceMcpServer, UserServiceWiring}
import mcp.mux.stub.{OrderService, OrderServiceMcpServer, OrderServiceWiring}
import mcp.mux.stub.{UserProfile, UserStatus, OrderSummary, OrderStatus, OrderItem}
import mcp.mux.stub.BaboonServiceRtDefault

// ---------------------------------------------------------------------------
// Stub service implementations: every method returns minimal valid data.
// ---------------------------------------------------------------------------

class StubUserService extends UserService {
  def createUser(arg: mcp.mux.stub.userservice.createuser.In): mcp.mux.stub.userservice.createuser.Out =
    mcp.mux.stub.userservice.createuser.Out(profile = UserProfile(userId = "u1", email = "a@b.c", status = UserStatus.Active))
  def getUser(arg: mcp.mux.stub.userservice.getuser.In): mcp.mux.stub.userservice.getuser.Out =
    mcp.mux.stub.userservice.getuser.Out(profile = Some(UserProfile(userId = "u1", email = "a@b.c", status = UserStatus.Active)))
}

class StubOrderService extends OrderService {
  def placeOrder(arg: mcp.mux.stub.orderservice.placeorder.In): mcp.mux.stub.orderservice.placeorder.Out =
    mcp.mux.stub.orderservice.placeorder.Out(summary = OrderSummary(orderId = "o1", status = OrderStatus.Confirmed, total = 10.0))
  def cancelOrder(arg: mcp.mux.stub.orderservice.cancelorder.In): mcp.mux.stub.orderservice.cancelorder.Out =
    mcp.mux.stub.orderservice.cancelorder.Out(ok = true)
}

// ---------------------------------------------------------------------------
// Mux tests
// ---------------------------------------------------------------------------

class McpMuxTests extends AnyFlatSpec with Matchers {

  private val codecCtx = BaboonCodecContext.Default
  private val rt       = BaboonServiceRtDefault

  private val stubUser  = new StubUserService
  private val stubOrder = new StubOrderService

  // The union of both services' tools in registration-then-declaration order:
  // UserService registered first (createUser, getUser declared in that order),
  // OrderService second (placeOrder, cancelOrder).
  private val expectedUnion = List(
    "UserService_createUser",
    "UserService_getUser",
    "OrderService_placeOrder",
    "OrderService_cancelOrder",
  )

  private val mergedInfo = McpServerInfo(name = "MergedEndpoint", version = "1.0.0")

  // Delegate factories: the SAME errors-mode invokeJson the integrator would bind.
  @scala.annotation.nowarn("cat=unused-params")
  private def userDelegate(
    method: BaboonMethodId,
    data: String,
    ctx: Null,
    cc: BaboonCodecContext,
  ): Either[BaboonWiringError, String] =
    UserServiceWiring.invokeJson(method, data, stubUser, rt, cc)

  @scala.annotation.nowarn("cat=unused-params")
  private def orderDelegate(
    method: BaboonMethodId,
    data: String,
    ctx: Null,
    cc: BaboonCodecContext,
  ): Either[BaboonWiringError, String] =
    OrderServiceWiring.invokeJson(method, data, stubOrder, rt, cc)

  private def makeUserServer(): IBaboonRoutableMcpServer[Null] =
    new UserServiceMcpServer[Null](userDelegate)

  private def makeOrderServer(): IBaboonRoutableMcpServer[Null] =
    new OrderServiceMcpServer[Null](orderDelegate)

  private def makeMuxer(): AbstractMcpMuxer[Null] =
    new AbstractMcpMuxer[Null](mergedInfo, makeUserServer(), makeOrderServer())

  // Helper: send one JSON-RPC request and assert a response was returned.
  private def send(mux: AbstractMcpMuxer[Null], session: McpSession, req: JsonRpcRequest): JsonRpcResponse =
    mux.handle(req, session, null, codecCtx) match {
      case Some(resp) => resp
      case None       => fail(s"""Expected a response for "${req.method}" but got None""")
    }

  private def makeInitReq(id: Long): JsonRpcRequest =
    JsonRpcRequest(
      id     = Some(JsonRpcId.LongId(id)),
      method = "initialize",
      params = Some(Json.obj(
        "protocolVersion" -> Json.fromString("2025-06-18"),
        "capabilities"    -> Json.obj(),
        "clientInfo"      -> Json.obj("name" -> Json.fromString("test-client"), "version" -> Json.fromString("0.0.1")),
      )),
    )

  private def initSession(mux: AbstractMcpMuxer[Null], session: McpSession): Unit = {
    mux.handle(makeInitReq(0L), session, null, codecCtx)
    mux.handle(
      JsonRpcRequest(id = None, method = "notifications/initialized", params = None),
      session, null, codecCtx,
    )
    ()
  }

  private def initedSession(mux: AbstractMcpMuxer[Null]): McpSession = {
    val session = new McpSession
    initSession(mux, session)
    session
  }

  private def callTool(mux: AbstractMcpMuxer[Null], session: McpSession, toolName: String, args: Json): JsonRpcResponse =
    send(mux, session, JsonRpcRequest(
      id     = Some(JsonRpcId.LongId(99L)),
      method = "tools/call",
      params = Some(Json.obj("name" -> Json.fromString(toolName), "arguments" -> args)),
    ))

  // ---------------------------------------------------------------------------
  // §1 — tools/list returns UNION in registration-then-declaration order
  // ---------------------------------------------------------------------------

  "MCP muxer §1: tools/list" should "return the UNION of both services' tools in registration-then-declaration order" in {
    val mux     = makeMuxer()
    val session = initedSession(mux)

    val resp = send(mux, session, JsonRpcRequest(
      id     = Some(JsonRpcId.LongId(1L)),
      method = "tools/list",
      params = None,
    ))

    resp.id shouldBe Some(JsonRpcId.LongId(1L))
    resp.error shouldBe None

    val tools = resp.result
      .flatMap(_.hcursor.downField("tools").as[List[Json]].toOption)
      .getOrElse(fail("tools/list result.tools missing"))

    tools should have length 4

    val names = tools.map(_.hcursor.downField("name").as[String].getOrElse(fail("tool.name missing")))
    names shouldBe expectedUnion
  }

  it should "NOT return tools in interleaved order (negative control proves ordering assertion is live)" in {
    val mux     = makeMuxer()
    val session = initedSession(mux)

    val resp = send(mux, session, JsonRpcRequest(
      id = Some(JsonRpcId.LongId(1L)), method = "tools/list", params = None,
    ))

    val tools = resp.result
      .flatMap(_.hcursor.downField("tools").as[List[Json]].toOption)
      .getOrElse(fail("tools/list result.tools missing"))

    val names = tools.map(_.hcursor.downField("name").as[String].getOrElse(fail("tool.name missing")))
    // This WRONG order must NOT match (interleaved vs registration-then-declaration).
    names should not be List(
      "UserService_createUser",
      "OrderService_placeOrder",
      "UserService_getUser",
      "OrderService_cancelOrder",
    )
  }

  // ---------------------------------------------------------------------------
  // §2 — tools/call routes to the correct owning service
  // ---------------------------------------------------------------------------

  "MCP muxer §2: tools/call routing" should "route UserService_getUser -> UserService, Channel-A Right (isError=false)" in {
    val mux     = makeMuxer()
    val session = initedSession(mux)

    val resp = callTool(mux, session, "UserService_getUser", Json.obj("userId" -> Json.fromString("u1")))

    resp.id shouldBe Some(JsonRpcId.LongId(99L))
    resp.error shouldBe None

    val result  = resp.result.getOrElse(fail("result must be present"))
    val isError = result.hcursor.downField("isError").as[Boolean].toOption
    assert(isError.isEmpty || !isError.get, "isError must be false or absent for Channel-A")

    val content = result.hcursor.downField("content").as[List[Json]].getOrElse(fail("content missing"))
    content should have length 1
    content.head.hcursor.downField("type").as[String].getOrElse(fail("content[0].type missing")) shouldBe "text"

    // The JSON payload proves UserService handled it (profile.userId present).
    val text    = content.head.hcursor.downField("text").as[String].getOrElse(fail("content[0].text missing"))
    val payload = io.circe.parser.parse(text).getOrElse(fail(s"content[0].text is not valid JSON: $text"))
    // profile may be present (Some) in the wrapper — just check the structure
    val profileOpt = payload.hcursor.downField("profile").focus
    assert(profileOpt.isDefined, "result should contain a 'profile' field (UserService result)")
  }

  it should "route OrderService_cancelOrder -> OrderService, Channel-A Right (isError=false)" in {
    val mux     = makeMuxer()
    val session = initedSession(mux)

    val resp = callTool(mux, session, "OrderService_cancelOrder",
      Json.obj("orderId" -> Json.fromString("o1"), "reason" -> Json.Null),
    )

    resp.id shouldBe Some(JsonRpcId.LongId(99L))
    resp.error shouldBe None

    val result  = resp.result.getOrElse(fail("result must be present"))
    val isError = result.hcursor.downField("isError").as[Boolean].toOption
    assert(isError.isEmpty || !isError.get, "isError must be false or absent for Channel-A")

    val content = result.hcursor.downField("content").as[List[Json]].getOrElse(fail("content missing"))
    content should have length 1
    val text    = content.head.hcursor.downField("text").as[String].getOrElse(fail("content[0].text missing"))
    val payload = io.circe.parser.parse(text).getOrElse(fail(s"content[0].text is not valid JSON: $text"))
    // OrderService_cancelOrder returns {ok: true}
    payload.hcursor.downField("ok").as[Boolean].getOrElse(fail("ok missing")) shouldBe true
  }

  it should "produce Channel-B (isError=true) when decoder fails (placeOrder with null items)" in {
    // null for items (a required lst[OrderItem]) causes a decoder failure
    // -> BaboonWiringError.DecoderFailed -> Left -> Channel-B (isError=true).
    val mux     = makeMuxer()
    val session = initedSession(mux)

    val resp = callTool(mux, session, "OrderService_placeOrder",
      Json.obj("userId" -> Json.fromString("u1"), "items" -> Json.Null),
    )

    resp.id shouldBe Some(JsonRpcId.LongId(99L))
    // Channel B: MUST be a result (not error) with isError=true.
    assert(resp.result.isDefined, "Channel-B: result must be present")
    resp.error shouldBe None

    val result = resp.result.get
    result.hcursor.downField("isError").as[Boolean].getOrElse(fail("isError missing")) shouldBe true
  }

  // ---------------------------------------------------------------------------
  // §3 — DuplicateTool on collision
  // ---------------------------------------------------------------------------

  "MCP muxer §3: DuplicateTool" should "throw BaboonMcpWiringException(DuplicateTool) when registering a colliding tool name via ctor" in {
    // Second UserServiceMcpServer re-declares UserService_createUser / UserService_getUser.
    val thrown = try {
      new AbstractMcpMuxer[Null](mergedInfo, makeUserServer(), makeUserServer())
      None
    } catch {
      case e: BaboonMcpWiringException => Some(e)
    }

    assert(thrown.isDefined, "BaboonMcpWiringException must be thrown for duplicate tool registration")
    thrown.get.error match {
      case BaboonMcpWiringError.DuplicateTool(toolName) =>
        toolName shouldBe "UserService_createUser"
      case other =>
        fail(s"Expected DuplicateTool, got $other")
    }
  }

  it should "throw BaboonMcpWiringException(DuplicateTool) when registering a colliding tool name via register()" in {
    val mux = new AbstractMcpMuxer[Null](mergedInfo, makeUserServer())
    val thrown = try {
      mux.register(makeUserServer())
      None
    } catch {
      case e: BaboonMcpWiringException => Some(e)
    }

    assert(thrown.isDefined, "BaboonMcpWiringException must be thrown when calling register() with duplicate tool")
    assert(thrown.get.error.isInstanceOf[BaboonMcpWiringError.DuplicateTool])
  }

  // ---------------------------------------------------------------------------
  // §4 — NoMatchingTool on unknown tool name
  // ---------------------------------------------------------------------------

  "MCP muxer §4: NoMatchingTool" should "return -32602 Channel-A error for an unknown tool name" in {
    val mux     = makeMuxer()
    val session = initedSession(mux)

    val resp = callTool(mux, session, "UserService_nope", Json.obj())

    // MUST be a Channel-A error (-32602), not a result.
    assert(resp.error.isDefined, "Unknown tool must produce a Channel-A error")
    resp.result shouldBe None
    resp.error.get.code shouldBe -32602
    assert(resp.error.get.message.nonEmpty, "error.message must be non-empty")
  }
}
