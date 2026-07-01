// T178 / D40 — Scala zero-service MCP overlay test (RED baseline).
//
// Purpose: prove the ZERO-SERVICE MCP runtime contract. The zero-service model
// (mcp-stub-zero-services-ok) declares @root types but NO RPC block, so
// servicesOf(domain) is empty and NO per-service `<Service>McpServer` is
// generated. With zero services the ONLY source of the MCP runtime types
// (AbstractMcpMuxer / IBaboonRoutableMcpServer / McpServerInfo / McpSession /
// JsonRpcRequest / JsonRpcResponse / JsonRpcErrorCodes) is the STATIC runtime
// file `BaboonMcpRuntime.scala`.
//
// RED (pre-fix): the current generator emits NO `BaboonMcpRuntime.scala` for a
// zero-service model, so the references below are unresolved and `sbt test`
// FAILS to compile ("not found: type AbstractMcpMuxer"). That failure IS the
// D40 reproduction.
//
// GREEN (post-fix, later task): once the generator emits the static MCP runtime
// even when servicesOf(domain) is empty, this file compiles and the runtime
// assertions below pass — an empty muxer lists zero tools and rejects any
// tools/call with JSON-RPC -32602.
//
// Assertion discipline: ScalaTest matchers throw unconditionally on failure.

import baboon.runtime.shared._
import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class McpZeroTests extends AnyFlatSpec with Matchers {
  private val codecCtx = BaboonCodecContext.Default

  // COMPILE-TIME contract: constructing AbstractMcpMuxer[Ctx] with ZERO
  // registered servers requires the static runtime file to exist. With zero
  // services there is no generated <Service>McpServer to import — these types
  // resolve ONLY from BaboonMcpRuntime.scala.
  private def makeEmptyMuxer(): AbstractMcpMuxer[Any] =
    new AbstractMcpMuxer[Any](McpServerInfo("ZeroEndpoint", "1.0.0"))

  private def initedSession(mux: AbstractMcpMuxer[Any]): McpSession = {
    val session = new McpSession()
    mux.handle(
      JsonRpcRequest(
        Some(Json.fromInt(0)),
        "initialize",
        Some(Json.obj(
          "protocolVersion" -> Json.fromString("2025-06-18"),
          "capabilities" -> Json.obj(),
          "clientInfo" -> Json.obj("name" -> Json.fromString("t"), "version" -> Json.fromString("0")),
        )),
      ),
      session, null.asInstanceOf[Any], codecCtx,
    )
    mux.handle(JsonRpcRequest(None, "notifications/initialized", None), session, null.asInstanceOf[Any], codecCtx)
    session
  }

  // RUNTIME contract (a): tools/list on an empty muxer returns an empty array.
  "An empty AbstractMcpMuxer" should "list zero tools" in {
    val mux = makeEmptyMuxer()
    val session = initedSession(mux)

    val resp = mux.handle(
      JsonRpcRequest(Some(Json.fromInt(1)), "tools/list", None),
      session, null.asInstanceOf[Any], codecCtx,
    )
    resp should not be empty
    val r = resp.get
    r.error shouldBe None
    val tools = r.result.flatMap(_.asObject).flatMap(_("tools")).flatMap(_.asArray)
    tools.isDefined shouldBe true
    tools.get.size shouldBe 0
  }

  // RUNTIME contract (b): an unknown tool call yields JSON-RPC -32602.
  it should "reject an unknown tool call with -32602" in {
    val mux = makeEmptyMuxer()
    val session = initedSession(mux)

    val resp = mux.handle(
      JsonRpcRequest(
        Some(Json.fromInt(2)),
        "tools/call",
        Some(Json.obj("name" -> Json.fromString("anything_at_all"), "arguments" -> Json.obj())),
      ),
      session, null.asInstanceOf[Any], codecCtx,
    )
    resp should not be empty
    val r = resp.get
    r.result shouldBe None
    r.error.isDefined shouldBe true
    r.error.get.code shouldBe JsonRpcErrorCodes.InvalidParams
    r.error.get.message should not be empty
  }
}
