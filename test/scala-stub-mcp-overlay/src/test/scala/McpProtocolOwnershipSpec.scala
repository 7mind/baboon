import baboon.runtime.shared._
import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite

class McpProtocolOwnershipSpec extends AnyFunSuite {
  private val testInfo           = McpServerInfo("test", "1")
  private val method             = BaboonMethodId("service", "call")
  private def tool(name: String) = McpToolEntry(name, method, Json.obj())
  private class Server extends AbstractBaboonMcpServer[String] {
    override val serverInfo: McpServerInfo = testInfo
    var current: Seq[McpToolEntry]         = Seq(tool("first"))
    override def tools: Seq[McpToolEntry]  = current
    override protected def invokeJson(m: BaboonMethodId, data: String, ctx: String, codecCtx: BaboonCodecContext): Either[BaboonWiringError, String] =
      if (ctx == "fail") Left(BaboonWiringError.NoMatchingMethod(m)) else Right(s"$ctx:$data")
    override protected def describeWiringError(error: BaboonWiringError): String = "custom-server-error"
  }
  private def request(method: String, params: Option[Json]) = JsonRpcRequest(Some(JsonRpcId.LongId(7)), method, params)
  private def call(name: String)                            = request("tools/call", Some(Json.obj("name" -> Json.fromString(name))))
  private val initialize                                    = request("initialize", Some(Json.obj("protocolVersion" -> Json.fromString("ignored"))))

  test("standalone and mux protocol transcripts preserve sessions, IDs and error channels") {
    val server = new Server
    val mux = new AbstractMcpMuxer[String](testInfo, server) {
      override protected def describeWiringError(error: BaboonWiringError): String = "custom-server-error"
    }
    val a = new McpSession
    val b = new McpSession
    val requests = List(
      call("first"),
      request("tools/list", None),
      request("initialize", None),
      initialize,
      request("notifications/initialized", None),
      request("tools/list", None),
      call("first"),
      call("absent"),
      request("tools/call", None),
      request("unknown", None),
    )
    requests.foreach {
      req =>
        assert(server.handle(req, a, "ctx", BaboonCodecContext.Compact) == mux.handle(req, b, "ctx", BaboonCodecContext.Compact))
        assert(a.initialized == b.initialized)
    }
    assert(server.handle(call("first"), a, "fail", BaboonCodecContext.Compact) == mux.handle(call("first"), b, "fail", BaboonCodecContext.Compact))
    assert(server.handle(call("first"), a, "fail", BaboonCodecContext.Compact).get.result.get.noSpaces.contains("custom-server-error"))
    assert(server.handle(call("first"), new McpSession, "ctx", BaboonCodecContext.Compact).get.error.nonEmpty)
  }

  test("standalone tools remain dynamic while mux registration retains its snapshot") {
    val server  = new Server
    val mux     = new AbstractMcpMuxer[String](testInfo, server)
    val session = new McpSession
    session.initialized = true
    server.current      = Seq(tool("second"))
    assert(server.handle(call("second"), session, "ctx", BaboonCodecContext.Compact).get.error.isEmpty)
    assert(server.handle(call("first"), session, "ctx", BaboonCodecContext.Compact).get.error.nonEmpty)
    assert(mux.handle(call("first"), session, "ctx", BaboonCodecContext.Compact).get.error.isEmpty)
    assert(mux.handle(call("second"), session, "ctx", BaboonCodecContext.Compact).get.error.nonEmpty)
    intercept[BaboonMcpWiringException](mux.register(new Server))
  }

  test("mux overridden registration may dispatch during base construction") {
    val server = new Server
    val mux = new AbstractMcpMuxer[String](testInfo, server) {
      override def register(value: IBaboonRoutableMcpServer[String]): Unit = {
        assert(handle(initialize, new McpSession, "ctx", BaboonCodecContext.Compact).get.error.isEmpty)
        super.register(value)
      }
    }
    assert(mux.handle(initialize, new McpSession, "ctx", BaboonCodecContext.Compact).get.error.isEmpty)
  }
}
