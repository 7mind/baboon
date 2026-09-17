package mcp

import baboon.runtime.shared.*
import kotlinx.serialization.json.*
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class McpProtocolOwnershipTest {
    private val info = McpServerInfo("test", "1")
    private val method = BaboonMethodId("service", "call")
    private fun tool(name: String) = McpToolEntry(name, method, buildJsonObject { })
    private inner class Server : AbstractBaboonMcpServer<String>() {
        override val serverInfo = info
        var current = listOf(tool("first"))
        override val tools get() = current
        override fun invokeJson(method: BaboonMethodId, data: String, ctx: String, codecCtx: BaboonCodecContext): Either<BaboonWiringError, String> =
            if (ctx == "fail") Either.Left(BaboonWiringError.NoMatchingMethod(method)) else Either.Right("$ctx:$data")
        override fun describeWiringError(e: BaboonWiringError) = "custom-server-error"
    }
    private fun request(method: String, params: JsonElement?) = JsonRpcRequest(JsonPrimitive(7), method, params)
    private fun call(name: String) = request("tools/call", buildJsonObject { put("name", JsonPrimitive(name)) })
    private val initialize = request("initialize", buildJsonObject { put("protocolVersion", JsonPrimitive("ignored")) })

    @Test
    fun standaloneAndMuxRetainSessionsIdsAndSuccessTranscripts() {
        val server = Server()
        val mux = AbstractMcpMuxer(info, server)
        val a = McpSession()
        val b = McpSession()
        val requests = listOf(
            call("first"), request("tools/list", null), request("initialize", null), initialize,
            request("notifications/initialized", null), request("tools/list", null), call("first"),
            call("absent"), request("tools/call", null), request("unknown", null),
        )
        for (req in requests) {
            assertEquals(server.handle(req, a, "ctx", BaboonCodecContext.Compact), mux.handle(req, b, "ctx", BaboonCodecContext.Compact))
            assertEquals(a.initialized, b.initialized)
        }
        val failure = server.handle(call("first"), a, "fail", BaboonCodecContext.Compact)!!
        assertTrue(failure.result!!.jsonObject["isError"]!!.jsonPrimitive.boolean)
        assertEquals("custom-server-error", failure.result!!.jsonObject["content"]!!.jsonArray[0].jsonObject["text"]!!.jsonPrimitive.content)
        assertNotNull(server.handle(call("first"), McpSession(), "ctx", BaboonCodecContext.Compact)!!.error)
    }

    @Test
    fun standaloneToolsRemainDynamicAndMuxRetainsRegistrationSnapshot() {
        val server = Server()
        val mux = AbstractMcpMuxer(info, server)
        val session = McpSession().also { it.initialized = true }
        server.current = listOf(tool("second"))
        assertNull(server.handle(call("second"), session, "ctx", BaboonCodecContext.Compact)!!.error)
        assertNotNull(server.handle(call("first"), session, "ctx", BaboonCodecContext.Compact)!!.error)
        assertNull(mux.handle(call("first"), session, "ctx", BaboonCodecContext.Compact)!!.error)
        assertNotNull(mux.handle(call("second"), session, "ctx", BaboonCodecContext.Compact)!!.error)
        assertThrows(BaboonMcpWiringException::class.java) { mux.register(Server()) }
    }
}
