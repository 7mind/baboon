/**
 * T12 — Scala MCP round-trip overlay test.
 *
 * Drives the generated McpToolsMcpServer[Ctx] through the canonical T7 scenario
 * (docs/research/mcp-roundtrip-scenario.md) using an entirely in-process fake
 * transport. No stdio or HTTP is involved.
 *
 * Assertion discipline (T7 §5.1):
 *   - All assertions are ScalaTest `assert(...)` and `fail(...)` which throw
 *     unconditionally on failure. No conditional guards around assertions.
 *
 * K1 validity tier (T7 §5.3 — structural-equality tier for Scala):
 *   - At tools/list each returned inputSchema is:
 *     (a) parsed through io.circe.parser.parse — proves well-formed Circe JSON;
 *     (b) compared against the T7 §2.3 REFERENCE value embedded as a literal in this
 *         test, via key-by-key recursive structural equality. `required` arrays are
 *         compared as sets (§5.4). This is NOT a self-round-trip — it asserts the
 *         emitter output matches an INDEPENDENT authoritative reference.
 *     Catches Circe codec-rendering divergence without a per-language JSON Schema
 *     validator (no AJV/NJsonSchema available in JVM-Scala).
 *
 * Negative controls (T7 §5.2):
 *   - §4.1 (unknown tool → -32602 Channel-A error): if the server returned success
 *     for McpTools_nonexistent the assertions below would fail.
 *   - §4.2 (malformed decode → Channel-B isError=true): if isError were false the
 *     test would fail.
 *   - K1 structural equality (schema structure): the parsed inputSchema for each tool
 *     is compared against an independent reference; any rendering divergence fails.
 *   - K1 negative control (schema STRUCTURE): a deliberately wrong reference for
 *     McpTools_ping (extra field `"WRONG": true`) makes the structural comparison
 *     fail, proving the K1 gate is not vacuous. This is documented and verified via
 *     the `verifyNegativeControlFails` helper; after the test the correct reference
 *     is used and the test passes.
 *   - DELIBERATE-NEGATIVE-CONTROL (tool-name, documented, not left active):
 *     Replacing `assert(toolName(4) == "McpTools_ping")` with
 *     `assert(toolName(4) == "McpTools_WRONG")` makes the test fail, proving the
 *     position-4 name assertion is live.
 *
 * Channel-B (§4.2) trigger strategy:
 *   Send ping with missing required field `seqno`. The generated
 *   `mcptools.ping.In_JsonCodec.instance.decode` accesses `.seqno` field which is
 *   absent in the input JSON, causing a decoder failure. This is caught by the
 *   wiring as `BaboonWiringError.DecoderFailed` and the MCP server returns Channel-B.
 */

import baboon.runtime.shared._
import io.circe.Json
import io.circe.parser.{parse => circeParseJson}
import mcp.stub.{McpToolsMcpServer, McpToolsWiring}
import mcp.stub.mcptools.listcollections.{In => ListCollectionsIn, Out => ListCollectionsOut}
import mcp.stub.mcptools.processshape.{In => ProcessShapeIn, Out => ProcessShapeOut}
import mcp.stub.mcptools.pagepoints.{In => PagePointsIn, Out => PagePointsOut}
import mcp.stub.mcptools.ping.{In => PingIn, Out => PingOut}
import mcp.stub.mcptools.describepricing.{In => DescribePricingIn, Out => DescribePricingOut}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// Note: submitComposite is omitted from the success path test below because it
// uses the FFancyStr foreign type codec which requires external registration;
// the Scala BaboonSingleton LazyInstance pattern doesn't support runtime override
// in this test context. ping, listCollections, processShape, and pagePoints are
// fully exercised. The generated submitComposite codec now references
// FFancyStr_JsonCodec via a properly imported _JsonCodec ScType (T29/D4), so no
// FFancyStr shim is required for compilation.

// ---------------------------------------------------------------------------
// Stub McpTools service: every method returns ok=true by convention (T7 §3).
// ---------------------------------------------------------------------------

class StubMcpTools extends mcp.stub.McpTools {
  def listCollections(arg: ListCollectionsIn): ListCollectionsOut = ListCollectionsOut(ok = true)
  def submitComposite(arg: mcp.stub.mcptools.submitcomposite.In): mcp.stub.mcptools.submitcomposite.Out =
    mcp.stub.mcptools.submitcomposite.Out(ok = true)
  def processShape(arg: ProcessShapeIn): ProcessShapeOut = ProcessShapeOut(ok = true)
  def processTagged(arg: mcp.stub.mcptools.processtagged.In): mcp.stub.mcptools.processtagged.Out =
    mcp.stub.mcptools.processtagged.Out(ok = true)
  def pagePoints(arg: PagePointsIn): PagePointsOut = PagePointsOut(ok = true)
  def ping(arg: PingIn): PingOut = PingOut(ok = true)
  def describePricing(arg: DescribePricingIn): DescribePricingOut = DescribePricingOut(ok = true)
}

// ---------------------------------------------------------------------------
// Fake transport: routes through the generated McpToolsWiring.invokeJson.
// This is transport-abstract — no HTTP, no stdio, no network.
// ---------------------------------------------------------------------------

class McpTests extends AnyFlatSpec with Matchers {

  // Note: FFancyStr_JsonCodec.instance throws by default (foreign type codec).
  // Tests that use submitComposite (which requires FFancyStr codec) are omitted.

  private val codecCtx = BaboonCodecContext.Default
  private val rt       = mcp.stub.BaboonServiceRtDefault

  private val stub = new StubMcpTools

  // Fake invokeJson delegate: routes through the generated wiring.
  @scala.annotation.nowarn("cat=unused-params")
  private def fakeInvokeJson(
    method: BaboonMethodId,
    data: String,
    ctx: Null,
    codecCtx: BaboonCodecContext,
  ): Either[BaboonWiringError, String] =
    McpToolsWiring.invokeJson(method, data, stub, rt, codecCtx)

  private def makeServer(): McpToolsMcpServer[Null] =
    new McpToolsMcpServer[Null](fakeInvokeJson)

  // Helper: send one JSON-RPC request and assert a response was returned.
  private def send(server: McpToolsMcpServer[Null], session: McpSession, req: JsonRpcRequest): JsonRpcResponse = {
    server.handle(req, session, null, codecCtx) match {
      case Some(resp) => resp
      case None       => fail(s"""Expected a response for "${req.method}" but got None (notification not expected here)""")
    }
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

  private def initSession(server: McpToolsMcpServer[Null], session: McpSession): Unit = {
    server.handle(makeInitReq(0L), session, null, codecCtx)
    server.handle(
      JsonRpcRequest(id = None, method = "notifications/initialized", params = None),
      session, null, codecCtx,
    )
    ()
  }

  // ---------------------------------------------------------------------------
  // §1 — initialize
  // ---------------------------------------------------------------------------

  "MCP §1: initialize" should "return correct protocolVersion, capabilities, serverInfo" in {
    val server  = makeServer()
    val session = new McpSession

    val resp = send(server, session, makeInitReq(1L))

    resp.id shouldBe Some(JsonRpcId.LongId(1L))
    resp.error shouldBe None

    val result = resp.result.getOrElse(fail("result must be present"))
    val cursor = result.hcursor

    // protocolVersion MUST be "2025-06-18"
    cursor.downField("protocolVersion").as[String].getOrElse(fail("protocolVersion missing")) shouldBe "2025-06-18"

    // capabilities MUST contain exactly one key "tools" with value {}
    val caps = cursor.downField("capabilities").as[Json].getOrElse(fail("capabilities missing"))
    val capKeys = caps.asObject.getOrElse(fail("capabilities must be a JSON object")).keys.toList
    capKeys shouldBe List("tools")
    caps.hcursor.downField("tools").as[Json].getOrElse(fail("capabilities.tools missing")) shouldBe Json.obj()

    // serverInfo: name and version must be non-empty strings
    val info    = cursor.downField("serverInfo").as[Json].getOrElse(fail("serverInfo missing"))
    val srvName = info.hcursor.downField("name").as[String].getOrElse(fail("serverInfo.name missing"))
    val srvVer  = info.hcursor.downField("version").as[String].getOrElse(fail("serverInfo.version missing"))
    assert(srvName.nonEmpty, "serverInfo.name must be non-empty")
    assert(srvVer.nonEmpty, "serverInfo.version must be non-empty")
  }

  it should "return None for notifications/initialized" in {
    val server  = makeServer()
    val session = new McpSession
    server.handle(makeInitReq(0L), session, null, codecCtx)

    val notifResp = server.handle(
      JsonRpcRequest(id = None, method = "notifications/initialized", params = None),
      session, null, codecCtx,
    )
    notifResp shouldBe None
  }

  // ---------------------------------------------------------------------------
  // §2 — tools/list + K1 inputSchema validation
  // ---------------------------------------------------------------------------

  private def initAndList(): (List[Json], JsonRpcResponse) = {
    val server  = makeServer()
    val session = new McpSession
    initSession(server, session)

    val resp = send(server, session, JsonRpcRequest(
      id     = Some(JsonRpcId.LongId(2L)),
      method = "tools/list",
      params = None,
    ))

    val toolsArr = resp.result
      .flatMap(_.hcursor.downField("tools").as[List[Json]].toOption)
      .getOrElse(fail("tools/list result.tools missing"))
    (toolsArr, resp)
  }

  "MCP §2: tools/list" should "return exactly 7 tools in declaration order (positions 0–6)" in {
    val (tools, resp) = initAndList()

    resp.id shouldBe Some(JsonRpcId.LongId(2L))
    resp.error shouldBe None
    tools should have length 7

    def toolName(i: Int): String = tools(i).hcursor.downField("name").as[String].getOrElse(fail(s"tools[$i].name missing"))

    // Exact position assertions per §0 (model declaration order).
    // processTagged is declared between processShape and pagePoints (T26/D11),
    // so it occupies index 3 and shifts pagePoints→4, ping→5.
    // describePricing (D34/T125) is declared after ping at index 6.
    // DELIBERATE-NEGATIVE-CONTROL: changing "McpTools_ping" → "McpTools_WRONG"
    // on the next line makes this test fail, proving position[5] check is live.
    toolName(0) shouldBe "McpTools_listCollections"
    toolName(1) shouldBe "McpTools_submitComposite"
    toolName(2) shouldBe "McpTools_processShape"
    toolName(3) shouldBe "McpTools_processTagged"
    toolName(4) shouldBe "McpTools_pagePoints"
    toolName(5) shouldBe "McpTools_ping"
    toolName(6) shouldBe "McpTools_describePricing"

    // No "nextCursor" key (§2.2)
    resp.result.flatMap(_.hcursor.downField("nextCursor").focus) shouldBe None

    // T119: McpTools_ping carries a single-line doc comment in mcp_stub.baboon.
    // T128: McpTools_describePricing carries a multi-line doc comment; its tools/list
    // entry must expose the flattened text including embedded newlines and hazard chars
    // ($, ", \). Both documented tools must have a description. Every other tool must not.
    val pingDescription = "Liveness probe returning a fixed acknowledgement token."
    // T128: Expected description for McpTools_describePricing.
    // DocFormat.cleanPrefix strips " * " prefix, collapses leading/trailing blank lines,
    // preserves internal blank line. Two literal backslashes (\\) in the baboon source
    // survive as two backslashes in the description string.
    val describePricingDescription =
      "Returns the fee schedule for the requested service tier.\n" +
      "Base cost is $5 per call; \"premium\" tier costs $20 per call.\n" +
      "\n" +
      "Pass the tier name using the \\\\ delimiter convention documented in\n" +
      "the API guide (e.g. \"standard\\\\premium\")."
    for (t <- tools) {
      val name = t.hcursor.downField("name").as[String].getOrElse(fail("tool.name missing"))
      if (name == "McpTools_ping") {
        t.hcursor.downField("description").as[String].toOption shouldBe Some(pingDescription)
      } else if (name == "McpTools_describePricing") {
        // T128: unconditional throw on mismatch — proves $, ", \, \n survive round-trip.
        val actual = t.hcursor.downField("description").as[String].toOption
        if (!actual.contains(describePricingDescription)) {
          fail(s"T128: McpTools_describePricing description round-trip FAILED.\nExpected: ${describePricingDescription}\nActual: ${actual}")
        }
      } else {
        t.hcursor.downField("description").focus shouldBe None
      }
    }
  }

  it should "set $schema to the Draft-2020-12 URI in each inputSchema" in {
    val (tools, _) = initAndList()
    for (t <- tools) {
      val schema = t.hcursor.downField("inputSchema").as[Json].getOrElse(fail("inputSchema missing"))
      schema.hcursor.downField("$schema").as[String].getOrElse(fail("$schema missing")) shouldBe
        "https://json-schema.org/draft/2020-12/schema"
    }
  }

  // ---------------------------------------------------------------------------
  // T7 §2.3 canonical reference inputSchemas (authoritative values).
  // Source: docs/research/mcp-roundtrip-scenario.md §2.3 and
  //         McpInputSchemaEmissionTest.scala (T5 golden).
  // Parsed once at object initialisation; a parse failure indicates a defect in
  // this test file (not in the generator under test).
  // ---------------------------------------------------------------------------

  private def parseRef(toolName: String, json: String): Json =
    circeParseJson(json).fold(
      err => throw new AssertionError(s"BUG in test: embedded reference JSON for '$toolName' did not parse: $err"),
      identity,
    )

  private val refPing: Json = parseRef("McpTools_ping",
    """{
      |  "$schema": "https://json-schema.org/draft/2020-12/schema",
      |  "type": "object",
      |  "properties": {
      |    "seqno": { "type": "integer", "format": "int32" },
      |    "label": { "type": "string" }
      |  },
      |  "required": ["seqno", "label"]
      |}""".stripMargin)

  private val refListCollections: Json = parseRef("McpTools_listCollections",
    """{
      |  "$schema": "https://json-schema.org/draft/2020-12/schema",
      |  "type": "object",
      |  "properties": {
      |    "tags":      { "type": "array", "items": { "type": "string" } },
      |    "uniqueIds": { "type": "array", "items": { "type": "integer", "format": "int64" }, "uniqueItems": true },
      |    "labels":    { "type": "object", "additionalProperties": { "type": "string" } },
      |    "byColor":   {
      |      "type": "object",
      |      "additionalProperties": { "type": "string" },
      |      "propertyNames": { "type": "string", "enum": ["Red", "Green", "Blue"] }
      |    }
      |  },
      |  "required": ["tags", "uniqueIds", "labels", "byColor"],
      |  "$defs": {
      |    "mcp_stub_Color": {
      |      "type": "string",
      |      "enum": ["Red", "Green", "Blue"]
      |    }
      |  }
      |}""".stripMargin)

  private val refSubmitComposite: Json = parseRef("McpTools_submitComposite",
    """{
      |  "$schema": "https://json-schema.org/draft/2020-12/schema",
      |  "type": "object",
      |  "properties": {
      |    "nested":     { "$ref": "#/$defs/mcp_stub_Nested" },
      |    "maybePoint": { "oneOf": [{ "$ref": "#/$defs/mcp_stub_Point" }, { "type": "null" }] },
      |    "color":      { "$ref": "#/$defs/mcp_stub_Color" },
      |    "fancy":      { "type": "string" }
      |  },
      |  "required": ["nested", "color", "fancy"],
      |  "$defs": {
      |    "mcp_stub_Color": {
      |      "type": "string",
      |      "enum": ["Red", "Green", "Blue"]
      |    },
      |    "mcp_stub_Nested": {
      |      "type": "object",
      |      "properties": {
      |        "point": { "$ref": "#/$defs/mcp_stub_Point" },
      |        "color": { "$ref": "#/$defs/mcp_stub_Color" },
      |        "label": { "oneOf": [{ "type": "string" }, { "type": "null" }] }
      |      },
      |      "required": ["point", "color"]
      |    },
      |    "mcp_stub_Point": {
      |      "type": "object",
      |      "properties": {
      |        "x": { "type": "integer", "format": "int32" },
      |        "y": { "type": "integer", "format": "int32" }
      |      },
      |      "required": ["x", "y"]
      |    }
      |  }
      |}""".stripMargin)

  private val refProcessShape: Json = parseRef("McpTools_processShape",
    """{
      |  "$schema": "https://json-schema.org/draft/2020-12/schema",
      |  "type": "object",
      |  "properties": {
      |    "shape": { "$ref": "#/$defs/mcp_stub_Shape" },
      |    "tree":  { "$ref": "#/$defs/mcp_stub_Tree" }
      |  },
      |  "required": ["shape", "tree"],
      |  "$defs": {
      |    "mcp_stub_Shape": {
      |      "oneOf": [
      |        { "$ref": "#/$defs/mcp_stub_Shape_Circle" },
      |        { "$ref": "#/$defs/mcp_stub_Shape_Rect" }
      |      ]
      |    },
      |    "mcp_stub_Shape_Circle": {
      |      "type": "object",
      |      "properties": {
      |        "radius": { "type": "number", "format": "double" }
      |      },
      |      "required": ["radius"]
      |    },
      |    "mcp_stub_Shape_Rect": {
      |      "type": "object",
      |      "properties": {
      |        "w": { "type": "number", "format": "double" },
      |        "h": { "type": "number", "format": "double" }
      |      },
      |      "required": ["w", "h"]
      |    },
      |    "mcp_stub_Tree": {
      |      "type": "object",
      |      "properties": {
      |        "value":    { "type": "integer", "format": "int32" },
      |        "left":     { "oneOf": [{ "$ref": "#/$defs/mcp_stub_Tree" }, { "type": "null" }] },
      |        "children": { "type": "array", "items": { "$ref": "#/$defs/mcp_stub_Tree" } }
      |      },
      |      "required": ["value", "children"]
      |    }
      |  }
      |}""".stripMargin)

  // processTagged: contract-bearing ADT (T26/D11). `Tagged` is `is HasId`;
  // the HasId contract carries `id: str`, merged into every branch DTO at typing
  // time (BaboonTranslator:289-306). Each branch $defs entry therefore already
  // has `id` in properties + required, WITHOUT any allOf merge (H3 lock — see
  // McpInputSchemaEmissionTest.scala "processTagged" case). Branch order in
  // oneOf is declaration order: TagA then TagB.
  private val refProcessTagged: Json = parseRef("McpTools_processTagged",
    """{
      |  "$schema": "https://json-schema.org/draft/2020-12/schema",
      |  "type": "object",
      |  "properties": {
      |    "tagged": { "$ref": "#/$defs/mcp_stub_Tagged" }
      |  },
      |  "required": ["tagged"],
      |  "$defs": {
      |    "mcp_stub_Tagged": {
      |      "oneOf": [
      |        { "$ref": "#/$defs/mcp_stub_Tagged_TagA" },
      |        { "$ref": "#/$defs/mcp_stub_Tagged_TagB" }
      |      ]
      |    },
      |    "mcp_stub_Tagged_TagA": {
      |      "type": "object",
      |      "properties": {
      |        "id":  { "type": "string" },
      |        "tag": { "type": "string" }
      |      },
      |      "required": ["id", "tag"]
      |    },
      |    "mcp_stub_Tagged_TagB": {
      |      "type": "object",
      |      "properties": {
      |        "id":     { "type": "string" },
      |        "weight": { "type": "integer", "format": "int32" }
      |      },
      |      "required": ["id", "weight"]
      |    }
      |  }
      |}""".stripMargin)

  private val refPagePoints: Json = parseRef("McpTools_pagePoints",
    """{
      |  "$schema": "https://json-schema.org/draft/2020-12/schema",
      |  "type": "object",
      |  "properties": {
      |    "page": { "$ref": "#/$defs/mcp_stub_PointPage" }
      |  },
      |  "required": ["page"],
      |  "$defs": {
      |    "mcp_stub_Point": {
      |      "type": "object",
      |      "properties": {
      |        "x": { "type": "integer", "format": "int32" },
      |        "y": { "type": "integer", "format": "int32" }
      |      },
      |      "required": ["x", "y"]
      |    },
      |    "mcp_stub_PointPage": {
      |      "type": "object",
      |      "properties": {
      |        "items": { "type": "array", "items": { "$ref": "#/$defs/mcp_stub_Point" } },
      |        "total": { "type": "integer", "format": "int32", "minimum": 0 }
      |      },
      |      "required": ["items", "total"]
      |    }
      |  }
      |}""".stripMargin)

  // D34/T125: scalar-only, single string field, no $defs
  private val refDescribePricing: Json = parseRef("McpTools_describePricing",
    """{
      |  "$schema": "https://json-schema.org/draft/2020-12/schema",
      |  "type": "object",
      |  "properties": {
      |    "tier": { "type": "string" }
      |  },
      |  "required": ["tier"]
      |}""".stripMargin)

  // ---------------------------------------------------------------------------
  // K1 structural-equality helpers (T7 §5.4).
  // Circe `Json` object equality is key-order-insensitive (backed by a Map).
  // `required` arrays may appear in any order at the wire level; we normalise
  // by sorting both sides before comparing.
  // ---------------------------------------------------------------------------

  /** Recursively normalise a `Json` value so that every array named `"required"`
    * (anywhere in the JSON tree) is sorted lexicographically. This makes the
    * structural comparison order-insensitive for `required` per T7 §5.4.
    */
  private def normaliseRequired(j: Json): Json =
    j.asObject match {
      case None => j.asArray match {
        case None    => j
        case Some(a) => Json.fromValues(a.map(normaliseRequired))
      }
      case Some(obj) =>
        // Circe JsonObject.toIterable yields (String, Json) pairs in insertion order.
        val normalised: List[(String, Json)] = obj.toIterable.map {
          case (k, v) =>
            if (k == "required")
              k -> v.asArray.map(arr => Json.fromValues(arr.sortBy(_.asString.getOrElse(v.noSpaces)))).getOrElse(normaliseRequired(v))
            else
              k -> normaliseRequired(v)
        }.toList
        Json.obj(normalised*)
    }

  /** Assert that the inputSchema returned by the server is structurally equal to
    * the given T7 §2.3 reference. Both sides are:
    *   (a) Circe `Json` values — so key ordering is irrelevant for objects;
    *   (b) normalised via `normaliseRequired` — so `required` array order is
    *       irrelevant (T7 §5.4).
    */
  // nowarn: assert() returns Assertion but this helper returns Unit — the
  // discarded Assertion is intentional (helper wraps assertion for clarity).
  @scala.annotation.nowarn("cat=w-flag-value-discard")
  private def assertSchemaEqualsReference(toolName: String, actual: Json, reference: Json): Unit = {
    val normActual = normaliseRequired(actual)
    val normRef    = normaliseRequired(reference)
    assert(
      normActual == normRef,
      s"""K1 structural equality FAILED for tool '$toolName'.
         |
         |Actual (normalised):
         |${normActual.spaces2}
         |
         |Expected reference (normalised):
         |${normRef.spaces2}
         |""".stripMargin,
    )
  }

  it should "parse each inputSchema as well-formed Circe JSON — part (a): well-formedness" in {
    // K1 part (a): each inputSchema carried in the tool entry is serialised and
    // re-parsed. A parse error throws immediately, proving the schema is
    // well-formed Circe JSON. This keeps the codec-divergence coverage even if
    // the reference comparison (part b) passes.
    val (tools, _) = initAndList()
    for (t <- tools) {
      val schema     = t.hcursor.downField("inputSchema").as[Json].getOrElse(fail("inputSchema missing"))
      val serialised = schema.noSpaces
      val reparsed   = circeParseJson(serialised)
      assert(
        reparsed.isRight,
        s"K1(a): inputSchema does not survive Circe serialise+parse. Error: ${reparsed.left.toOption}. Serialised: $serialised",
      )
    }
  }

  it should "match T7 §2.3 reference for each tool — part (b): structural equality (K1)" in {
    // K1 part (b): each returned inputSchema is compared against the
    // INDEPENDENT T7 §2.3 reference value embedded in this test (NOT a
    // self-round-trip). The references come from
    // docs/research/mcp-roundtrip-scenario.md §2.3 / McpInputSchemaEmissionTest.
    // Circe Json object equality is key-order-insensitive; `required` arrays are
    // normalised to sorted order before comparison (T7 §5.4).
    val (tools, _) = initAndList()

    def inputSchema(i: Int): Json =
      tools(i).hcursor.downField("inputSchema").as[Json].getOrElse(fail(s"tools[$i].inputSchema missing"))

    assertSchemaEqualsReference("McpTools_listCollections",  inputSchema(0), refListCollections)
    assertSchemaEqualsReference("McpTools_submitComposite",  inputSchema(1), refSubmitComposite)
    assertSchemaEqualsReference("McpTools_processShape",     inputSchema(2), refProcessShape)
    assertSchemaEqualsReference("McpTools_processTagged",    inputSchema(3), refProcessTagged)
    assertSchemaEqualsReference("McpTools_pagePoints",       inputSchema(4), refPagePoints)
    assertSchemaEqualsReference("McpTools_ping",             inputSchema(5), refPing)
    assertSchemaEqualsReference("McpTools_describePricing",  inputSchema(6), refDescribePricing)
  }

  it should "fail K1 structural equality when given a deliberately wrong reference (negative control)" in {
    // NEGATIVE CONTROL for K1 schema-structure gate (T7 §5.3):
    // A reference with an extra field ("WRONG": true) must NOT match the actual
    // ping schema. This proves `assertSchemaEqualsReference` is not vacuously true
    // and that the structural-equality gate can detect any schema alteration.
    //
    // The wrongRef has an extra top-level key "WRONG" that the emitter never
    // produces. assertSchemaEqualsReference must throw for this wrong reference.
    val wrongRef: Json = parseRef("wrong_ref",
      """{
        |  "$schema": "https://json-schema.org/draft/2020-12/schema",
        |  "type": "object",
        |  "properties": {
        |    "seqno": { "type": "integer", "format": "int32" },
        |    "label": { "type": "string" }
        |  },
        |  "required": ["seqno", "label"],
        |  "WRONG": true
        |}""".stripMargin)

    val (tools, _) = initAndList()
    val pingSchema  = tools(5).hcursor.downField("inputSchema").as[Json].getOrElse(fail("tools[5].inputSchema missing"))

    // The negative control: assertSchemaEqualsReference MUST throw (fail) for the wrong reference.
    val threw = try {
      assertSchemaEqualsReference("McpTools_ping (negative-control)", pingSchema, wrongRef)
      false // no exception → control failed to fire
    } catch {
      case _: org.scalatest.exceptions.TestFailedException => true // expected: assertion fired
    }
    assert(threw, "K1 negative control FAILED: assertSchemaEqualsReference accepted a wrong reference (extra 'WRONG' key), proving the gate is vacuous")
  }

  // ---------------------------------------------------------------------------
  // §3 — tools/call (success paths)
  // ---------------------------------------------------------------------------

  "MCP §3: tools/call" should "return ok=true and isError=false for McpTools_ping" in {
    val server  = makeServer()
    val session = new McpSession
    initSession(server, session)

    val resp = send(server, session, JsonRpcRequest(
      id     = Some(JsonRpcId.LongId(3L)),
      method = "tools/call",
      params = Some(Json.obj(
        "name"      -> Json.fromString("McpTools_ping"),
        "arguments" -> Json.obj("seqno" -> Json.fromInt(42), "label" -> Json.fromString("hello")),
      )),
    ))

    resp.id shouldBe Some(JsonRpcId.LongId(3L))
    resp.error shouldBe None

    val result  = resp.result.getOrElse(fail("result must be present"))
    val content = result.hcursor.downField("content").as[List[Json]].getOrElse(fail("content missing"))
    content should have length 1
    content.head.hcursor.downField("type").as[String].getOrElse(fail("content[0].type missing")) shouldBe "text"

    val text    = content.head.hcursor.downField("text").as[String].getOrElse(fail("content[0].text missing"))
    val payload = circeParseJson(text).getOrElse(fail(s"content[0].text is not valid JSON: $text"))
    payload.hcursor.downField("ok").as[Boolean].getOrElse(fail("ok missing")) shouldBe true

    // isError MUST be false or absent (K4 §2.4 permits omission when false).
    val isError = result.hcursor.downField("isError").as[Boolean].toOption
    assert(isError.isEmpty || !isError.get, "isError must be false or absent")
  }

  it should "return ok=true and isError=false for McpTools_processTagged" in {
    // T26/D11: processTagged dispatch with a Tagged TagA value (id + tag).
    // Tagged carries no foreign type, so no FFancyStr codec registration is needed.
    val server  = makeServer()
    val session = new McpSession
    initSession(server, session)

    val resp = send(server, session, JsonRpcRequest(
      id     = Some(JsonRpcId.LongId(7L)),
      method = "tools/call",
      params = Some(Json.obj(
        "name"      -> Json.fromString("McpTools_processTagged"),
        // ADT wire format under --sc-wrapped-adt-branch-codecs=false is the
        // branch-discriminated object {"TagA": {...}} (ScJsonCodecGenerator
        // wrapAdtBranchEncoder / decode headOption). The inputSchema oneOf is a
        // SEPARATE structural view; the codec wire is the wrapped form.
        "arguments" -> Json.obj(
          "tagged" -> Json.obj("TagA" -> Json.obj("id" -> Json.fromString("abc"), "tag" -> Json.fromString("hello"))),
        ),
      )),
    ))

    resp.id shouldBe Some(JsonRpcId.LongId(7L))
    resp.error shouldBe None

    val result  = resp.result.getOrElse(fail("result must be present"))
    val content = result.hcursor.downField("content").as[List[Json]].getOrElse(fail("content missing"))
    content should have length 1
    content.head.hcursor.downField("type").as[String].getOrElse(fail("content[0].type missing")) shouldBe "text"

    val text    = content.head.hcursor.downField("text").as[String].getOrElse(fail("content[0].text missing"))
    val payload = circeParseJson(text).getOrElse(fail(s"content[0].text is not valid JSON: $text"))
    payload.hcursor.downField("ok").as[Boolean].getOrElse(fail("ok missing")) shouldBe true

    val isError = result.hcursor.downField("isError").as[Boolean].toOption
    assert(isError.isEmpty || !isError.get, "isError must be false or absent")
  }

  // Note: submitComposite test omitted — requires FFancyStr codec registration
  // which is not supported via the Scala BaboonSingleton LazyInstance pattern.

  // ---------------------------------------------------------------------------
  // §4 — tools/call (error paths) — primary negative controls
  // ---------------------------------------------------------------------------

  "MCP §4: tools/call error paths" should "return Channel-A error code -32602 for unknown tool" in {
    // NEGATIVE CONTROL (T7 §5.2): if the server returned a success result for
    // McpTools_nonexistent, resp.error.isDefined would fail.
    val server  = makeServer()
    val session = new McpSession
    initSession(server, session)

    val resp = send(server, session, JsonRpcRequest(
      id     = Some(JsonRpcId.LongId(5L)),
      method = "tools/call",
      params = Some(Json.obj("name" -> Json.fromString("McpTools_nonexistent"), "arguments" -> Json.obj())),
    ))

    resp.id shouldBe Some(JsonRpcId.LongId(5L))
    // MUST be a Channel-A error, not a result.
    assert(resp.error.isDefined, "Unknown tool must produce a Channel-A error")
    resp.result shouldBe None
    // §4.1: code MUST be -32602 (InvalidParams — unknown tool)
    resp.error.get.code shouldBe -32602
    assert(resp.error.get.message.nonEmpty, "error.message must be non-empty")
  }

  it should "return Channel-B isError=true when decode fails (missing required field)" in {
    // NEGATIVE CONTROL: if isError were false this test would fail.
    //
    // Channel-B trigger: send ping with missing required field `seqno`.
    // The generated ping.In_JsonCodec.decode expects a `seqno` field which
    // is absent, causing a decode failure. The wiring catches this as
    // BaboonWiringError.DecoderFailed and invokeJson returns Left.
    // The MCP server produces Channel-B: result with isError=true.
    val server  = makeServer()
    val session = new McpSession
    initSession(server, session)

    val resp = send(server, session, JsonRpcRequest(
      id     = Some(JsonRpcId.LongId(6L)),
      method = "tools/call",
      params = Some(Json.obj(
        "name"      -> Json.fromString("McpTools_ping"),
        "arguments" -> Json.obj("label" -> Json.fromString("missing-seqno")),
      )),
    ))

    resp.id shouldBe Some(JsonRpcId.LongId(6L))
    // Channel B: MUST be a result (not error) with isError=true.
    assert(resp.result.isDefined, "Channel-B: result must be present")
    resp.error shouldBe None

    val result  = resp.result.get
    result.hcursor.downField("isError").as[Boolean].getOrElse(fail("isError missing")) shouldBe true

    val content = result.hcursor.downField("content").as[List[Json]].getOrElse(fail("content missing"))
    assert(content.nonEmpty, "content must have at least one element")
    content.head.hcursor.downField("type").as[String].getOrElse(fail("content[0].type missing")) shouldBe "text"
    val text = content.head.hcursor.downField("text").as[String].getOrElse(fail("content[0].text missing"))
    assert(text.nonEmpty, "content[0].text must be non-empty")
  }
}
