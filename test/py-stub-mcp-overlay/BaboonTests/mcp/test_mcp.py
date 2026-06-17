# T18 — Python MCP round-trip overlay test.
#
# Drives the generated McpToolsMcpServer[Ctx] through the canonical T7 scenario
# (docs/research/mcp-roundtrip-scenario.md) using an entirely in-process fake
# transport. No stdio or HTTP is involved.
#
# Assertion discipline (T7 §5.1):
#   All assertions use unconditional `raise` on failure (not bare `assert`).
#   Python test framework assertions (unittest) raise AssertionError unconditionally.
#
# K1 inputSchema validation (T7 §5.3 tier T12-T18):
#   Part (a) — well-formedness: each returned inputSchema round-trips through
#     json.loads without throwing, proving it is well-formed JSON.
#   Part (b) — structural equality: each returned inputSchema is asserted
#     STRUCTURALLY EQUAL to the corresponding T7 §2.3 reference value.
#     `required` arrays are compared as SETS per §5.4.
#     `$defs` keys are compared by lookup (key-order-insensitive via dict ==).
#
# Negative controls (T7 §5.2):
#   - §4.1 (unknown tool → -32602): server must return a JSON-RPC error object.
#   - §4.2 (malformed decode → Channel-B isError=true): server must return
#     isError=true in result, not a JSON-RPC error.
#   - Schema structural-equality negative control: a deliberately-wrong
#     reference must make the comparator return False; the test FAILS if the
#     comparator erroneously returns True.
#
# Run from py-stub/:
#   python3 -m unittest BaboonTests.mcp.test_mcp

import json
import unittest

from BaboonDefinitions.Generated.baboon_codecs import BaboonCodecContext
from BaboonDefinitions.Generated.baboon_mcp_runtime import McpSession
from BaboonDefinitions.Generated.mcp.stub.BaboonServiceRt import BaboonServiceRtDefault, IBaboonServiceRt
from BaboonDefinitions.Generated.mcp.stub.McpTools import McpTools
from BaboonDefinitions.Generated.mcp.stub.McpToolsMcpServer import McpToolsMcpServer
from BaboonDefinitions.Generated.mcp.stub.McpTools_Wiring import invoke_json_McpTools
from BaboonDefinitions.Generated.mcp.stub.mcptools.listcollections.Out import Out as ListCollectionsOut
from BaboonDefinitions.Generated.mcp.stub.mcptools.submitcomposite.Out import Out as SubmitCompositeOut
from BaboonDefinitions.Generated.mcp.stub.mcptools.processshape.Out import Out as ProcessShapeOut
from BaboonDefinitions.Generated.mcp.stub.mcptools.processtagged.Out import Out as ProcessTaggedOut
from BaboonDefinitions.Generated.mcp.stub.mcptools.pagepoints.Out import Out as PagePointsOut
from BaboonDefinitions.Generated.mcp.stub.mcptools.ping.Out import Out as PingOut

# ---------------------------------------------------------------------------
# T7 §2.3 reference inputSchema values (authoritative: McpInputSchemaEmitter
# golden test McpInputSchemaEmissionTest.scala + mcp-roundtrip-scenario.md §2.3).
# Python dict == comparison is recursive + key-order-insensitive (§5.4).
# ---------------------------------------------------------------------------

# Tool 1: McpTools_listCollections — list/set/map[str]/map[enum-key]
REF_LIST_COLLECTIONS = json.loads(
    '{"$schema":"https://json-schema.org/draft/2020-12/schema",'
    '"type":"object",'
    '"properties":{'
    '"tags":{"type":"array","items":{"type":"string"}},'
    '"uniqueIds":{"type":"array","items":{"type":"integer","format":"int64"},"uniqueItems":true},'
    '"labels":{"type":"object","additionalProperties":{"type":"string"}},'
    '"byColor":{"type":"object","additionalProperties":{"type":"string"},'
    '"propertyNames":{"type":"string","enum":["Red","Green","Blue"]}}'
    '},'
    '"required":["tags","uniqueIds","labels","byColor"],'
    '"$defs":{"mcp_stub_Color":{"type":"string","enum":["Red","Green","Blue"]}}'
    '}'
)

# Tool 2: McpTools_submitComposite — nested DTO + opt[DTO] + enum + foreign-string
REF_SUBMIT_COMPOSITE = json.loads(
    '{"$schema":"https://json-schema.org/draft/2020-12/schema",'
    '"type":"object",'
    '"properties":{'
    '"nested":{"$ref":"#/$defs/mcp_stub_Nested"},'
    '"maybePoint":{"oneOf":[{"$ref":"#/$defs/mcp_stub_Point"},{"type":"null"}]},'
    '"color":{"$ref":"#/$defs/mcp_stub_Color"},'
    '"fancy":{"type":"string"}'
    '},'
    '"required":["nested","color","fancy"],'
    '"$defs":{'
    '"mcp_stub_Color":{"type":"string","enum":["Red","Green","Blue"]},'
    '"mcp_stub_Nested":{"type":"object","properties":{'
    '"point":{"$ref":"#/$defs/mcp_stub_Point"},'
    '"color":{"$ref":"#/$defs/mcp_stub_Color"},'
    '"label":{"oneOf":[{"type":"string"},{"type":"null"}]}'
    '},"required":["point","color"]},'
    '"mcp_stub_Point":{"type":"object","properties":{'
    '"x":{"type":"integer","format":"int32"},'
    '"y":{"type":"integer","format":"int32"}'
    '},"required":["x","y"]}'
    '}'
    '}'
)

# Tool 3: McpTools_processShape — ADT oneOf + recursive Tree
REF_PROCESS_SHAPE = json.loads(
    '{"$schema":"https://json-schema.org/draft/2020-12/schema",'
    '"type":"object",'
    '"properties":{'
    '"shape":{"$ref":"#/$defs/mcp_stub_Shape"},'
    '"tree":{"$ref":"#/$defs/mcp_stub_Tree"}'
    '},'
    '"required":["shape","tree"],'
    '"$defs":{'
    '"mcp_stub_Shape":{"oneOf":[{"$ref":"#/$defs/mcp_stub_Shape_Circle"},{"$ref":"#/$defs/mcp_stub_Shape_Rect"}]},'
    '"mcp_stub_Tree":{"type":"object","properties":{'
    '"value":{"type":"integer","format":"int32"},'
    '"left":{"oneOf":[{"$ref":"#/$defs/mcp_stub_Tree"},{"type":"null"}]},'
    '"children":{"type":"array","items":{"$ref":"#/$defs/mcp_stub_Tree"}}'
    '},"required":["value","children"]},'
    '"mcp_stub_Shape_Circle":{"type":"object","properties":{'
    '"radius":{"type":"number","format":"double"}'
    '},"required":["radius"]},'
    '"mcp_stub_Shape_Rect":{"type":"object","properties":{'
    '"w":{"type":"number","format":"double"},'
    '"h":{"type":"number","format":"double"}'
    '},"required":["w","h"]}'
    '}'
    '}'
)

# Tool 4: McpTools_processTagged — contract-bearing ADT (T26/D11). `Tagged` is
# `is HasId`; the HasId contract carries `id: str`, merged into every branch DTO
# at typing time. Each branch $defs entry has `id` + own field, both required,
# NO allOf. Branch order in oneOf is declaration order: TagA then TagB.
REF_PROCESS_TAGGED = json.loads(
    '{"$schema":"https://json-schema.org/draft/2020-12/schema",'
    '"type":"object",'
    '"properties":{'
    '"tagged":{"$ref":"#/$defs/mcp_stub_Tagged"}'
    '},'
    '"required":["tagged"],'
    '"$defs":{'
    '"mcp_stub_Tagged":{"oneOf":[{"$ref":"#/$defs/mcp_stub_Tagged_TagA"},{"$ref":"#/$defs/mcp_stub_Tagged_TagB"}]},'
    '"mcp_stub_Tagged_TagA":{"type":"object","properties":{'
    '"id":{"type":"string"},'
    '"tag":{"type":"string"}'
    '},"required":["id","tag"]},'
    '"mcp_stub_Tagged_TagB":{"type":"object","properties":{'
    '"id":{"type":"string"},'
    '"weight":{"type":"integer","format":"int32"}'
    '},"required":["id","weight"]}'
    '}'
    '}'
)

# Tool 5: McpTools_pagePoints — template-instantiation alias PointPage = Page[Point]
REF_PAGE_POINTS = json.loads(
    '{"$schema":"https://json-schema.org/draft/2020-12/schema",'
    '"type":"object",'
    '"properties":{'
    '"page":{"$ref":"#/$defs/mcp_stub_PointPage"}'
    '},'
    '"required":["page"],'
    '"$defs":{'
    '"mcp_stub_Point":{"type":"object","properties":{'
    '"x":{"type":"integer","format":"int32"},'
    '"y":{"type":"integer","format":"int32"}'
    '},"required":["x","y"]},'
    '"mcp_stub_PointPage":{"type":"object","properties":{'
    '"items":{"type":"array","items":{"$ref":"#/$defs/mcp_stub_Point"}},'
    '"total":{"type":"integer","format":"int32","minimum":0}'
    '},"required":["items","total"]}'
    '}'
    '}'
)

# Tool 6: McpTools_ping — scalar-only, no $defs
REF_PING = json.loads(
    '{"$schema":"https://json-schema.org/draft/2020-12/schema",'
    '"type":"object",'
    '"properties":{'
    '"seqno":{"type":"integer","format":"int32"},'
    '"label":{"type":"string"}'
    '},'
    '"required":["seqno","label"]}'
)

REFERENCES = [
    REF_LIST_COLLECTIONS,   # tools[0] = McpTools_listCollections
    REF_SUBMIT_COMPOSITE,   # tools[1] = McpTools_submitComposite
    REF_PROCESS_SHAPE,      # tools[2] = McpTools_processShape
    REF_PROCESS_TAGGED,     # tools[3] = McpTools_processTagged
    REF_PAGE_POINTS,        # tools[4] = McpTools_pagePoints
    REF_PING,               # tools[5] = McpTools_ping
]

# ---------------------------------------------------------------------------
# Structural equality helper (T7 §5.4):
#   - dict: compare by key lookup (key-order-insensitive — Python dict == is already).
#   - list for key "required": compare as a set (order-insensitive).
#   - All other lists: compare element-by-element (ordered).
#   - Scalars: ==
# ---------------------------------------------------------------------------

def schemas_structurally_equal(actual, expected, _in_required_key=False):
    """Recursive structural-equality check for JSON Schema objects.

    `required` arrays are compared as sets (order-insensitive, T7 §5.4).
    `$defs` and other objects are compared by key lookup (dict == does this).
    """
    if _in_required_key:
        # Both must be lists; compare as sets of primitive values.
        if not isinstance(actual, list) or not isinstance(expected, list):
            return False
        return set(json.dumps(x) for x in actual) == set(json.dumps(x) for x in expected)

    if isinstance(expected, dict):
        if not isinstance(actual, dict):
            return False
        if set(actual.keys()) != set(expected.keys()):
            return False
        for key, exp_val in expected.items():
            act_val = actual.get(key)
            if not schemas_structurally_equal(act_val, exp_val, _in_required_key=(key == "required")):
                return False
        return True

    if isinstance(expected, list):
        if not isinstance(actual, list):
            return False
        if len(actual) != len(expected):
            return False
        return all(schemas_structurally_equal(a, e) for a, e in zip(actual, expected))

    return actual == expected


# ---------------------------------------------------------------------------
# Stub McpTools service: every method returns ok=true (T7 §3 convention).
# ---------------------------------------------------------------------------

class _StubMcpTools(McpTools):
    # Pydantic v2 models require keyword arguments — use ok=True, not True.
    def listCollections(self, arg): return ListCollectionsOut(ok=True)
    def submitComposite(self, arg): return SubmitCompositeOut(ok=True)
    def processShape(self, arg): return ProcessShapeOut(ok=True)
    def processTagged(self, arg): return ProcessTaggedOut(ok=True)
    def pagePoints(self, arg): return PagePointsOut(ok=True)
    def ping(self, arg): return PingOut(ok=True)


# ---------------------------------------------------------------------------
# Test helpers
# ---------------------------------------------------------------------------

def _make_server():
    rt = BaboonServiceRtDefault()
    stub = _StubMcpTools()
    codec_ctx = BaboonCodecContext.Default
    def _fake_invoke(method, data, ctx, codec_context):
        return invoke_json_McpTools(method, data, stub, rt, codec_context)
    return McpToolsMcpServer(_fake_invoke)


def _init_session(server, session, codec_ctx):
    """Send initialize + notifications/initialized to set up session."""
    server.handle(
        {
            "jsonrpc": "2.0",
            "id": 0,
            "method": "initialize",
            "params": {
                "protocolVersion": "2025-06-18",
                "capabilities": {},
                "clientInfo": {"name": "test-client", "version": "0.0.1"},
            },
        },
        session,
        None,
        codec_ctx,
    )
    notif_resp = server.handle(
        {"jsonrpc": "2.0", "method": "notifications/initialized"},
        session,
        None,
        codec_ctx,
    )
    if notif_resp is not None:
        raise AssertionError(f"notifications/initialized must produce None, got {notif_resp!r}")


def _send(server, session, codec_ctx, req):
    resp = server.handle(req, session, None, codec_ctx)
    if resp is None:
        raise AssertionError(f"Expected a response for {req['method']!r} but got None")
    return resp


def _init_and_list():
    codec_ctx = BaboonCodecContext.Default
    server = _make_server()
    session = McpSession()
    _init_session(server, session, codec_ctx)
    resp = _send(
        server,
        session,
        codec_ctx,
        {"jsonrpc": "2.0", "id": 2, "method": "tools/list"},
    )
    tools = resp["result"]["tools"]
    return tools, resp, server, session, codec_ctx


# ---------------------------------------------------------------------------
# T7 §1 — initialize
# ---------------------------------------------------------------------------

class Sec1InitializeTests(unittest.TestCase):

    def test_initialize_response_is_correct(self):
        codec_ctx = BaboonCodecContext.Default
        server = _make_server()
        session = McpSession()

        resp = _send(
            server,
            session,
            codec_ctx,
            {
                "jsonrpc": "2.0",
                "id": 1,
                "method": "initialize",
                "params": {
                    "protocolVersion": "2025-06-18",
                    "capabilities": {},
                    "clientInfo": {"name": "test-client", "version": "0.0.1"},
                },
            },
        )

        self.assertEqual(resp["id"], 1, "id must be 1")
        self.assertNotIn("error", resp, "Expected no error for initialize")

        result = resp["result"]
        self.assertEqual(
            result["protocolVersion"],
            "2025-06-18",
            "protocolVersion must be 2025-06-18",
        )

        caps = result["capabilities"]
        self.assertEqual(len(caps), 1, "capabilities must have exactly one key")
        self.assertIn("tools", caps, "capabilities.tools must be present")
        self.assertEqual(caps["tools"], {}, "capabilities.tools must be {}")

        info = result["serverInfo"]
        self.assertIsInstance(info["name"], str, "serverInfo.name must be a string")
        self.assertGreater(len(info["name"]), 0, "serverInfo.name must be non-empty")
        self.assertIsInstance(info["version"], str, "serverInfo.version must be a string")
        self.assertGreater(len(info["version"]), 0, "serverInfo.version must be non-empty")

    def test_initialized_notification_produces_no_response(self):
        codec_ctx = BaboonCodecContext.Default
        server = _make_server()
        session = McpSession()

        server.handle(
            {
                "jsonrpc": "2.0",
                "id": 0,
                "method": "initialize",
                "params": {"protocolVersion": "2025-06-18", "capabilities": {}, "clientInfo": {"name": "t", "version": "0"}},
            },
            session,
            None,
            codec_ctx,
        )

        notif_resp = server.handle(
            {"jsonrpc": "2.0", "method": "notifications/initialized"},
            session,
            None,
            codec_ctx,
        )
        self.assertIsNone(notif_resp, "notifications/initialized MUST produce no response (None)")


# ---------------------------------------------------------------------------
# T7 §2 — tools/list + inputSchema validation (K1 tier)
# ---------------------------------------------------------------------------

class Sec2ToolsListTests(unittest.TestCase):

    def test_exactly_six_tools_in_declaration_order(self):
        tools, resp, *_ = _init_and_list()

        self.assertEqual(resp["id"], 2, "id must be 2")
        self.assertNotIn("error", resp)
        self.assertEqual(len(tools), 6, "MUST be exactly 6 tools")

        # Exact position assertions (model declaration order, T7 §0).
        # processTagged is declared between processShape and pagePoints (T26/D11),
        # so it occupies index 3 and shifts pagePoints→4, ping→5.
        # DELIBERATE-NEGATIVE-CONTROL: replacing "McpTools_ping" with "McpTools_WRONG"
        # on the next line makes this test fail, proving position[5] check is live.
        self.assertEqual(tools[0]["name"], "McpTools_listCollections")
        self.assertEqual(tools[1]["name"], "McpTools_submitComposite")
        self.assertEqual(tools[2]["name"], "McpTools_processShape")
        self.assertEqual(tools[3]["name"], "McpTools_processTagged")
        self.assertEqual(tools[4]["name"], "McpTools_pagePoints")
        self.assertEqual(tools[5]["name"], "McpTools_ping")

        # No "nextCursor" key (§2.2)
        self.assertNotIn("nextCursor", resp["result"], "nextCursor must not be present")

        # T119: McpTools_ping carries a distinctive doc comment in
        # mcp_stub.baboon; its tools/list entry must expose that text as
        # "description". Every other (undocumented) tool must have no
        # description key.
        documented_tool_name = "McpTools_ping"
        documented_tool_description = "Liveness probe returning a fixed acknowledgement token."
        for t in tools:
            if t["name"] == documented_tool_name:
                self.assertEqual(
                    documented_tool_description,
                    t.get("description"),
                    f"Tool {t['name']} must carry its doc-comment description",
                )
            else:
                self.assertNotIn("description", t, f"Tool {t['name']} must have no description")

    def test_each_inputSchema_has_draft202012_schema_uri(self):
        tools, *_ = _init_and_list()
        for t in tools:
            schema = t["inputSchema"]
            self.assertEqual(
                schema.get("$schema"),
                "https://json-schema.org/draft/2020-12/schema",
                f"Tool {t['name']}: $schema must be the Draft 2020-12 URI",
            )

    def test_k1_all_inputSchemas_are_well_formed_json(self):
        # K1 part (a) — well-formedness gate: each inputSchema must survive
        # json.loads(json.dumps(...)) without throwing.
        tools, *_ = _init_and_list()
        for t in tools:
            schema_repr = json.dumps(t["inputSchema"])
            reparsed = json.loads(schema_repr)
            if reparsed is None:
                raise AssertionError(f"Tool {t['name']}: schema must not be None after re-parse")

    def test_k1_all_six_tools_structural_equality_to_t7_reference(self):
        # K1 part (b) — structural equality to T7 §2.3 reference.
        # Each returned inputSchema is re-parsed via json.dumps/loads (codec-divergence
        # coverage) and compared key-by-key recursively to the embedded T7 reference.
        # "required" arrays are compared as SETS per T7 §5.4.
        # "$defs" keys are compared by lookup (key-order-insensitive via dict ==).
        tools, *_ = _init_and_list()

        for i, (t, ref) in enumerate(zip(tools, REFERENCES)):
            tool_name = t["name"]
            # Re-parse through json to exercise codec round-trip.
            actual = json.loads(json.dumps(t["inputSchema"]))
            if not schemas_structurally_equal(actual, ref):
                raise AssertionError(
                    f"Tool {tool_name} (index {i}): inputSchema is not structurally equal "
                    f"to T7 §2.3 reference.\n"
                    f"  actual:   {json.dumps(actual, indent=2)}\n"
                    f"  expected: {json.dumps(ref, indent=2)}"
                )

    def test_k1_negative_control_wrong_reference_detected_by_comparator(self):
        # NEGATIVE CONTROL (T7 §5.2 / schema structure):
        # Asserts that the structural comparator DETECTS a wrong reference.
        # If schemas_structurally_equal erroneously returns True for an incorrect
        # schema, this test FAILS — proving the comparator is live.
        #
        # Wrong reference: ping schema with an extra top-level field "extra":"bad".
        wrong_ref = json.loads(
            '{"$schema":"https://json-schema.org/draft/2020-12/schema",'
            '"type":"object",'
            '"properties":{'
            '"seqno":{"type":"integer","format":"int32"},'
            '"label":{"type":"string"}'
            '},'
            '"required":["seqno","label"],'
            '"extra":"bad"}'
        )
        tools, *_ = _init_and_list()
        actual_ping = json.loads(json.dumps(tools[5]["inputSchema"]))

        # The comparator MUST return False for the wrong reference.
        if schemas_structurally_equal(actual_ping, wrong_ref):
            raise AssertionError(
                "NEGATIVE CONTROL FAILED: schemas_structurally_equal returned True for a "
                "deliberately-wrong reference (extra field 'extra'). The comparator is not "
                "functioning correctly."
            )

        # And MUST return True for the correct reference.
        if not schemas_structurally_equal(actual_ping, REF_PING):
            raise AssertionError(
                "Positive case failed after negative control: ping schema must equal REF_PING."
            )


# ---------------------------------------------------------------------------
# T7 §3 — tools/call (success paths)
# ---------------------------------------------------------------------------

class Sec3ToolsCallSuccessTests(unittest.TestCase):

    def test_ping_returns_ok_true(self):
        tools, resp_list, server, session, codec_ctx = _init_and_list()

        resp = _send(
            server,
            session,
            codec_ctx,
            {
                "jsonrpc": "2.0",
                "id": 3,
                "method": "tools/call",
                "params": {"name": "McpTools_ping", "arguments": {"seqno": 42, "label": "hello"}},
            },
        )

        self.assertEqual(resp["id"], 3)
        self.assertNotIn("error", resp, "Unexpected error on ping call")

        result = resp["result"]
        content = result["content"]
        self.assertEqual(len(content), 1, "content must have exactly one element")
        self.assertEqual(content[0]["type"], "text")

        payload = json.loads(content[0]["text"])
        self.assertEqual(payload["ok"], True, "ok must be true")

        # isError MUST be false or absent (K4 §2.4)
        is_error = result.get("isError")
        if is_error is not None and is_error is not False:
            raise AssertionError(f"isError must be false or absent, got {is_error!r}")

    def test_listCollections_returns_ok_true(self):
        tools, resp_list, server, session, codec_ctx = _init_and_list()

        # D6/T30: byColor is map[Color,str] with an enum key. The Python JSON codec
        # stringifies the enum key to its wire-name (`value.value`) and decodes it
        # back via the enum constructor, so a NON-EMPTY enum-keyed map round-trips as
        # a string-keyed JSON object. (Previously this test passed an empty dict {} to
        # dodge the missing Typedef.Enum key-codec arm — the BUG-throw fixed by T30.)
        byColor = {"Green": "ok", "Red": "stop"}

        # The non-empty instance must conform to the tool's inputSchema (the wire form
        # the schema now declares): every key is one of the enum's wire values and
        # every value is a string. (py-stub has no jsonschema dependency, so validate
        # the byColor fragment of the reference schema structurally.)
        by_color_schema = REF_LIST_COLLECTIONS["properties"]["byColor"]
        self.assertEqual(by_color_schema["type"], "object")
        allowed_keys = set(by_color_schema["propertyNames"]["enum"])
        for k, v in byColor.items():
            self.assertIn(k, allowed_keys, f"byColor key {k!r} must be an enum wire value")
            self.assertIsInstance(v, str, "byColor value must be a string")

        resp = _send(
            server,
            session,
            codec_ctx,
            {
                "jsonrpc": "2.0",
                "id": 4,
                "method": "tools/call",
                "params": {
                    "name": "McpTools_listCollections",
                    "arguments": {
                        "tags": ["a", "b"],
                        "uniqueIds": [1, 2],
                        "labels": {"k": "v"},
                        "byColor": byColor,
                    },
                },
            },
        )

        self.assertEqual(resp["id"], 4)
        self.assertNotIn("error", resp, "Unexpected error on listCollections call")

        result = resp["result"]
        content = result["content"]
        self.assertEqual(len(content), 1)
        self.assertEqual(content[0]["type"], "text")

        payload = json.loads(content[0]["text"])
        self.assertEqual(payload["ok"], True, "ok must be true")

        is_error = result.get("isError")
        if is_error is not None and is_error is not False:
            raise AssertionError(f"isError must be false or absent, got {is_error!r}")

    def test_processTagged_returns_ok_true(self):
        # T26/D11: processTagged dispatch with a Tagged TagA value.
        # ADT wire format under --py-wrapped-adt-branch-codecs=false is the
        # branch-discriminated object {"TagA": {...}} (the codec wraps the branch;
        # the inputSchema oneOf is a separate structural view). Tagged carries no
        # foreign type, so no FFancyStr codec registration is needed.
        tools, resp_list, server, session, codec_ctx = _init_and_list()

        resp = _send(
            server,
            session,
            codec_ctx,
            {
                "jsonrpc": "2.0",
                "id": 7,
                "method": "tools/call",
                "params": {
                    "name": "McpTools_processTagged",
                    "arguments": {"tagged": {"TagA": {"id": "abc", "tag": "hello"}}},
                },
            },
        )

        self.assertEqual(resp["id"], 7)
        self.assertNotIn("error", resp, "Unexpected error on processTagged call")

        result = resp["result"]
        content = result["content"]
        self.assertEqual(len(content), 1)
        self.assertEqual(content[0]["type"], "text")

        payload = json.loads(content[0]["text"])
        self.assertEqual(payload["ok"], True, "ok must be true")

        is_error = result.get("isError")
        if is_error is not None and is_error is not False:
            raise AssertionError(f"isError must be false or absent, got {is_error!r}")


# ---------------------------------------------------------------------------
# T7 §4 — tools/call (error paths) — primary negative controls
# ---------------------------------------------------------------------------

class Sec4ErrorPathTests(unittest.TestCase):

    def test_unknown_tool_channel_a_error_code_32602(self):
        # NEGATIVE CONTROL (T7 §5.2): if the server returned a success result for
        # McpTools_nonexistent, assertIn("error", resp) below would fail.
        tools, resp_list, server, session, codec_ctx = _init_and_list()

        resp = _send(
            server,
            session,
            codec_ctx,
            {
                "jsonrpc": "2.0",
                "id": 5,
                "method": "tools/call",
                "params": {"name": "McpTools_nonexistent", "arguments": {}},
            },
        )

        self.assertEqual(resp["id"], 5)
        # MUST be a Channel-A error, not a result.
        self.assertIn("error", resp, "Unknown tool must produce a Channel-A error")
        self.assertNotIn("result", resp, "No result expected for unknown tool")
        # §4.1: code MUST be -32602 (InvalidParams — unknown tool)
        self.assertEqual(resp["error"]["code"], -32602, "Unknown tool error code MUST be -32602")
        error_message = resp["error"]["message"]
        if not isinstance(error_message, str) or len(error_message) == 0:
            raise AssertionError(f"error.message must be a non-empty string, got {error_message!r}")

    def test_decode_failure_channel_b_is_error_true(self):
        # NEGATIVE CONTROL: if isError were false this test would fail.
        #
        # Channel-B trigger: send ping with missing required "seqno".
        # The generated ping.In_JsonCodec.decode raises an exception
        # because the required field is absent. The wiring catch-block wraps
        # it as DecoderFailed and invoke_json_McpTools returns BaboonLeft.
        # The MCP server produces Channel-B: result with isError=True.
        tools, resp_list, server, session, codec_ctx = _init_and_list()

        resp = _send(
            server,
            session,
            codec_ctx,
            {
                "jsonrpc": "2.0",
                "id": 6,
                "method": "tools/call",
                "params": {"name": "McpTools_ping", "arguments": {"label": "missing-seqno"}},
            },
        )

        self.assertEqual(resp["id"], 6)
        # Channel B: MUST be a result (not error) with isError=True.
        self.assertIn("result", resp, "Channel-B: result must be present")
        self.assertNotIn("error", resp, "Channel-B: must not be a JSON-RPC error")

        result = resp["result"]
        self.assertEqual(result.get("isError"), True, "isError MUST be True for Channel-B decode failure")

        content = result["content"]
        self.assertGreater(len(content), 0, "content must have at least one element")
        self.assertEqual(content[0]["type"], "text")
        if not isinstance(content[0]["text"], str) or len(content[0]["text"]) == 0:
            raise AssertionError(f"content[0].text must be a non-empty string")


if __name__ == "__main__":
    unittest.main()
