// T16 — Dart MCP round-trip overlay test.
//
// Drives the generated McpToolsMcpServer<McpTools> through the canonical T7
// scenario (docs/research/mcp-roundtrip-scenario.md) using an entirely
// in-process fake transport. No stdio or HTTP is involved.
//
// Assertion discipline (T7 §5.1):
//   - All assertions use test-package `expect`/`fail` which throw
//     unconditionally on failure. Dart `assert(...)` is elided outside debug
//     mode — this test uses ONLY `expect(...)`/`if (!cond) fail(...)`.
//
// dart:convert JSON Schema validation tier (K1 — T16 tier):
//   Part (a) — well-formedness: each returned inputSchema is re-encoded and
//     re-decoded via dart:convert `jsonEncode`/`jsonDecode` (well-formedness gate).
//   Part (b) — structural equality: each returned inputSchema is asserted
//     STRUCTURALLY EQUAL to the corresponding T7 §2.3 reference value.
//     `required` arrays are compared as SETS per §5.4.
//     `$defs` keys are compared by lookup (key-order-insensitive).
//
// Negative controls (T7 §5.2):
//   - §4.1 (unknown tool → -32602): if the server returned success for
//     McpTools_nonexistent the assertions below would fail.
//   - §4.2 (malformed decode → Channel-B isError=true): if isError were false
//     this test would fail.
//   - Schema structural-equality negative control (k1_negativeControl):
//     a deliberately-wrong reference must make the comparator return false;
//     the test FAILS if the comparator erroneously returns true.
//
// Channel-B trigger (§4.2): send `MccTools_ping` with missing required field
//   `seqno`. The generated `in_JsonCodec.decode` tries to access the missing
//   field, causing a cast or null-check failure caught as BaboonWiringException.

import 'dart:convert';
import 'package:collection/collection.dart';
import 'package:baboon_runtime/baboon_runtime.dart';
import 'package:baboon_runtime/baboon_mcp_runtime.dart';
import 'package:test/test.dart';

import '../../lib/mcp/stub/mcp_tools.dart';
import '../../lib/mcp/stub/mcp_tools_mcp_server.dart';
import '../../lib/mcp/stub/mcptools/listcollections/in.dart' as listcollections_in;
import '../../lib/mcp/stub/mcptools/listcollections/out.dart' as listcollections_out;
import '../../lib/mcp/stub/mcptools/submitcomposite/in.dart' as submitcomposite_in;
import '../../lib/mcp/stub/mcptools/submitcomposite/out.dart' as submitcomposite_out;
import '../../lib/mcp/stub/mcptools/processshape/in.dart' as processshape_in;
import '../../lib/mcp/stub/mcptools/processshape/out.dart' as processshape_out;
import '../../lib/mcp/stub/mcptools/processtagged/in.dart' as processtagged_in;
import '../../lib/mcp/stub/mcptools/processtagged/out.dart' as processtagged_out;
import '../../lib/mcp/stub/mcptools/pagepoints/in.dart' as pagepoints_in;
import '../../lib/mcp/stub/mcptools/pagepoints/out.dart' as pagepoints_out;
import '../../lib/mcp/stub/mcptools/ping/in.dart' as ping_in;
import '../../lib/mcp/stub/mcptools/ping/out.dart' as ping_out;

// ---------------------------------------------------------------------------
// T7 §2.3 reference inputSchema values (authoritative: McpInputSchemaEmitter
// golden test McpInputSchemaEmissionTest.scala + mcp-roundtrip-scenario.md).
// Uses Dart raw-string prefix r'' to avoid interpolation of $ signs.
// ---------------------------------------------------------------------------

// Tool 1: McpTools_listCollections — list/set/map[str]/map[enum-key]
final _refListCollections = jsonDecode(r'{"$schema":"https://json-schema.org/draft/2020-12/schema","type":"object","properties":{"tags":{"type":"array","items":{"type":"string"}},"uniqueIds":{"type":"array","items":{"type":"integer","format":"int64"},"uniqueItems":true},"labels":{"type":"object","additionalProperties":{"type":"string"}},"byColor":{"type":"object","additionalProperties":{"type":"string"},"propertyNames":{"type":"string","enum":["Red","Green","Blue"]}}},"required":["tags","uniqueIds","labels","byColor"],"$defs":{"mcp_stub_Color":{"type":"string","enum":["Red","Green","Blue"]}}}') as Map<String, dynamic>;

// Tool 2: McpTools_submitComposite — nested DTO + opt[DTO] + enum + foreign-string
final _refSubmitComposite = jsonDecode(r'{"$schema":"https://json-schema.org/draft/2020-12/schema","type":"object","properties":{"nested":{"$ref":"#/$defs/mcp_stub_Nested"},"maybePoint":{"oneOf":[{"$ref":"#/$defs/mcp_stub_Point"},{"type":"null"}]},"color":{"$ref":"#/$defs/mcp_stub_Color"},"fancy":{"type":"string"}},"required":["nested","color","fancy"],"$defs":{"mcp_stub_Color":{"type":"string","enum":["Red","Green","Blue"]},"mcp_stub_Nested":{"type":"object","properties":{"point":{"$ref":"#/$defs/mcp_stub_Point"},"color":{"$ref":"#/$defs/mcp_stub_Color"},"label":{"oneOf":[{"type":"string"},{"type":"null"}]}},"required":["point","color"]},"mcp_stub_Point":{"type":"object","properties":{"x":{"type":"integer","format":"int32"},"y":{"type":"integer","format":"int32"}},"required":["x","y"]}}}') as Map<String, dynamic>;

// Tool 3: McpTools_processShape — ADT oneOf + recursive Tree
final _refProcessShape = jsonDecode(r'{"$schema":"https://json-schema.org/draft/2020-12/schema","type":"object","properties":{"shape":{"$ref":"#/$defs/mcp_stub_Shape"},"tree":{"$ref":"#/$defs/mcp_stub_Tree"}},"required":["shape","tree"],"$defs":{"mcp_stub_Shape":{"oneOf":[{"$ref":"#/$defs/mcp_stub_Shape_Circle"},{"$ref":"#/$defs/mcp_stub_Shape_Rect"}]},"mcp_stub_Tree":{"type":"object","properties":{"value":{"type":"integer","format":"int32"},"left":{"oneOf":[{"$ref":"#/$defs/mcp_stub_Tree"},{"type":"null"}]},"children":{"type":"array","items":{"$ref":"#/$defs/mcp_stub_Tree"}}},"required":["value","children"]},"mcp_stub_Shape_Circle":{"type":"object","properties":{"radius":{"type":"number","format":"double"}},"required":["radius"]},"mcp_stub_Shape_Rect":{"type":"object","properties":{"w":{"type":"number","format":"double"},"h":{"type":"number","format":"double"}},"required":["w","h"]}}}') as Map<String, dynamic>;

// Tool 4: McpTools_processTagged — contract-bearing ADT (T26/D11). `Tagged` is
// `is HasId`; the HasId contract carries `id: str`, merged into every branch DTO
// at typing time. Each branch $defs entry has `id` + own field, both required,
// NO allOf. Branch order in oneOf is declaration order: TagA then TagB.
final _refProcessTagged = jsonDecode(r'{"$schema":"https://json-schema.org/draft/2020-12/schema","type":"object","properties":{"tagged":{"$ref":"#/$defs/mcp_stub_Tagged"}},"required":["tagged"],"$defs":{"mcp_stub_Tagged":{"oneOf":[{"$ref":"#/$defs/mcp_stub_Tagged_TagA"},{"$ref":"#/$defs/mcp_stub_Tagged_TagB"}]},"mcp_stub_Tagged_TagA":{"type":"object","properties":{"id":{"type":"string"},"tag":{"type":"string"}},"required":["id","tag"]},"mcp_stub_Tagged_TagB":{"type":"object","properties":{"id":{"type":"string"},"weight":{"type":"integer","format":"int32"}},"required":["id","weight"]}}}') as Map<String, dynamic>;

// Tool 5: McpTools_pagePoints — template-instantiation alias PointPage = Page[Point]
final _refPagePoints = jsonDecode(r'{"$schema":"https://json-schema.org/draft/2020-12/schema","type":"object","properties":{"page":{"$ref":"#/$defs/mcp_stub_PointPage"}},"required":["page"],"$defs":{"mcp_stub_Point":{"type":"object","properties":{"x":{"type":"integer","format":"int32"},"y":{"type":"integer","format":"int32"}},"required":["x","y"]},"mcp_stub_PointPage":{"type":"object","properties":{"items":{"type":"array","items":{"$ref":"#/$defs/mcp_stub_Point"}},"total":{"type":"integer","format":"int32","minimum":0}},"required":["items","total"]}}}') as Map<String, dynamic>;

// Tool 6: McpTools_ping — scalar-only, no $defs
final _refPing = jsonDecode(r'{"$schema":"https://json-schema.org/draft/2020-12/schema","type":"object","properties":{"seqno":{"type":"integer","format":"int32"},"label":{"type":"string"}},"required":["seqno","label"]}') as Map<String, dynamic>;

// ---------------------------------------------------------------------------
// Structural equality helper (T7 §5.4):
//   - Map: compare by key lookup (key-order-insensitive).
//   - List for key "required": compare as a set (order-insensitive).
//   - All other Lists: compare element-by-element (ordered).
//   - Primitive values: standard equality.
// ---------------------------------------------------------------------------
bool _schemasStructurallyEqual(dynamic actual, dynamic expected) =>
    _schemasEqualImpl(actual, expected, inRequiredKey: false);

bool _schemasEqualImpl(dynamic actual, dynamic expected, {required bool inRequiredKey}) {
  if (inRequiredKey) {
    // Both must be Lists; compare as sets of strings.
    if (actual is! List || expected is! List) return false;
    final aSet = actual.map((e) => e.toString()).toSet();
    final eSet = expected.map((e) => e.toString()).toSet();
    return const SetEquality<String>().equals(aSet, eSet);
  }
  if (actual is Map && expected is Map) {
    if (actual.length != expected.length) return false;
    for (final key in expected.keys) {
      if (!actual.containsKey(key)) return false;
      final actVal = actual[key];
      final expVal = expected[key];
      if (!_schemasEqualImpl(actVal, expVal, inRequiredKey: key == 'required')) {
        return false;
      }
    }
    return true;
  }
  if (actual is List && expected is List) {
    if (actual.length != expected.length) return false;
    for (int i = 0; i < actual.length; i++) {
      if (!_schemasEqualImpl(actual[i], expected[i], inRequiredKey: false)) {
        return false;
      }
    }
    return true;
  }
  return actual == expected;
}

// ---------------------------------------------------------------------------
// Stub MccTools service: every method returns ok=true (T7 §3 convention).
// ---------------------------------------------------------------------------
class _StubMcpTools implements McpTools {
  @override
  listcollections_out.out listCollections(listcollections_in.in_ arg) =>
      listcollections_out.out(ok: true);
  @override
  submitcomposite_out.out submitComposite(submitcomposite_in.in_ arg) =>
      submitcomposite_out.out(ok: true);
  @override
  processshape_out.out processShape(processshape_in.in_ arg) =>
      processshape_out.out(ok: true);
  @override
  processtagged_out.out processTagged(processtagged_in.in_ arg) =>
      processtagged_out.out(ok: true);
  @override
  pagepoints_out.out pagePoints(pagepoints_in.in_ arg) =>
      pagepoints_out.out(ok: true);
  @override
  ping_out.out ping(ping_in.in_ arg) => ping_out.out(ok: true);
}

// ---------------------------------------------------------------------------
// Server factory: uses McpTools as the Ctx type so McpToolsWiring.invokeJson
// can be used directly as the delegate.
// ---------------------------------------------------------------------------
McpToolsMcpServer<McpTools> _makeServer(McpTools impl) {
  return McpToolsMcpServer<McpTools>(
    (method, data, stub, codecCtx) =>
        McpToolsWiring.invokeJson(method, data, stub, codecCtx),
  );
}

void _initSession(
    McpToolsMcpServer<McpTools> server, McpSession session, McpTools impl) {
  server.handle(
    JsonRpcRequest(1, 'initialize', jsonDecode(
        '{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"test-client","version":"0.0.1"}}'
    )),
    session, impl, BaboonCodecContext.defaultCtx,
  );
  server.handle(
    JsonRpcRequest(null, 'notifications/initialized', null),
    session, impl, BaboonCodecContext.defaultCtx,
  );
}

JsonRpcResponse _send(McpToolsMcpServer<McpTools> server, McpSession session,
    McpTools impl, JsonRpcRequest req) {
  final resp =
      server.handle(req, session, impl, BaboonCodecContext.defaultCtx);
  if (resp == null) fail('Expected a response for "${req.method}" but got null');
  return resp;
}

void main() {
  final stub = _StubMcpTools();

  // ---------------------------------------------------------------------------
  // §1 — initialize
  // ---------------------------------------------------------------------------
  group('sec1_initialize', () {
    test('responseIsCorrect', () {
      final server = _makeServer(stub);
      final session = McpSession();
      final resp = _send(
          server, session, stub,
          JsonRpcRequest(
            1, 'initialize',
            jsonDecode('{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"test-client","version":"0.0.1"}}'),
          ));
      expect(resp.error, isNull, reason: 'Expected no error for initialize');
      final result = resp.result as Map<String, dynamic>;
      expect(result['protocolVersion'], equals('2025-06-18'),
          reason: 'protocolVersion must be 2025-06-18');
      final caps = result['capabilities'] as Map<String, dynamic>;
      expect(caps.length, equals(1), reason: 'capabilities must have exactly one key');
      expect(caps.containsKey('tools'), isTrue,
          reason: 'capabilities.tools must be present');
      expect((caps['tools'] as Map).isEmpty, isTrue,
          reason: 'capabilities.tools must be {}');
      final info = result['serverInfo'] as Map<String, dynamic>;
      expect((info['name'] as String).isNotEmpty, isTrue,
          reason: 'serverInfo.name must be non-empty');
      expect((info['version'] as String).isNotEmpty, isTrue,
          reason: 'serverInfo.version must be non-empty');
    });

    test('initializedNotification_producesNoResponse', () {
      final server = _makeServer(stub);
      final session = McpSession();
      server.handle(
        JsonRpcRequest(0, 'initialize',
            jsonDecode('{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"t","version":"0"}}')),
        session, stub, BaboonCodecContext.defaultCtx,
      );
      final notifResp = server.handle(
        JsonRpcRequest(null, 'notifications/initialized', null),
        session, stub, BaboonCodecContext.defaultCtx,
      );
      expect(notifResp, isNull,
          reason: 'notifications/initialized MUST produce no response');
    });
  });

  // ---------------------------------------------------------------------------
  // §2 — tools/list + inputSchema validation (K1 tier)
  // ---------------------------------------------------------------------------
  group('sec2_toolsList', () {
    late McpToolsMcpServer<McpTools> server;
    late McpSession session;
    late List<Map<String, dynamic>> tools;
    late Map<String, dynamic> toolsResult;

    setUp(() {
      server = _makeServer(stub);
      session = McpSession();
      _initSession(server, session, stub);
      final resp = _send(server, session, stub,
          JsonRpcRequest(2, 'tools/list', null));
      toolsResult = resp.result as Map<String, dynamic>;
      tools = (toolsResult['tools'] as List)
          .map((e) => e as Map<String, dynamic>)
          .toList();
    });

    test('exactlySixToolsInDeclarationOrder', () {
      expect(tools.length, equals(6), reason: 'MUST be exactly 6 tools');
      // Exact position assertions (model declaration order, T7 §0).
      // processTagged is declared between processShape and pagePoints (T26/D11),
      // so it occupies index 3 and shifts pagePoints→4, ping→5.
      // DELIBERATE-NEGATIVE-CONTROL: replacing "McpTools_ping" with
      // "McpTools_WRONG" on the last expect line makes this test fail,
      // proving position[5] check is live.
      expect(tools[0]['name'], equals('McpTools_listCollections'));
      expect(tools[1]['name'], equals('McpTools_submitComposite'));
      expect(tools[2]['name'], equals('McpTools_processShape'));
      expect(tools[3]['name'], equals('McpTools_processTagged'));
      expect(tools[4]['name'], equals('McpTools_pagePoints'));
      expect(tools[5]['name'], equals('McpTools_ping'));
      // No "nextCursor" key (§2.2)
      expect(toolsResult.containsKey('nextCursor'), isFalse,
          reason: 'nextCursor must not be present');
      // T119: McpTools_ping carries a distinctive doc comment in
      // mcp_stub.baboon; its tools/list entry must expose that text as
      // 'description'. Every other (undocumented) tool must have no
      // description key.
      const documentedToolName = 'McpTools_ping';
      const documentedToolDescription =
          'Liveness probe returning a fixed acknowledgement token.';
      for (final t in tools) {
        if (t['name'] == documentedToolName) {
          expect(t['description'], equals(documentedToolDescription),
              reason: 'Tool ${t["name"]} must carry its doc-comment description');
        } else {
          expect(t.containsKey('description'), isFalse,
              reason: 'Tool ${t["name"]} must have no description');
        }
      }
    });

    test('eachInputSchema_hasDraft202012SchemaUri', () {
      for (final t in tools) {
        final schema = t['inputSchema'] as Map<String, dynamic>;
        expect(schema[r'$schema'],
            equals('https://json-schema.org/draft/2020-12/schema'),
            reason: 'Tool ${t["name"]}: \$schema must be the Draft 2020-12 URI');
      }
    });

    test('k1_allInputSchemas_areWellFormedJson_viaDartConvert', () {
      // K1 part (a) — well-formedness gate: each inputSchema must survive a
      // full dart:convert encode/decode cycle without throwing.
      for (final t in tools) {
        final schema = t['inputSchema'];
        expect(schema, isA<Map<String, dynamic>>(),
            reason: 'Tool ${t["name"]}: inputSchema must be a Map');
        // Serialise and re-parse to prove round-trip fidelity.
        final serialized = jsonEncode(schema);
        final reparsed = jsonDecode(serialized);
        expect(reparsed, isNotNull,
            reason: 'Tool ${t["name"]}: schema must not be null after re-encode/decode');
      }
    });

    test('k1_allSixTools_structuralEqualityToT7Reference', () {
      // K1 part (b) — structural equality to T7 §2.3 reference.
      // Each inputSchema is re-encoded and re-decoded via dart:convert
      // (codec-divergence coverage) and compared key-by-key recursively.
      // `required` arrays compared as SETS per §5.4.
      final refs = [
        _refListCollections, // tools[0] = McpTools_listCollections
        _refSubmitComposite, // tools[1] = McpTools_submitComposite
        _refProcessShape,    // tools[2] = McpTools_processShape
        _refProcessTagged,   // tools[3] = McpTools_processTagged
        _refPagePoints,      // tools[4] = McpTools_pagePoints
        _refPing,            // tools[5] = McpTools_ping
      ];
      for (int i = 0; i < 6; i++) {
        final toolName = tools[i]['name'] as String;
        // Re-encode and re-decode to exercise full dart:convert round-trip.
        final actualJson = jsonEncode(tools[i]['inputSchema']);
        final actual = jsonDecode(actualJson) as Map<String, dynamic>;
        final eq = _schemasStructurallyEqual(actual, refs[i]);
        if (!eq) {
          fail('Tool $toolName (index $i): inputSchema is not structurally equal '
              'to T7 §2.3 reference.\n'
              '  actual:   $actualJson\n'
              '  expected: ${jsonEncode(refs[i])}');
        }
      }
    });

    test('k1_negativeControl_wrongReferenceDetectedByComparator', () {
      // NEGATIVE CONTROL (T7 §5.2 / schema structure):
      // Asserts that the structural comparator DETECTS a wrong reference.
      // If _schemasStructurallyEqual erroneously returns true for an incorrect
      // schema, this test FAILS — proving the comparator is live.
      //
      // Wrong reference: ping schema with an extra top-level field "extra":"bad".
      final wrongRef = jsonDecode(
          r'{"$schema":"https://json-schema.org/draft/2020-12/schema",'
          '"type":"object",'
          '"properties":{"seqno":{"type":"integer","format":"int32"},"label":{"type":"string"}},'
          '"required":["seqno","label"],'
          '"extra":"bad"}') as Map<String, dynamic>;

      final actualPingJson = jsonEncode(tools[5]['inputSchema']);
      final actualPing = jsonDecode(actualPingJson) as Map<String, dynamic>;

      // The comparator MUST return false for the wrong reference.
      final falseResult = _schemasStructurallyEqual(actualPing, wrongRef);
      if (falseResult) {
        fail('NEGATIVE CONTROL FAILED: _schemasStructurallyEqual returned true '
            'for a deliberately-wrong reference (extra field "extra"). '
            'The comparator is not functioning correctly.');
      }
      // And MUST return true for the correct reference.
      final trueResult = _schemasStructurallyEqual(actualPing, _refPing);
      if (!trueResult) {
        fail('Positive case failed after negative control: '
            'ping schema must equal _refPing.');
      }
    });
  });

  // ---------------------------------------------------------------------------
  // §3 — tools/call (success paths)
  // ---------------------------------------------------------------------------
  group('sec3_toolsCall', () {
    test('ping_returnsOkTrue', () {
      final server = _makeServer(stub);
      final session = McpSession();
      _initSession(server, session, stub);

      final resp = _send(server, session, stub, JsonRpcRequest(
        3, 'tools/call',
        jsonDecode(
            '{"name":"McpTools_ping","arguments":{"seqno":42,"label":"hello"}}'),
      ));

      expect(resp.error, isNull, reason: 'Unexpected error on ping call');
      final result = resp.result as Map<String, dynamic>;
      final content = result['content'] as List;
      expect(content.length, equals(1),
          reason: 'content must have exactly one element');
      expect(content[0]['type'], equals('text'));
      final payload =
          jsonDecode(content[0]['text'] as String) as Map<String, dynamic>;
      expect(payload['ok'], equals(true), reason: 'ok must be true');
      // isError MUST be false or absent (K4 §2.4)
      final isError = result['isError'];
      expect(isError == null || isError == false, isTrue,
          reason: 'isError must be false or absent');
    });

    test('processTagged_returnsOkTrue', () {
      // T26/D11: processTagged dispatch with a Tagged TagA value.
      // ADT wire format under --dt-wrapped-adt-branch-codecs=false is the
      // branch-discriminated object {"TagA":{...}} (the codec wraps the branch;
      // the inputSchema oneOf is a separate structural view). Tagged carries no
      // foreign type, so no FFancyStr codec registration is needed.
      final server = _makeServer(stub);
      final session = McpSession();
      _initSession(server, session, stub);

      final resp = _send(server, session, stub, JsonRpcRequest(
        7, 'tools/call',
        jsonDecode('{"name":"McpTools_processTagged","arguments":{"tagged":{"TagA":{"id":"abc","tag":"hello"}}}}'),
      ));

      expect(resp.error, isNull, reason: 'Unexpected error on processTagged call');
      final result = resp.result as Map<String, dynamic>;
      final content = result['content'] as List;
      expect(content.length, equals(1));
      expect(content[0]['type'], equals('text'));
      final payload =
          jsonDecode(content[0]['text'] as String) as Map<String, dynamic>;
      expect(payload['ok'], equals(true), reason: 'ok must be true');
      final isError = result['isError'];
      expect(isError == null || isError == false, isTrue,
          reason: 'isError must be false or absent');
    });

    test('listCollections_returnsOkTrue', () {
      final server = _makeServer(stub);
      final session = McpSession();
      _initSession(server, session, stub);

      // D6/T30: byColor is map[Color,str]. In the Dart JSON codec, map[enum-key]
      // is a JSON object with enum-name keys (e.g. {"Green":"ok"}). Send a NON-EMPTY
      // object conforming to the inputSchema (exercises the enum key-codec path).
      final resp = _send(server, session, stub, JsonRpcRequest(
        4, 'tools/call',
        jsonDecode('{"name":"McpTools_listCollections","arguments":{"tags":["a","b"],"uniqueIds":[1,2],"labels":{"k":"v"},"byColor":{"Green":"ok","Red":"stop"}}}'),
      ));

      expect(resp.error, isNull, reason: 'Unexpected error on listCollections call');
      final result = resp.result as Map<String, dynamic>;
      final content = result['content'] as List;
      expect(content.length, equals(1));
      expect(content[0]['type'], equals('text'));
      final payload =
          jsonDecode(content[0]['text'] as String) as Map<String, dynamic>;
      expect(payload['ok'], equals(true), reason: 'ok must be true');
      final isError = result['isError'];
      expect(isError == null || isError == false, isTrue,
          reason: 'isError must be false or absent');
    });
  });

  // ---------------------------------------------------------------------------
  // §4 — tools/call (error paths) — primary negative controls
  // ---------------------------------------------------------------------------
  group('sec4_toolsCallErrors', () {
    test('unknownTool_channelAError_code32602', () {
      // NEGATIVE CONTROL (T7 §5.2): if the server returned a success result for
      // McpTools_nonexistent, expect(resp.error, isNotNull) would fail.
      final server = _makeServer(stub);
      final session = McpSession();
      _initSession(server, session, stub);

      final resp = _send(server, session, stub, JsonRpcRequest(
        5, 'tools/call',
        jsonDecode('{"name":"McpTools_nonexistent","arguments":{}}'),
      ));

      // MUST be a Channel-A error, not a result.
      expect(resp.error, isNotNull,
          reason: 'Unknown tool must produce a Channel-A error');
      expect(resp.result, isNull,
          reason: 'No result expected for unknown tool');
      // §4.1: code MUST be -32602 (InvalidParams — unknown tool)
      expect(resp.error!.code, equals(-32602),
          reason: 'Unknown tool error code MUST be -32602');
      expect(resp.error!.message.isNotEmpty, isTrue,
          reason: 'error.message must be non-empty');
    });

    test('decodeFailure_channelB_isErrorTrue', () {
      // NEGATIVE CONTROL: if isError were false this test would fail.
      //
      // Channel-B trigger: send McpTools_ping with missing required "seqno".
      // The generated in_JsonCodec.decode tries to access the missing field,
      // which causes an error caught as BaboonWiringException by the MCP runtime.
      final server = _makeServer(stub);
      final session = McpSession();
      _initSession(server, session, stub);

      final resp = _send(server, session, stub, JsonRpcRequest(
        6, 'tools/call',
        jsonDecode('{"name":"McpTools_ping","arguments":{"label":"missing-seqno"}}'),
      ));

      // Channel B: MUST be a result (not error) with isError=true.
      expect(resp.result, isNotNull, reason: 'Channel-B: result must be present');
      expect(resp.error, isNull,
          reason: 'Channel-B: must not be a JSON-RPC error');
      final result = resp.result as Map<String, dynamic>;
      expect(result['isError'], equals(true),
          reason: 'isError MUST be true for Channel-B decode failure');
      final content = result['content'] as List;
      expect(content.isNotEmpty, isTrue,
          reason: 'content must have at least one element');
      expect(content[0]['type'], equals('text'));
      expect((content[0]['text'] as String).isNotEmpty, isTrue,
          reason: 'content[0].text must be non-empty');
    });
  });
}
