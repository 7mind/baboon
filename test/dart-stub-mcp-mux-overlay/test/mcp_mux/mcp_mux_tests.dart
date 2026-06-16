// T112 — Dart MCP muxer round-trip overlay test.
//
// Exercises `AbstractMcpMuxer<Ctx>` by composing two FRESHLY GENERATED
// `<Service>McpServer` instances produced from the T102 `mcp-mux-stub-ok/`
// model (UserService + OrderService). Composition is done strictly through
// the public T114 routable surface:
//   - the muxer ctor takes `IBaboonRoutableMcpServer<Ctx>` members,
//   - the test NEVER subclasses a `<Service>McpServer`,
//   - the test NEVER calls a member server's own `handle()`.
//
// Generated code lands in `lib/mcp/mux/stub/` (the isolated dir set by the
// `test-gen-dt-mcp-mux` mdl action). No committed generated fixtures.
//
// Four asserted muxer behaviours (T112 acceptance):
//   1. tools/list  -> UNION of both services' tools in registration-then-
//                    declaration order;
//   2. tools/call  -> routes the flat tool name to the correct owning
//                    service — proven for a tool of EACH service:
//                    UserService_getUser (Channel-A Right, isError=false),
//                    OrderService_cancelOrder (Channel-A Right, isError=false);
//   3. register a server with a colliding tool name -> throws
//      BaboonMcpWiringException(DuplicateTool);
//   4. unknown tool name -> -32602 "unknown tool" response (NoMatchingTool).
//
// Assertion discipline (T7 §5.1):
//   All assertions use test-package `expect`/`fail` which throw
//   unconditionally on failure. Dart `assert(...)` is elided outside debug
//   mode — this test uses ONLY `expect(...)`/`if (!cond) fail(...)`.
//
// Dart MCP is SYNC ONLY — no async muxer lane (R112 criticism 3).

import 'dart:convert';
import 'package:baboon_runtime/baboon_runtime.dart';
import 'package:baboon_runtime/baboon_mcp_runtime.dart';
import 'package:test/test.dart';

import '../../lib/mcp/mux/stub/user_service.dart';
import '../../lib/mcp/mux/stub/order_service.dart';
import '../../lib/mcp/mux/stub/user_service_mcp_server.dart';
import '../../lib/mcp/mux/stub/order_service_mcp_server.dart';
import '../../lib/mcp/mux/stub/user_profile.dart';
import '../../lib/mcp/mux/stub/user_status.dart';
import '../../lib/mcp/mux/stub/order_status.dart';
import '../../lib/mcp/mux/stub/order_summary.dart';
import '../../lib/mcp/mux/stub/userservice/createuser/out.dart' as createuser_out;
import '../../lib/mcp/mux/stub/userservice/getuser/out.dart' as getuser_out;
import '../../lib/mcp/mux/stub/userservice/createuser/in.dart' as createuser_in;
import '../../lib/mcp/mux/stub/userservice/getuser/in.dart' as getuser_in;
import '../../lib/mcp/mux/stub/orderservice/placeorder/out.dart' as placeorder_out;
import '../../lib/mcp/mux/stub/orderservice/cancelorder/out.dart' as cancelorder_out;
import '../../lib/mcp/mux/stub/orderservice/placeorder/in.dart' as placeorder_in;
import '../../lib/mcp/mux/stub/orderservice/cancelorder/in.dart' as cancelorder_in;

// ---------------------------------------------------------------------------
// The union of both services' tools in registration-then-declaration order:
// UserService registered first (createUser, getUser declared in that order),
// OrderService second (placeOrder, cancelOrder).
// ---------------------------------------------------------------------------
const List<String> _expectedUnion = [
  'UserService_createUser',
  'UserService_getUser',
  'OrderService_placeOrder',
  'OrderService_cancelOrder',
];

const _mergedInfo = McpServerInfo('MergedEndpoint', '1.0.0');

// ---------------------------------------------------------------------------
// Stub implementations: every method returns minimal valid data.
// ---------------------------------------------------------------------------
class _StubUserService implements UserService {
  @override
  createuser_out.out createUser(createuser_in.in_ arg) =>
      createuser_out.out(
        profile: UserProfile(
          userId: 'u1',
          email: 'a@b.c',
          status: UserStatus.Active,
        ),
      );

  @override
  getuser_out.out getUser(getuser_in.in_ arg) =>
      getuser_out.out(
        profile: UserProfile(
          userId: 'u1',
          email: 'a@b.c',
          status: UserStatus.Active,
        ),
      );
}

class _StubOrderService implements OrderService {
  @override
  placeorder_out.out placeOrder(placeorder_in.in_ arg) =>
      placeorder_out.out(
        summary: OrderSummary(
          orderId: 'o1',
          status: OrderStatus.Confirmed,
          total: 10.0,
        ),
      );

  @override
  cancelorder_out.out cancelOrder(cancelorder_in.in_ arg) =>
      cancelorder_out.out(ok: true);
}

// ---------------------------------------------------------------------------
// Delegate factories: the SAME errors-mode invokeJson the integrator would bind.
// dt uses no-errors mode (service-result-no-errors=true) — invokeJson returns
// String directly; no BaboonServiceRt threading needed.
// ---------------------------------------------------------------------------
final _stubUser = _StubUserService();
final _stubOrder = _StubOrderService();

String _userDelegate(BaboonMethodId method, String data, dynamic ctx, BaboonCodecContext codecCtx) =>
    UserServiceWiring.invokeJson(method, data, _stubUser, codecCtx);

String _orderDelegate(BaboonMethodId method, String data, dynamic ctx, BaboonCodecContext codecCtx) =>
    OrderServiceWiring.invokeJson(method, data, _stubOrder, codecCtx);

// ---------------------------------------------------------------------------
// Server factories: bound through the PUBLIC routable interface only.
// ---------------------------------------------------------------------------
IBaboonRoutableMcpServer<dynamic> _makeUserServer() =>
    UserServiceMcpServer<dynamic>(_userDelegate);

IBaboonRoutableMcpServer<dynamic> _makeOrderServer() =>
    OrderServiceMcpServer<dynamic>(_orderDelegate);

AbstractMcpMuxer<dynamic> _makeMuxer() =>
    AbstractMcpMuxer<dynamic>(_mergedInfo, [_makeUserServer(), _makeOrderServer()]);

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------
JsonRpcResponse _send(AbstractMcpMuxer<dynamic> mux, McpSession session, JsonRpcRequest req) {
  final resp = mux.handle(req, session, null, BaboonCodecContext.defaultCtx);
  if (resp == null) fail('Expected a response for "${req.method}" but got null');
  return resp;
}

McpSession _initedSession(AbstractMcpMuxer<dynamic> mux) {
  final session = McpSession();
  mux.handle(
    JsonRpcRequest(1, 'initialize',
        jsonDecode('{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"test-client","version":"0.0.1"}}')),
    session, null, BaboonCodecContext.defaultCtx,
  );
  mux.handle(
    JsonRpcRequest(null, 'notifications/initialized', null),
    session, null, BaboonCodecContext.defaultCtx,
  );
  return session;
}

JsonRpcResponse _callTool(AbstractMcpMuxer<dynamic> mux, McpSession session, String toolName, Map<String, dynamic> args) {
  return _send(mux, session, JsonRpcRequest(
    99, 'tools/call',
    {'name': toolName, 'arguments': args},
  ));
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------
void main() {
  // ---------------------------------------------------------------------------
  // §1 — tools/list returns UNION in registration-then-declaration order
  // ---------------------------------------------------------------------------
  group('sec1_toolsList', () {
    test('unionInRegistrationThenDeclarationOrder', () {
      final mux = _makeMuxer();
      final session = _initedSession(mux);

      final resp = _send(mux, session, JsonRpcRequest(2, 'tools/list', null));
      expect(resp.error, isNull, reason: 'Unexpected error on tools/list');
      final result = resp.result as Map<String, dynamic>;
      final names = (result['tools'] as List)
          .map((e) => (e as Map<String, dynamic>)['name'] as String)
          .toList();

      expect(names, equals(_expectedUnion),
          reason: 'tools/list MUST return union in registration-then-declaration order');
    });

    test('negativeControl_notInterleavedOrder', () {
      // Proves the ordering assertion is live: a wrong interleaved order must NOT match.
      final mux = _makeMuxer();
      final session = _initedSession(mux);

      final resp = _send(mux, session, JsonRpcRequest(2, 'tools/list', null));
      final result = resp.result as Map<String, dynamic>;
      final names = (result['tools'] as List)
          .map((e) => (e as Map<String, dynamic>)['name'] as String)
          .toList();

      // DELIBERATE-NEGATIVE-CONTROL: interleaved order must NOT match.
      final interleavedOrder = [
        'UserService_createUser',
        'OrderService_placeOrder',
        'UserService_getUser',
        'OrderService_cancelOrder',
      ];
      if (names.toString() == interleavedOrder.toString()) {
        fail('NEGATIVE CONTROL FAILED: tools were in interleaved order, ordering assertion is not live');
      }
    });
  });

  // ---------------------------------------------------------------------------
  // §2 — tools/call routes to the correct owning service
  // ---------------------------------------------------------------------------
  group('sec2_toolsCallRouting', () {
    test('userService_getUser_channelARight_isErrorFalse', () {
      final mux = _makeMuxer();
      final session = _initedSession(mux);

      final resp = _callTool(mux, session, 'UserService_getUser', {'userId': 'u1'});
      expect(resp.error, isNull, reason: 'Unexpected error on UserService_getUser');
      final result = resp.result as Map<String, dynamic>;
      final isError = result['isError'];
      expect(isError == null || isError == false, isTrue,
          reason: 'isError must be false or absent for Channel-A');
      final content = result['content'] as List;
      expect(content.length, equals(1));
      expect(content[0]['type'], equals('text'));

      // The JSON payload proves UserService handled it (profile field present).
      final payload = jsonDecode(content[0]['text'] as String) as Map<String, dynamic>;
      expect(payload.containsKey('profile'), isTrue,
          reason: 'UserService result must contain a profile field');
    });

    test('orderService_cancelOrder_channelARight_isErrorFalse', () {
      final mux = _makeMuxer();
      final session = _initedSession(mux);

      final resp = _callTool(mux, session, 'OrderService_cancelOrder', {'orderId': 'o1', 'reason': null});
      expect(resp.error, isNull, reason: 'Unexpected error on OrderService_cancelOrder');
      final result = resp.result as Map<String, dynamic>;
      final isError = result['isError'];
      expect(isError == null || isError == false, isTrue,
          reason: 'isError must be false or absent for Channel-A');
      final content = result['content'] as List;
      expect(content.length, equals(1));
      final payload = jsonDecode(content[0]['text'] as String) as Map<String, dynamic>;
      // OrderService_cancelOrder returns {ok: true}
      expect(payload['ok'], equals(true), reason: 'ok must be true');
    });

    test('orderService_placeOrder_decoderFailure_channelB_isErrorTrue', () {
      // items:null causes a decoder failure in the Dart JSON codec
      // (lst[OrderItem] cannot be null). This is caught as a runtime error
      // -> Channel-B (isError=true).
      final mux = _makeMuxer();
      final session = _initedSession(mux);

      final resp = _callTool(mux, session, 'OrderService_placeOrder', {'userId': 'u1', 'items': null});
      // Channel B: MUST be a result (not error) with isError=true.
      expect(resp.result, isNotNull, reason: 'Channel-B: result must be present');
      expect(resp.error, isNull, reason: 'Channel-B: must not be a JSON-RPC error');
      final result = resp.result as Map<String, dynamic>;
      expect(result['isError'], equals(true),
          reason: 'isError MUST be true for Channel-B decode failure');
      final content = result['content'] as List;
      expect(content.isNotEmpty, isTrue);
      expect((content[0]['text'] as String).isNotEmpty, isTrue);
    });
  });

  // ---------------------------------------------------------------------------
  // §3 — DuplicateTool on collision
  // ---------------------------------------------------------------------------
  group('sec3_duplicateTool', () {
    test('ctorThrows_BaboonMcpWiringException_DuplicateTool_onCollision', () {
      // Second UserServiceMcpServer re-declares UserService_createUser / UserService_getUser.
      // UNCONDITIONAL THROW assertion: if exception is NOT thrown, fail() is called.
      BaboonMcpWiringException? thrown;
      try {
        AbstractMcpMuxer<dynamic>(_mergedInfo, [_makeUserServer(), _makeUserServer()]);
      } on BaboonMcpWiringException catch (e) {
        thrown = e;
      }

      if (thrown == null) {
        fail('BaboonMcpWiringException must be thrown for duplicate tool registration via ctor');
      }
      expect(thrown.error, isA<DuplicateTool>(),
          reason: 'error must be DuplicateTool');
      expect((thrown.error as DuplicateTool).toolName, equals('UserService_createUser'),
          reason: 'first duplicate tool name must be UserService_createUser');
    });

    test('register_throws_BaboonMcpWiringException_DuplicateTool_onCollision', () {
      // register() with a server that has overlapping tool names throws DuplicateTool.
      // UNCONDITIONAL THROW assertion: if exception is NOT thrown, fail() is called.
      final mux = AbstractMcpMuxer<dynamic>(_mergedInfo, [_makeUserServer()]);

      BaboonMcpWiringException? thrown;
      try {
        mux.register(_makeUserServer());
      } on BaboonMcpWiringException catch (e) {
        thrown = e;
      }

      if (thrown == null) {
        fail('BaboonMcpWiringException must be thrown when calling register() with duplicate tool');
      }
      expect(thrown.error, isA<DuplicateTool>(),
          reason: 'error must be DuplicateTool');
    });
  });

  // ---------------------------------------------------------------------------
  // §4 — NoMatchingTool on unknown tool name
  // ---------------------------------------------------------------------------
  group('sec4_noMatchingTool', () {
    test('unknownTool_returns_32602_channelAError', () {
      final mux = _makeMuxer();
      final session = _initedSession(mux);

      final resp = _callTool(mux, session, 'UserService_nope', {});

      // MUST be a Channel-A error (-32602), not a result.
      // UNCONDITIONAL assertion: if resp.error is null, expect() throws.
      expect(resp.error, isNotNull,
          reason: 'Unknown tool must produce a Channel-A error');
      expect(resp.result, isNull,
          reason: 'No result expected for unknown tool');
      expect(resp.error!.code, equals(-32602),
          reason: 'Unknown tool error code MUST be -32602');
      expect(resp.error!.message.isNotEmpty, isTrue,
          reason: 'error.message must be non-empty');
    });
  });
}
