// T178 / D40 — Dart zero-service MCP overlay test (RED baseline).
//
// Purpose: prove the ZERO-SERVICE MCP runtime contract. The zero-service model
// (mcp-stub-zero-services-ok) declares @root types but NO RPC block, so
// servicesOf(domain) is empty and NO per-service `<Service>McpServer` class is
// generated. With zero services the ONLY source of the MCP runtime types
// (AbstractMcpMuxer / IBaboonRoutableMcpServer / McpServerInfo / McpSession /
// JsonRpcRequest / JsonRpcResponse / JsonRpcError) is the STATIC runtime file
// `baboon_mcp_runtime.dart`.
//
// RED (pre-fix): the current generator emits NO `baboon_mcp_runtime.dart` for a
// zero-service model, so the import below resolves to a non-existent URI and
// THIS FILE FAILS `dart analyze` / `dart test` (Target of URI doesn't exist /
// undefined class AbstractMcpMuxer). That failure IS the D40 reproduction.
//
// GREEN (post-fix, later task): once the generator emits the static MCP runtime
// even when servicesOf(domain) is empty, this file analyzes/compiles and the
// runtime assertions below pass — an empty muxer lists zero tools and rejects
// any tools/call with JSON-RPC -32602.
//
// Assertion discipline: explicit `throw` on failure — unconditional (Dart
// `assert` is stripped outside debug/VM assertions-enabled mode).

import 'package:baboon_runtime/baboon_runtime.dart';
import 'package:baboon_runtime/baboon_mcp_runtime.dart';
import 'package:test/test.dart';

void _require(bool cond, String message) {
  if (!cond) {
    throw StateError(message);
  }
}

// COMPILE-TIME contract: constructing AbstractMcpMuxer<Ctx> with ZERO registered
// servers requires the static runtime file to exist. With zero services there is
// no generated <Service>McpServer to import — these types resolve ONLY from
// baboon_mcp_runtime.dart.
AbstractMcpMuxer<Object?> _makeEmptyMuxer() =>
    AbstractMcpMuxer<Object?>(const McpServerInfo('ZeroEndpoint', '1.0.0'));

McpSession _initedSession(AbstractMcpMuxer<Object?> mux) {
  final session = McpSession();
  mux.handle(
    JsonRpcRequest(0, 'initialize', {
      'protocolVersion': '2025-06-18',
      'capabilities': <String, dynamic>{},
      'clientInfo': {'name': 't', 'version': '0'},
    }),
    session,
    null,
    BaboonCodecContext.defaultCtx,
  );
  mux.handle(
    JsonRpcRequest(null, 'notifications/initialized', null),
    session,
    null,
    BaboonCodecContext.defaultCtx,
  );
  return session;
}

void main() {
  // RUNTIME contract (a): tools/list on an empty muxer returns an empty array.
  test('zero services: tools/list is empty', () {
    final mux = _makeEmptyMuxer();
    final session = _initedSession(mux);

    final resp = mux.handle(
      JsonRpcRequest(1, 'tools/list', null),
      session,
      null,
      BaboonCodecContext.defaultCtx,
    );

    _require(resp != null, 'tools/list must return a response');
    _require(resp!.error == null, 'tools/list must not return an error');

    final result = resp.result as Map;
    final tools = result['tools'] as List;
    _require(tools.isEmpty, 'empty muxer MUST list zero tools');
  });

  // RUNTIME contract (b): an unknown tool call yields JSON-RPC -32602.
  test('zero services: unknown tool call yields -32602', () {
    final mux = _makeEmptyMuxer();
    final session = _initedSession(mux);

    final resp = mux.handle(
      JsonRpcRequest(2, 'tools/call', {
        'name': 'anything_at_all',
        'arguments': <String, dynamic>{},
      }),
      session,
      null,
      BaboonCodecContext.defaultCtx,
    );

    _require(resp != null, 'tools/call must return a response');
    _require(resp!.error != null,
        'unknown tool on empty muxer MUST produce a Channel-A error');
    _require(resp.result == null, 'no result expected for unknown tool');
    _require(resp.error!.code == -32602,
        'unknown-tool error code MUST be -32602 (InvalidParams)');
    _require(resp.error!.message.isNotEmpty, 'error.message must be non-empty');
  });
}
