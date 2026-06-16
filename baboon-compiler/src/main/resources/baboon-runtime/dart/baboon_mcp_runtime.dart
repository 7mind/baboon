import 'dart:convert';
import 'package:baboon_runtime/baboon_runtime.dart';

// Additive MCP server runtime (decisions ledger M1; contract:
// docs/research/mcp-dispatch-runtime-contract.md). Emitted only when
// `--dart-generate-mcp-server=true`; the service-wiring runtime in
// baboon_runtime.dart is unchanged. These types are STATIC (no per-model
// templating) — the only per-model code is the generated `<Service>McpServer`
// and its tool-registry literals.
//
// The MCP dispatch surface is transport-abstract: it implements the JSON-RPC
// method state machine + tool dispatch ONLY. Bytes-in/bytes-out (stdio framing,
// Streamable-HTTP bodies) is an injected adapter the generated surface never
// contains — mirroring the abstract-context service contract, which supplies
// `Ctx` per invocation rather than baking an I/O loop into the wrapper.

// --- JSON-RPC value types (already parsed from bytes by the adapter) ---
//
// A notification carries no `id`; the server produces no response for it.
// `params`/`result`/`error.data` use Dart dynamic (json-decoded objects) —
// the MCP runtime reuses what dart:convert already speaks rather than
// introducing a new model.

class JsonRpcRequest {
  final Object? id;
  final String method;
  final dynamic params;

  const JsonRpcRequest(this.id, this.method, this.params);
}

class JsonRpcError {
  final int code;
  final String message;
  final dynamic data;

  const JsonRpcError(this.code, this.message, [this.data]);
}

class JsonRpcResponse {
  final Object? id;
  final dynamic result;
  final JsonRpcError? error;

  const JsonRpcResponse(this.id, {this.result, this.error});
}

// JSON-RPC / MCP protocol constants (wire contract K4).
const String mcpProtocolVersion = '2025-06-18';

const int jsonRpcErrorParseError = -32700;
const int jsonRpcErrorInvalidRequest = -32600;
const int jsonRpcErrorMethodNotFound = -32601;
const int jsonRpcErrorInvalidParams = -32602;
const int jsonRpcErrorInternalError = -32603;

// --- Per-connection state (adapter-owned) ---
//
// The "initialized" precondition (reject `tools/*` before a successful
// `initialize`) is per-connection state; a connection is a transport concept.
// The latch therefore lives in this tiny value the adapter creates per
// connection, NOT as ambient mutable state inside the server object (which stays
// immutable and shareable across concurrent connections).
class McpSession {
  bool initialized = false;
}

// --- Tool registry ---
//
// One entry per Baboon method bound to a server. `inputSchema` is the
// precomputed, self-contained JSON Schema (from the shared T5 emitter), carried
// as a constant value — the runtime does not compute schemas.
class McpToolEntry {
  final String name;
  final BaboonMethodId method;
  final Map<String, dynamic> inputSchema;
  final String? description;

  const McpToolEntry(this.name, this.method, this.inputSchema, [this.description]);
}

class McpServerInfo {
  final String name;
  final String version;

  const McpServerInfo(this.name, this.version);
}

// --- Dispatch interface ---
//
// The single generated entrypoint, analogous to IBaboonJsonServiceCtx<Ctx>.
// It is NOT result-parametric (an MCP response is always a JsonRpcResponse value);
// the only free type parameter is the caller's Ctx — the SAME Ctx the
// service-wiring contract threads. `handle` is synchronous and performs no I/O.
// Returns null for an accepted notification (no reply).
abstract class IBaboonMcpServer<Ctx> {
  JsonRpcResponse? handle(JsonRpcRequest request, McpSession session, Ctx ctx, BaboonCodecContext codecCtx);
}

// --- PUBLIC routable-server surface (tasks:T114) ---
//
// The composition seam the cross-service MCP muxer (AbstractMcpMuxer) depends on.
// A sibling muxer reads each server's identity (`serverInfo`) and its
// declaration-ordered registry (`tools`) to build the union tools/list and the
// tool-name -> owner table, and routes a single tools/call into the owning server
// via `routeToolCall` — reusing its existing Channel-A/Channel-B mapping
// unchanged. Those inputs were not behind a stable public interface; this one
// promotes exactly them. The muxer depends on the interface, NEVER on `handle`.
// Dart MCP is sync-only (no async base); `routeToolCall` returns the wire result
// string (or throws BaboonWiringException, exactly as `invokeJsonFn` does).
abstract class IBaboonRoutableMcpServer<Ctx> {
  McpServerInfo get serverInfo;
  List<McpToolEntry> get tools;
  String routeToolCall(BaboonMethodId method, String data, Ctx ctx, BaboonCodecContext codecCtx);
}

// --- Transport-abstract dispatch base ---
//
// Shared `handle` state machine. The generated `<Service>McpServer` extends this
// with its fixed `serverInfo`, ordered tool registry, and `invokeJsonFn` delegate.
// All JSON-RPC method strings ("tools/list" …) and result keys ("protocolVersion",
// "inputSchema" …) are literal lowercase strings, NOT subject to any per-language
// symbol casing.
abstract class AbstractBaboonMcpServer<Ctx> implements IBaboonMcpServer<Ctx>, IBaboonRoutableMcpServer<Ctx> {
  @override
  McpServerInfo get serverInfo;
  @override
  List<McpToolEntry> get tools;

  // The JSON `tools/call` delegate the generated server supplies: routes one
  // tool invocation into the already-generated service dispatch (the errors-mode
  // `invokeJson`, which returns the wire result or throws BaboonWiringException).
  String Function(BaboonMethodId, String, Ctx, BaboonCodecContext) get invokeJsonFn;

  // PUBLIC dispatch entry (tasks:T114): the same path `handle` drives for its
  // own tools/call arm, exposed for the muxer to reuse Channel-A/B unchanged.
  @override
  String routeToolCall(BaboonMethodId method, String data, Ctx ctx, BaboonCodecContext codecCtx) =>
      invokeJsonFn(method, data, ctx, codecCtx);

  Map<String, McpToolEntry> _byName() {
    final m = <String, McpToolEntry>{};
    for (final t in tools) m[t.name] = t;
    return m;
  }

  @override
  JsonRpcResponse? handle(JsonRpcRequest request, McpSession session, Ctx ctx, BaboonCodecContext codecCtx) {
    final id = request.id;
    switch (request.method) {
      case 'initialize': {
        final params = request.params;
        if (params == null || params is! Map) {
          return _errorResponse(id, jsonRpcErrorInvalidParams, 'initialize: missing params');
        }
        if (params['protocolVersion'] == null) {
          return _errorResponse(id, jsonRpcErrorInvalidParams, 'initialize: missing protocolVersion');
        }
        session.initialized = true;
        return JsonRpcResponse(id, result: {
          'protocolVersion': mcpProtocolVersion,
          'capabilities': {'tools': <String, dynamic>{}},
          'serverInfo': {'name': serverInfo.name, 'version': serverInfo.version},
        });
      }
      case 'notifications/initialized':
        return null;
      case 'tools/list': {
        if (!session.initialized) {
          return _errorResponse(id, jsonRpcErrorInvalidRequest, 'tools/list before initialize');
        }
        final toolsArr = tools.map((t) {
          final entry = <String, dynamic>{
            'name': t.name,
            'inputSchema': t.inputSchema,
          };
          if (t.description != null) entry['description'] = t.description;
          return entry;
        }).toList();
        return JsonRpcResponse(id, result: {'tools': toolsArr});
      }
      case 'tools/call': {
        if (!session.initialized) {
          return _errorResponse(id, jsonRpcErrorInvalidRequest, 'tools/call before initialize');
        }
        final params = request.params;
        if (params == null || params is! Map) {
          return _errorResponse(id, jsonRpcErrorInvalidParams, 'tools/call: missing params');
        }
        final toolName = params['name'];
        if (toolName == null || toolName is! String) {
          return _errorResponse(id, jsonRpcErrorInvalidParams, 'tools/call: missing tool name');
        }
        final entry = _byName()[toolName];
        if (entry == null) {
          return _errorResponse(id, jsonRpcErrorInvalidParams, "tools/call: unknown tool '$toolName'");
        }
        final argsRaw = params['arguments'] ?? <String, dynamic>{};
        final argsJson = jsonEncode(argsRaw);
        try {
          final resultStr = invokeJsonFn(entry.method, argsJson, ctx, codecCtx);
          return JsonRpcResponse(id, result: {
            'content': [{'type': 'text', 'text': resultStr}],
            'isError': false,
          });
        } on BaboonWiringException catch (e) {
          // Channel B: a valid protocol call whose domain payload failed.
          return JsonRpcResponse(id, result: {
            'content': [{'type': 'text', 'text': _describeWiringError(e.error)}],
            'isError': true,
          });
        } catch (e) {
          // Channel B: unexpected error during dispatch.
          return JsonRpcResponse(id, result: {
            'content': [{'type': 'text', 'text': e.toString()}],
            'isError': true,
          });
        }
      }
      default:
        return _errorResponse(id, jsonRpcErrorMethodNotFound, 'Method not found: ${request.method}');
    }
  }

  JsonRpcResponse _errorResponse(Object? id, int code, String message) {
    return JsonRpcResponse(id, error: JsonRpcError(code, message));
  }

  String _describeWiringError(BaboonWiringError e) {
    return e.toString();
  }
}
