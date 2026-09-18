import 'package:baboon_runtime/baboon_runtime.dart';
import 'package:baboon_runtime/baboon_mcp_runtime.dart';
import 'package:test/test.dart';

class DynamicServer extends AbstractBaboonMcpServer<int> {
  @override
  final tools = <McpToolEntry>[
    const McpToolEntry('tool', BaboonMethodId('S', 'first'), {}),
  ];
  @override
  McpServerInfo get serverInfo => const McpServerInfo('test', '1');
  @override
  String Function(BaboonMethodId, String, int, BaboonCodecContext) get invokeJsonFn =>
      (method, data, ctx, codecCtx) {
        if (data == '{"fail":true}') throw const BaboonWiringException(NoMatchingMethod(BaboonMethodId('S', 'missing')));
        return '${method.methodName}:$ctx:$data';
      };
}

void main() {
  final codecCtx = BaboonCodecContext.defaultCtx;
  for (final muxed in [false, true]) {
    test('protocol and routing contract muxed=$muxed', () {
      final member = DynamicServer();
      final IBaboonMcpServer<int> server = muxed
          ? AbstractMcpMuxer<int>(const McpServerInfo('merged', '1'), [member])
          : member;
      final session = McpSession();
      JsonRpcResponse? call(String method, Object? params) =>
          server.handle(JsonRpcRequest(7, method, params), session, 42, codecCtx);
      expect(call('tools/list', null)!.error!.code, jsonRpcErrorInvalidRequest);
      expect(call('initialize', {})!.error!.code, jsonRpcErrorInvalidParams);
      expect(session.initialized, false);
      expect(call('initialize', {'protocolVersion': 'anything'})!.id, 7);
      expect(call('notifications/initialized', null), null);
      expect(call('tools/list', null)!.result['tools'][0]['name'], 'tool');
      final circular = <Object?>[];
      circular.add(circular);
      expect(call('tools/call', {'name': 'missing', 'arguments': circular})!.error!.code, jsonRpcErrorInvalidParams);
      expect(() => call('tools/call', {'name': 'tool', 'arguments': circular}), throwsA(isA<Error>()));
      final result = call('tools/call', {'name': 'tool'})!;
      expect(result.result['content'][0]['text'], 'first:42:{}');
      expect(result.result['isError'], false);
      expect(call('tools/call', {'name': 'tool', 'arguments': {'fail': true}})!.result['isError'], true);
      expect(call('unknown', null)!.error!.code, jsonRpcErrorMethodNotFound);
    });
  }
  test('standalone lookup observes mutations and selects last duplicate', () {
    final server = DynamicServer();
    final session = McpSession()..initialized = true;
    server.tools.add(const McpToolEntry('tool', BaboonMethodId('S', 'last'), {}));
    final result = server.handle(const JsonRpcRequest(1, 'tools/call', {'name': 'tool'}), session, 0, codecCtx)!;
    expect(result.result['content'][0]['text'], 'last:0:{}');
    server.tools.clear();
    expect(server.handle(const JsonRpcRequest(1, 'tools/call', {'name': 'tool'}), session, 0, codecCtx)!.error!.code, jsonRpcErrorInvalidParams);
  });
}
