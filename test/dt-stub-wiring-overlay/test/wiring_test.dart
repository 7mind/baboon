// Service-wiring + cross-domain muxer tests (no-errors service-result mode).
// Generated into this stub only by the `test-gen-dt-wiring` mdl action
// (rsync + codegen with --service-result-no-errors=true). Exercises the
// per-domain dispatchers (I1Wiring/I2Wiring) and the cross-domain
// JsonMuxer/UebaMuxer that routes by method.serviceId.
import 'dart:convert';
import 'dart:typed_data';

import 'package:baboon_runtime/baboon_runtime.dart';
import 'package:test/test.dart';

import '../lib/testpkg/pkg0/i1.dart';
import '../lib/testpkg/pkg0/i2.dart';
import '../lib/testpkg/pkg0/i1/testcall/in.dart' as i1in;
import '../lib/testpkg/pkg0/i1/testcall/out.dart' as i1out;
import '../lib/testpkg/pkg0/i2/noerrcall/in.dart' as i2in;
import '../lib/testpkg/pkg0/i2/noerrcall/out.dart' as i2out;
import '../lib/testpkg/pkg0/t7_empty.dart';

class MockI1 implements I1 {
  @override
  i1out.out testCall(i1in.in_ arg) => const i1out.out(i00: 42);
  @override
  T7_Empty testCall2(T7_Empty arg) => const T7_Empty();
}

class MockI2 implements I2 {
  @override
  i2out.out noErrCall(i2in.in_ arg) => i2out.out(result: 'result_${arg.value}');
}

void main() {
  final ctx = BaboonCodecContext.defaultCtx;

  String encodeI1In() => jsonEncode(i1in.in_JsonCodec.instance.encode(ctx, const i1in.in_()));
  String encodeI2In() => jsonEncode(i2in.in_JsonCodec.instance.encode(ctx, const i2in.in_(value: 123)));

  // ==================== Per-domain dispatch ====================

  test('I1Wiring.invokeJson routes testCall', () {
    final method = BaboonMethodId('I1', 'testCall');
    final result = I1Wiring.invokeJson(method, encodeI1In(), MockI1(), ctx);
    final decoded = i1out.out_JsonCodec.instance.decode(ctx, jsonDecode(result));
    expect(decoded.i00, 42);
  });

  test('I1Wiring.invokeJson throws NoMatchingMethod', () {
    final method = BaboonMethodId('I1', 'nonexistent');
    expect(
      () => I1Wiring.invokeJson(method, '{}', MockI1(), ctx),
      throwsA(predicate((e) => e is BaboonWiringException && e.error is NoMatchingMethod)),
    );
  });

  // ==================== Cross-domain Muxer ====================
  // A single muxer composes I1 and I2 and routes by method.serviceId.

  JsonMuxer<String> newJsonMuxer() =>
      JsonMuxer<String>([I1JsonService(MockI1()), I2JsonService(MockI2())]);
  UebaMuxer<Uint8List> newUebaMuxer() =>
      UebaMuxer<Uint8List>([I1UebaService(MockI1()), I2UebaService(MockI2())]);

  test('JsonMuxer routes to I1', () {
    final method = BaboonMethodId('I1', 'testCall');
    final result = newJsonMuxer().invoke(method, encodeI1In(), ctx);
    final decoded = i1out.out_JsonCodec.instance.decode(ctx, jsonDecode(result));
    expect(decoded.i00, 42);
  });

  test('JsonMuxer routes to I2', () {
    final method = BaboonMethodId('I2', 'noErrCall');
    final result = newJsonMuxer().invoke(method, encodeI2In(), ctx);
    final decoded = i2out.out_JsonCodec.instance.decode(ctx, jsonDecode(result));
    expect(decoded.result, 'result_123');
  });

  test('JsonMuxer throws NoMatchingService for unregistered service', () {
    final method = BaboonMethodId('Nonexistent', 'x');
    expect(
      () => newJsonMuxer().invoke(method, '{}', ctx),
      throwsA(predicate((e) => e is BaboonWiringException && e.error is NoMatchingService)),
    );
  });

  test('JsonMuxer throws DuplicateService on duplicate registration', () {
    expect(
      () => JsonMuxer<String>([I1JsonService(MockI1()), I1JsonService(MockI1())]),
      throwsA(predicate((e) => e is BaboonWiringException && e.error is DuplicateService)),
    );
  });

  test('UebaMuxer routes to I1', () {
    final method = BaboonMethodId('I1', 'testCall');
    final writer = BaboonBinTools.createWriter();
    i1in.in_UebaCodec.instance.encode(ctx, writer, const i1in.in_());
    final result = newUebaMuxer().invoke(method, writer.toBytes(), ctx);
    final decoded = i1out.out_UebaCodec.instance.decode(ctx, BaboonBinTools.createReader(result));
    expect(decoded.i00, 42);
  });

  test('UebaMuxer routes to I2', () {
    final method = BaboonMethodId('I2', 'noErrCall');
    final writer = BaboonBinTools.createWriter();
    i2in.in_UebaCodec.instance.encode(ctx, writer, const i2in.in_(value: 456));
    final result = newUebaMuxer().invoke(method, writer.toBytes(), ctx);
    final decoded = i2out.out_UebaCodec.instance.decode(ctx, BaboonBinTools.createReader(result));
    expect(decoded.result, 'result_456');
  });

  test('UebaMuxer throws NoMatchingService for unregistered service', () {
    final method = BaboonMethodId('Nonexistent', 'x');
    expect(
      () => newUebaMuxer().invoke(method, BaboonBinTools.createWriter().toBytes(), ctx),
      throwsA(predicate((e) => e is BaboonWiringException && e.error is NoMatchingService)),
    );
  });
}
