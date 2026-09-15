// Cross-language golden bytes for the top-level binary envelope (docs/spec/codec-envelope.md §2.1, §2.1.2, §2.1.3; docs/forward-compat.md, "Worked examples"). The same values must produce these exact bytes in every backend; the Scala and TypeScript stubs assert the same sequences structurally.
import 'dart:typed_data';

import 'package:baboon_runtime/baboon_codecs_facade.dart';
import 'package:baboon_runtime/baboon_runtime.dart';
import 'package:test/test.dart';

import '../../lib/fwde2e/chain/chain_append.dart';
import '../../lib/fwde2e/chain/domain_fwde2e_chain_facade.dart';
import '../../lib/fwde2e/fwd/domain_fwde2e_fwd_facade.dart';
import '../../lib/fwde2e/fwd/fwd_append_var.dart';
import '../../lib/fwde2e/fwd/fwd_enum_grows.dart';
import '../../lib/fwde2e/fwd/fwd_enum_host.dart';
import '../../lib/fwde2e/fwd/fwd_stable.dart';

String hx(Uint8List b) => b.map((x) => x.toRadixString(16).padLeft(2, '0').toUpperCase()).join(' ');

Uint8List enc(BaboonCodecsFacade f, BaboonCodecContext ctx, BaboonGenerated v) {
  final encoded = f.encodeToBin(ctx, v);
  expect(encoded, isA<BaboonRight<BaboonCodecException, dynamic>>());
  return (encoded as BaboonRight<BaboonCodecException, dynamic>).value as Uint8List;
}

final v1Tolerant = BaboonCodecContext.custom(false, ForwardWritePolicy.tolerant, BaboonEnvelopeVersion.v1, null);
final v2Compact = BaboonCodecContext.custom(false, ForwardWritePolicy.strict, BaboonEnvelopeVersion.v2, null);
final v2Indexed = BaboonCodecContext.custom(true, ForwardWritePolicy.strict, BaboonEnvelopeVersion.v2, null);

void main() {
  test('default contexts write v1 Strict', () {
    expect(BaboonCodecContext.compact.envelopeVersion, BaboonEnvelopeVersion.v1);
    expect(BaboonCodecContext.compact.forwardWritePolicy, ForwardWritePolicy.strict);
    expect(BaboonCodecContext.indexed.envelopeVersion, BaboonEnvelopeVersion.v1);
  });

  test('envelopes match the cross-language golden bytes', () {
    final fwd = DomainFwde2eFwdFacade();
    final chain = DomainFwde2eChainFacade();
    const app = FwdAppendVar(a: 42, b: 'hi', t: 't');
    // FwdAppendVar, v1 Strict (default) compact: identical bound elided
    expect(hx(enc(fwd, BaboonCodecContext.compact, app)), '01 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 00 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 00 2A 00 00 00 02 68 69 01 01 74');
    // FwdAppendVar, v1 Tolerant compact: prefix-compact bound 1.0.0 in the single slot
    expect(hx(enc(fwd, v1Tolerant, app)), '01 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 01 05 31 2E 30 2E 30 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 00 2A 00 00 00 02 68 69 01 01 74');
    // FwdAppendVar, v2 compact: flags 0b10, readableMin 1.0.0
    expect(hx(enc(fwd, v2Compact, app)), '02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 02 05 31 2E 30 2E 30 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 00 2A 00 00 00 02 68 69 01 01 74');
    // FwdAppendVar, v2 indexed: flags 0 (prefix-any-mode bound is 2.0.0)
    expect(hx(enc(fwd, v2Indexed, app)), '02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 00 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 01 04 00 00 00 03 00 00 00 07 00 00 00 03 00 00 00 2A 00 00 00 02 68 69 01 01 74');
    // FwdStable, v1 Strict compact: byte-identical since 1.0.0 -> hasMinCompat 1
    expect(hx(enc(fwd, BaboonCodecContext.compact, const FwdStable(s: 's'))), '01 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 01 05 31 2E 30 2E 30 16 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 53 74 61 62 6C 65 00 01 73');
    // FwdStable, v2 compact: flags 0b01, minCompat 1.0.0, readableMin elided
    expect(hx(enc(fwd, v2Compact, const FwdStable(s: 's'))), '02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 01 05 31 2E 30 2E 30 16 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 53 74 61 62 6C 65 00 01 73');
    // FwdEnumHost, v2 compact: flags 0, no bound
    expect(hx(enc(fwd, v2Compact, const FwdEnumHost(e: FwdEnumGrows.C))), '02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 00 18 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 45 6E 75 6D 48 6F 73 74 00 02');
    // ChainAppend 3.0.0, v2 compact: flags 0b10, readableMin 1.0.0
    expect(hx(enc(chain, v2Compact, const ChainAppend(a: 1, b: 'b', c: 'c'))), '02 0C 66 77 64 65 32 65 2E 63 68 61 69 6E 05 33 2E 30 2E 30 02 05 31 2E 30 2E 30 1A 66 77 64 65 32 65 2E 63 68 61 69 6E 2F 3A 23 43 68 61 69 6E 41 70 70 65 6E 64 00 01 00 00 00 01 01 62 01 01 63');
  });

  test('v2 envelope round-trips through its own facade', () {
    final fwd = DomainFwde2eFwdFacade();
    const app = FwdAppendVar(a: 42, b: 'hi', t: 't');
    final decoded = fwd.decodeFromBinBytes(enc(fwd, v2Compact, app));
    expect(decoded, isA<BaboonRight<BaboonCodecException, BaboonGenerated>>());
    expect((decoded as BaboonRight<BaboonCodecException, BaboonGenerated>).value, equals(app));
  });
}
