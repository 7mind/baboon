// Hand-written tests for the BaboonExt helpers added in MFACADE-PR-5:
//   BaboonMetaProvider.domainVersion (getter)
//   BaboonMetaProvider.baboonUnmodifiedSinceVersion (getter)
//   BaboonMeta.unmodifiedSinceVersion(String) (method)
//
// Uses only baboon_runtime symbols (no generated types) so it is not stomped
// by codegen and runs against the bare runtime package.

import 'package:baboon_runtime/baboon_runtime.dart';
import 'package:test/test.dart';

// Minimal stub implementing BaboonMetaProvider — mirrors _StubGenerated from
// any_meta_codec_test.dart.
class _StubMetaProvider implements BaboonMetaProvider {
  @override
  final String baboonDomainVersion;
  @override
  final String baboonDomainIdentifier;
  @override
  final String baboonTypeIdentifier;
  @override
  final List<String> baboonSameInVersions;

  const _StubMetaProvider({
    required this.baboonDomainVersion,
    required this.baboonDomainIdentifier,
    required this.baboonTypeIdentifier,
    required this.baboonSameInVersions,
  });
}

// Minimal stub implementing BaboonMeta (per-domain-version registry).
class _StubDomainMeta implements BaboonMeta {
  final List<String> _versions;

  const _StubDomainMeta(this._versions);

  @override
  List<String> sameInVersions(String typeId) => _versions;
}

void main() {
  group('BaboonExt — BaboonMetaProvider extensions', () {
    // --- domainVersion ---

    test('domainVersion returns BaboonDomainVersion with correct identifier and version', () {
      const stub = _StubMetaProvider(
        baboonDomainVersion: '3.1.4',
        baboonDomainIdentifier: 'my.domain',
        baboonTypeIdentifier: 'MyType',
        baboonSameInVersions: ['2.0.0', '3.1.4'],
      );
      final dv = stub.domainVersion;
      expect(dv.domainIdentifier, 'my.domain');
      expect(dv.domainVersion, '3.1.4');
    });

    test('domainVersion toString format is identifier:version', () {
      const stub = _StubMetaProvider(
        baboonDomainVersion: '1.0.0',
        baboonDomainIdentifier: 'acme.corp',
        baboonTypeIdentifier: 'Widget',
        baboonSameInVersions: ['1.0.0'],
      );
      expect(stub.domainVersion.toString(), 'acme.corp:1.0.0');
    });

    // --- baboonUnmodifiedSinceVersion ---

    test('baboonUnmodifiedSinceVersion returns first element of baboonSameInVersions', () {
      const stub = _StubMetaProvider(
        baboonDomainVersion: '5.0.0',
        baboonDomainIdentifier: 'dom',
        baboonTypeIdentifier: 'T',
        baboonSameInVersions: ['2.0.0', '3.0.0', '5.0.0'],
      );
      expect(stub.baboonUnmodifiedSinceVersion, '2.0.0');
    });

    test('baboonUnmodifiedSinceVersion returns single element when list has one entry', () {
      const stub = _StubMetaProvider(
        baboonDomainVersion: '1.0.0',
        baboonDomainIdentifier: 'dom',
        baboonTypeIdentifier: 'T',
        baboonSameInVersions: ['1.0.0'],
      );
      expect(stub.baboonUnmodifiedSinceVersion, '1.0.0');
    });
  });

  group('BaboonExt — BaboonMeta extensions', () {
    // --- unmodifiedSinceVersion ---

    test('unmodifiedSinceVersion returns first element from sameInVersions', () {
      final meta = _StubDomainMeta(['1.0.0', '2.0.0']);
      expect(meta.unmodifiedSinceVersion('any.TypeId'), '1.0.0');
    });

    test('unmodifiedSinceVersion is independent of typeId argument', () {
      final meta = _StubDomainMeta(['1.0.0', '2.0.0']);
      expect(meta.unmodifiedSinceVersion('other.Type'), '1.0.0');
    });
  });
}
