// Reproduction + regression test for PR-22-D05: `AbstractBaboonConversions._registry`
// must be keyed on the (fromTypeId, toTypeId) pair, not on `fromTypeId` alone.
//
// Two distinct conversions sharing the same source type previously overwrote each other
// silently, and `convertWithContext` ignored its `toTypeId` argument. Both lookups are
// now exercised below.

import 'package:baboon_runtime/baboon_runtime.dart';
import 'package:test/test.dart';

class _SrcValue {
  final int n;
  const _SrcValue(this.n);
}

class _TargetA {
  final int n;
  const _TargetA(this.n);
}

class _TargetB {
  final String s;
  const _TargetB(this.s);
}

const _fromId = 'pkg/Src';
const _toIdA = 'pkg/TargetA';
const _toIdB = 'pkg/TargetB';

class _ConvSrcToA extends AbstractConversion<_SrcValue, _TargetA> {
  @override
  String get versionFrom => '1.0.0';
  @override
  String get versionTo => '2.0.0';
  @override
  String get typeId => _fromId;
  @override
  String get toTypeId => _toIdA;
  @override
  _TargetA doConvert(dynamic context, AbstractBaboonConversions conversions, _SrcValue from) {
    return _TargetA(from.n);
  }
}

class _ConvSrcToB extends AbstractConversion<_SrcValue, _TargetB> {
  @override
  String get versionFrom => '1.0.0';
  @override
  String get versionTo => '2.0.0';
  @override
  String get typeId => _fromId;
  @override
  String get toTypeId => _toIdB;
  @override
  _TargetB doConvert(dynamic context, AbstractBaboonConversions conversions, _SrcValue from) {
    return _TargetB('s${from.n}');
  }
}
// NOTE on the pre-fix repro: prior to the (fromTypeId, toTypeId) registry change,
// `AbstractConversion` had no `toTypeId` member and the registry was
// `Map<String, AbstractConversion>` keyed on `typeId` only. Repeating
// `register(_ConvSrcToA())` then `register(_ConvSrcToB())` overwrote the first entry,
// so `convertWithContext(_, _, _fromId, _toIdA)` returned a `_TargetB` and the first
// expectation in the dispatch test fired `TypeMatcher<_TargetA>` mismatch. The repro
// is the test body below — it executes against the current (post-fix) code.

class _Conversions extends AbstractBaboonConversions {
  @override
  List<String> get versionsFrom => const ['1.0.0'];
  @override
  String get versionTo => '2.0.0';
}

void main() {
  group('AbstractBaboonConversions PR-22-D05 (fromTypeId, toTypeId) registry', () {
    test('two distinct targets from the same source dispatch independently', () {
      final conversions = _Conversions();
      conversions.register(_ConvSrcToA());
      conversions.register(_ConvSrcToB());

      final src = _SrcValue(7);

      final a = conversions.convertWithContext(null, src, _fromId, _toIdA);
      expect(a, isA<_TargetA>());
      expect((a as _TargetA).n, 7);

      final b = conversions.convertWithContext(null, src, _fromId, _toIdB);
      expect(b, isA<_TargetB>());
      expect((b as _TargetB).s, 's7');
    });

    test('unknown fromTypeId throws', () {
      final conversions = _Conversions();
      conversions.register(_ConvSrcToA());

      expect(
        () => conversions.convertWithContext(null, _SrcValue(1), 'pkg/Unknown', _toIdA),
        throwsA(isA<ArgumentError>()),
      );
    });

    test('known fromTypeId but unknown toTypeId throws', () {
      final conversions = _Conversions();
      conversions.register(_ConvSrcToA());

      expect(
        () => conversions.convertWithContext(null, _SrcValue(1), _fromId, 'pkg/Unknown'),
        throwsA(isA<ArgumentError>()),
      );
    });
  });
}
