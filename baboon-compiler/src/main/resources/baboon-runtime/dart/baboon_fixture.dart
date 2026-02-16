import 'dart:math';
import 'dart:typed_data';
import 'baboon_runtime.dart';

class BaboonRandom {
  final Random _rnd;

  BaboonRandom([Random? rnd]) : _rnd = rnd ?? Random();

  bool nextBool() => _rnd.nextBool();

  int nextI08() => _rnd.nextInt(256) - 128;
  int nextI16() => _rnd.nextInt(65536) - 32768;
  int nextI32() => _rnd.nextInt(0x7FFFFFFF) * (_rnd.nextBool() ? 1 : -1);
  int nextI64() {
    final high = _rnd.nextInt(0x7FFFFFFF);
    final low = _rnd.nextInt(0xFFFFFFFF);
    return (high << 32) | low;
  }

  int nextU08() => _rnd.nextInt(256);
  int nextU16() => _rnd.nextInt(65536);
  int nextU32() => _rnd.nextInt(0xFFFFFFFF);
  int nextU64() {
    final high = _rnd.nextInt(0xFFFFFFFF);
    final low = _rnd.nextInt(0xFFFFFFFF);
    return (high << 32) | low;
  }

  double nextF32() {
    final bd = ByteData(4);
    bd.setFloat32(0, _rnd.nextDouble() * 1000.0 - 500.0, Endian.little);
    return bd.getFloat32(0, Endian.little);
  }

  double nextF64() => _rnd.nextDouble() * 1e15 - 5e14;

  BaboonDecimal nextDecimal() {
    final intPart = _rnd.nextInt(999999) - 499999;
    final fracPart = _rnd.nextInt(1000000);
    return BaboonDecimal('$intPart.${fracPart.toString().padLeft(6, '0')}');
  }

  String nextString() {
    final length = _rnd.nextInt(20) + 1;
    const chars = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
    return String.fromCharCodes(
      List.generate(length, (_) => chars.codeUnitAt(_rnd.nextInt(chars.length))),
    );
  }

  Uint8List nextBytes() {
    final length = _rnd.nextInt(32) + 1;
    return Uint8List.fromList(List.generate(length, (_) => _rnd.nextInt(256)));
  }

  String nextUuid() {
    final bytes = List.generate(16, (_) => _rnd.nextInt(256));
    bytes[6] = (bytes[6] & 0x0f) | 0x40; // version 4
    bytes[8] = (bytes[8] & 0x3f) | 0x80; // variant
    final hex = bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
    return '${hex.substring(0, 8)}-${hex.substring(8, 12)}-${hex.substring(12, 16)}-${hex.substring(16, 20)}-${hex.substring(20)}';
  }

  DateTime nextTsu() {
    final year = 2000 + _rnd.nextInt(30);
    final month = 1 + _rnd.nextInt(12);
    final day = 1 + _rnd.nextInt(28);
    final hour = _rnd.nextInt(24);
    final minute = _rnd.nextInt(60);
    final second = _rnd.nextInt(60);
    final millis = _rnd.nextInt(1000);
    return DateTime.utc(year, month, day, hour, minute, second, millis);
  }

  BaboonDateTimeOffset nextTso() {
    final dt = nextTsu();
    final offsetHours = _rnd.nextInt(25) - 12;
    final offsetMillis = offsetHours * 3600000;
    return BaboonDateTimeOffset(
      epochMillis: dt.millisecondsSinceEpoch,
      offsetMillis: offsetMillis,
      kind: 'offset',
    );
  }

  int nextIntRange(int max) => _rnd.nextInt(max);

  T oneOf<T>(List<T> items) => items[_rnd.nextInt(items.length)];

  List<T> mkList<T>(T Function() gen) {
    final length = _rnd.nextInt(5) + 1;
    return List.generate(length, (_) => gen());
  }

  Set<T> mkSet<T>(T Function() gen) {
    final length = _rnd.nextInt(5) + 1;
    final result = <T>{};
    var attempts = 0;
    while (result.length < length && attempts < length * 10) {
      result.add(gen());
      attempts++;
    }
    return result;
  }

  Map<K, V> mkMap<K, V>(K Function() genKey, V Function() genValue) {
    final length = _rnd.nextInt(5) + 1;
    final result = <K, V>{};
    var attempts = 0;
    while (result.length < length && attempts < length * 10) {
      result[genKey()] = genValue();
      attempts++;
    }
    return result;
  }

  T? mkNullable<T>(T Function() gen) {
    return _rnd.nextBool() ? gen() : null;
  }

  T mkEnum<T>(List<T> values) {
    return values[_rnd.nextInt(values.length)];
  }
}

class BaboonRandomFactory {
  static BaboonRandom create() => BaboonRandom();
}
