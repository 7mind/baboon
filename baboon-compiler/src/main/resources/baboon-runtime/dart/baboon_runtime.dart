import 'dart:typed_data';
import 'dart:convert';

// --- Metadata Interfaces ---

/// Marker for every generated DTO/ADT/enum value. Dart static fields are not reachable through
/// interface dispatch, so the facade leans on the [BaboonMetaProvider] instance accessors
/// (added below) — generated code in PR 8.2/8.3 emits both the static-const meta (existing) and
/// the instance getters (new) so [BaboonTypeMeta.from] can read meta from a value of unknown
/// concrete type.
abstract interface class BaboonGenerated {}

abstract interface class BaboonGeneratedLatest implements BaboonGenerated {}

/// Marker for ADT branch types. The matching `static const String baboonAdtTypeIdentifier` is
/// emitted by codegen on the generated class; instance access for the facade goes via the
/// [BaboonAdtMember] mixin (added below) when PR 8.2/8.3 land.
abstract interface class BaboonAdtMemberMeta {
  static const String baboonAdtTypeIdentifier = '';
}

// --- Codec Context ---

/// Marker base for `BaboonCodecsFacade` so [BaboonCodecContext] can carry an optional facade
/// reference without an import cycle between `baboon_runtime.dart` and `baboon_codecs_facade.dart`.
/// The real facade lives in `baboon_codecs_facade.dart` and extends this base.
abstract class BaboonCodecsFacadeBase {
  const BaboonCodecsFacadeBase();
}

/// Codec context. Exposes [useIndices] (UEBA index emission) and an optional [facade] reference
/// threaded through generated codec calls so the `any`-feature cross-format conversion
/// (UEBA <-> JSON) can resolve codecs by `(domain, version, typeid)` from an `AnyMeta` envelope.
/// `null` for the bare [defaultCtx]/[indexed]/[compact] singletons; [withFacade] is the single
/// intended construction path for ctxes that thread a facade. Mirrors Scala/C#/Java/Kotlin/TS.
abstract class BaboonCodecContext {
  const BaboonCodecContext();

  bool get useIndices;
  BaboonCodecsFacadeBase? get facade => null;

  static const BaboonCodecContext defaultCtx = _BaboonCodecContextCompact();
  static const BaboonCodecContext compact = _BaboonCodecContextCompact();
  static const BaboonCodecContext indexed = _BaboonCodecContextIndexed();

  static BaboonCodecContext withFacade(bool useIndices, BaboonCodecsFacadeBase facade) =>
      _BaboonCodecContextWithFacade(useIndices, facade);
}

class _BaboonCodecContextCompact extends BaboonCodecContext {
  const _BaboonCodecContextCompact();
  @override
  bool get useIndices => false;
}

class _BaboonCodecContextIndexed extends BaboonCodecContext {
  const _BaboonCodecContextIndexed();
  @override
  bool get useIndices => true;
}

class _BaboonCodecContextWithFacade extends BaboonCodecContext {
  final bool _useIndices;
  final BaboonCodecsFacadeBase _facade;
  const _BaboonCodecContextWithFacade(this._useIndices, this._facade);
  @override
  bool get useIndices => _useIndices;
  @override
  BaboonCodecsFacadeBase get facade => _facade;
}

// --- Codec Data ---

/// Static metadata exposed by every generated codec — used by [BaboonCodecsFacade] to look up
/// codecs by `(domain, version, typeid)`. Mirrors Kotlin's `BaboonCodecData` interface.
abstract class BaboonCodecData {
  String get baboonDomainVersion;
  String get baboonDomainIdentifier;
  String get baboonTypeIdentifier;
}

// --- JSON Codecs ---

abstract class BaboonJsonCodec<T> implements BaboonCodecData {
  const BaboonJsonCodec();

  T decode(BaboonCodecContext ctx, dynamic wire);
}

abstract class BaboonJsonCodecBase<T> extends BaboonJsonCodec<T> {
  const BaboonJsonCodecBase();

  dynamic encode(BaboonCodecContext ctx, T value);
}

abstract class BaboonJsonCodecBaseGenerated<T> extends BaboonJsonCodecBase<T> {
  const BaboonJsonCodecBaseGenerated();
}

abstract class BaboonJsonCodecBaseGeneratedAdt<T> extends BaboonJsonCodecBase<T> {
  const BaboonJsonCodecBaseGeneratedAdt();
}

abstract class BaboonJsonCodecNoEncoder<T> extends BaboonJsonCodec<T> {
  const BaboonJsonCodecNoEncoder();
}

abstract class BaboonJsonCodecNoEncoderGenerated<T> extends BaboonJsonCodecNoEncoder<T> {
  const BaboonJsonCodecNoEncoderGenerated();
}

abstract class BaboonJsonCodecNoEncoderGeneratedAdt<T> extends BaboonJsonCodecNoEncoder<T> {
  const BaboonJsonCodecNoEncoderGeneratedAdt();
}

// --- Binary Codecs ---

abstract class BaboonBinCodec<T> implements BaboonCodecData {
  const BaboonBinCodec();

  T decode(BaboonCodecContext ctx, BaboonBinReader reader);
}

abstract class BaboonBinCodecBase<T> extends BaboonBinCodec<T> {
  const BaboonBinCodecBase();

  void encode(BaboonCodecContext ctx, BaboonBinWriter writer, T value);
}

abstract class BaboonBinCodecBaseGenerated<T> extends BaboonBinCodecBase<T> {
  const BaboonBinCodecBaseGenerated();
}

abstract class BaboonBinCodecBaseGeneratedAdt<T> extends BaboonBinCodecBase<T> {
  const BaboonBinCodecBaseGeneratedAdt();
}

abstract class BaboonBinCodecNoEncoder<T> extends BaboonBinCodec<T> {
  const BaboonBinCodecNoEncoder();
}

abstract class BaboonBinCodecNoEncoderGenerated<T> extends BaboonBinCodecNoEncoder<T> {
  const BaboonBinCodecNoEncoderGenerated();
}

abstract class BaboonBinCodecNoEncoderGeneratedAdt<T> extends BaboonBinCodecNoEncoder<T> {
  const BaboonBinCodecNoEncoderGeneratedAdt();
}

mixin BaboonBinCodecIndexed {
  int get indexElementsCount;

  List<BaboonIndexEntry> readIndex(BaboonCodecContext ctx, BaboonBinReader reader) {
    final header = reader.readU8();
    final hasIndex = (header & 1) != 0;
    if (!hasIndex) return [];
    final count = indexElementsCount;
    final entries = <BaboonIndexEntry>[];
    for (var i = 0; i < count; i++) {
      final offset = reader.readI32();
      final length = reader.readI32();
      entries.add(BaboonIndexEntry(offset, length));
    }
    return entries;
  }
}

class BaboonIndexEntry {
  final int offset;
  final int length;

  const BaboonIndexEntry(this.offset, this.length);
}

// --- Binary Writer ---

class BaboonBinWriter {
  static const int _dotnetEpochOffsetMs = 62135596800000;
  Uint8List _buf;
  int _pos;

  BaboonBinWriter([int initialCapacity = 256])
      : _buf = Uint8List(initialCapacity),
        _pos = 0;

  void _ensureCapacity(int needed) {
    if (_pos + needed > _buf.length) {
      var newCap = _buf.length * 2;
      while (newCap < _pos + needed) {
        newCap *= 2;
      }
      final newBuf = Uint8List(newCap);
      newBuf.setRange(0, _pos, _buf);
      _buf = newBuf;
    }
  }

  int get position => _pos;

  void writeU8(int value) {
    _ensureCapacity(1);
    _buf[_pos++] = value & 0xFF;
  }

  void writeI8(int value) {
    _ensureCapacity(1);
    final bd = ByteData(1);
    bd.setInt8(0, value);
    _buf[_pos++] = bd.getUint8(0);
  }

  void writeU16(int value) {
    _ensureCapacity(2);
    final bd = ByteData(2);
    bd.setUint16(0, value, Endian.little);
    _buf.setRange(_pos, _pos + 2, bd.buffer.asUint8List());
    _pos += 2;
  }

  void writeI16(int value) {
    _ensureCapacity(2);
    final bd = ByteData(2);
    bd.setInt16(0, value, Endian.little);
    _buf.setRange(_pos, _pos + 2, bd.buffer.asUint8List());
    _pos += 2;
  }

  void writeU32(int value) {
    _ensureCapacity(4);
    final bd = ByteData(4);
    bd.setUint32(0, value, Endian.little);
    _buf.setRange(_pos, _pos + 4, bd.buffer.asUint8List());
    _pos += 4;
  }

  void writeI32(int value) {
    _ensureCapacity(4);
    final bd = ByteData(4);
    bd.setInt32(0, value, Endian.little);
    _buf.setRange(_pos, _pos + 4, bd.buffer.asUint8List());
    _pos += 4;
  }

  void writeU64(int value) {
    _ensureCapacity(8);
    final bd = ByteData(8);
    bd.setUint64(0, value, Endian.little);
    _buf.setRange(_pos, _pos + 8, bd.buffer.asUint8List());
    _pos += 8;
  }

  void writeI64(int value) {
    _ensureCapacity(8);
    final bd = ByteData(8);
    bd.setInt64(0, value, Endian.little);
    _buf.setRange(_pos, _pos + 8, bd.buffer.asUint8List());
    _pos += 8;
  }

  void writeF32(double value) {
    _ensureCapacity(4);
    final bd = ByteData(4);
    bd.setFloat32(0, value, Endian.little);
    _buf.setRange(_pos, _pos + 4, bd.buffer.asUint8List());
    _pos += 4;
  }

  void writeF64(double value) {
    _ensureCapacity(8);
    final bd = ByteData(8);
    bd.setFloat64(0, value, Endian.little);
    _buf.setRange(_pos, _pos + 8, bd.buffer.asUint8List());
    _pos += 8;
  }

  void writeBool(bool value) {
    writeU8(value ? 1 : 0);
  }

  void writeString(String value) {
    final bytes = utf8.encode(value);
    // 7-bit VLQ encoding for string length (compatible with .NET BinaryWriter)
    var len = bytes.length;
    do {
      var currentByte = len & 0x7F;
      len >>= 7;
      if (len != 0) currentByte |= 0x80;
      writeU8(currentByte);
    } while (len != 0);
    writeAll(Uint8List.fromList(bytes));
  }

  void writeBytes(Uint8List data) {
    writeI32(data.length);
    writeAll(data);
  }

  void writeDecimal(BaboonDecimal value) {
    // .NET decimal format: lo (i32), mid (i32), hi (i32), flags (i32) = 16 bytes
    String str = value.value;
    final isNeg = str.startsWith('-');
    if (isNeg) str = str.substring(1);

    int scale = 0;
    final dotIdx = str.indexOf('.');
    if (dotIdx >= 0) {
      scale = str.length - dotIdx - 1;
    }

    final mantissa = BigInt.parse(str.replaceAll('.', ''));
    final lo = ((mantissa) & BigInt.from(0xFFFFFFFF)).toInt();
    final mid = ((mantissa >> 32) & BigInt.from(0xFFFFFFFF)).toInt();
    final hi = ((mantissa >> 64) & BigInt.from(0xFFFFFFFF)).toInt();

    final sign = isNeg ? 0x80000000 : 0;
    final flags = sign | (scale << 16);

    _writeRawI32(lo);
    _writeRawI32(mid);
    _writeRawI32(hi);
    _writeRawI32(flags);
  }

  void _writeRawI32(int value) {
    _ensureCapacity(4);
    final bd = ByteData(4);
    bd.setUint32(0, value & 0xFFFFFFFF, Endian.little);
    _buf.setRange(_pos, _pos + 4, bd.buffer.asUint8List());
    _pos += 4;
  }

  void writeUuid(String uuid) {
    final hex = uuid.replaceAll('-', '');
    assert(hex.length == 32);

    // .NET mixed-endian GUID format
    final bytes = Uint8List(16);
    // First 4 bytes: little-endian
    bytes[0] = int.parse(hex.substring(6, 8), radix: 16);
    bytes[1] = int.parse(hex.substring(4, 6), radix: 16);
    bytes[2] = int.parse(hex.substring(2, 4), radix: 16);
    bytes[3] = int.parse(hex.substring(0, 2), radix: 16);
    // Next 2 bytes: little-endian
    bytes[4] = int.parse(hex.substring(10, 12), radix: 16);
    bytes[5] = int.parse(hex.substring(8, 10), radix: 16);
    // Next 2 bytes: little-endian
    bytes[6] = int.parse(hex.substring(14, 16), radix: 16);
    bytes[7] = int.parse(hex.substring(12, 14), radix: 16);
    // Remaining 8 bytes: big-endian
    for (var i = 0; i < 8; i++) {
      bytes[8 + i] = int.parse(hex.substring(16 + i * 2, 18 + i * 2), radix: 16);
    }
    writeAll(bytes);
  }

  void writeTsu(DateTime value) {
    final epochMs = value.toUtc().millisecondsSinceEpoch;
    final dotnetUtcMs = epochMs + _dotnetEpochOffsetMs;
    writeI64(dotnetUtcMs);
    writeI64(0);
    writeU8(1); // kind = 1 (UTC)
  }

  void writeTso(BaboonDateTimeOffset value) {
    final dotnetUtcMs = value.epochMillis + _dotnetEpochOffsetMs;
    final dotnetLocalMs = dotnetUtcMs + value.offsetMillis;
    writeI64(dotnetLocalMs);
    writeI64(value.offsetMillis);
    writeU8(0); // kind = 0 (offset)
  }

  void writeAll(Uint8List data) {
    _ensureCapacity(data.length);
    _buf.setRange(_pos, _pos + data.length, data);
    _pos += data.length;
  }

  Uint8List toBytes() {
    return Uint8List.fromList(_buf.sublist(0, _pos));
  }
}

// --- Binary Reader ---

class BaboonBinReader {
  final Uint8List _buf;
  final ByteData _view;
  int _pos;

  BaboonBinReader(Uint8List data)
      : _buf = data,
        _view = ByteData.view(data.buffer, data.offsetInBytes, data.length),
        _pos = 0;

  int get position => _pos;

  int readU8() {
    final v = _buf[_pos];
    _pos += 1;
    return v;
  }

  int readI8() {
    final v = _view.getInt8(_pos);
    _pos += 1;
    return v;
  }

  int readU16() {
    final v = _view.getUint16(_pos, Endian.little);
    _pos += 2;
    return v;
  }

  int readI16() {
    final v = _view.getInt16(_pos, Endian.little);
    _pos += 2;
    return v;
  }

  int readU32() {
    final v = _view.getUint32(_pos, Endian.little);
    _pos += 4;
    return v;
  }

  int readI32() {
    final v = _view.getInt32(_pos, Endian.little);
    _pos += 4;
    return v;
  }

  int readU64() {
    final v = _view.getUint64(_pos, Endian.little);
    _pos += 8;
    return v;
  }

  int readI64() {
    final v = _view.getInt64(_pos, Endian.little);
    _pos += 8;
    return v;
  }

  double readF32() {
    final v = _view.getFloat32(_pos, Endian.little);
    _pos += 4;
    return v;
  }

  double readF64() {
    final v = _view.getFloat64(_pos, Endian.little);
    _pos += 8;
    return v;
  }

  bool readBool() {
    return readU8() != 0;
  }

  String readString() {
    // 7-bit VLQ decoding for string length (compatible with .NET BinaryReader)
    var length = 0;
    var shift = 0;
    while (true) {
      final byteRead = readU8();
      length |= (byteRead & 0x7F) << shift;
      shift += 7;
      if ((byteRead & 0x80) == 0) break;
    }
    final bytes = _buf.sublist(_pos, _pos + length);
    _pos += length;
    return utf8.decode(bytes);
  }

  Uint8List readBytes() {
    final length = readI32();
    final bytes = _buf.sublist(_pos, _pos + length);
    _pos += length;
    return Uint8List.fromList(bytes);
  }

  /// Read exactly [count] raw bytes from the wire (no length prefix). Used by the `any`-field
  /// decoder helper to extract the meta-bytes window and the trailing blob within a pre-known
  /// length frame. PR 8.2 — needed to read the framed payload that already has its length on the
  /// wire as a separate i32.
  Uint8List readNBytes(int count) {
    final bytes = _buf.sublist(_pos, _pos + count);
    _pos += count;
    return Uint8List.fromList(bytes);
  }

  /// Skip [count] raw bytes without materialising them. Used by the `any`-field decoder helper to
  /// honour the meta-length window (forward-compat: ignore future meta-extension bytes that the
  /// current readBin doesn't understand). PR 8.2.
  void skipBytes(int count) {
    _pos += count;
  }

  BaboonDecimal readDecimal() {
    // .NET decimal format: lo (i32), mid (i32), hi (i32), flags (i32) = 16 bytes
    final lo = BigInt.from(_readRawU32());
    final mid = BigInt.from(_readRawU32());
    final hi = BigInt.from(_readRawU32());
    final flags = _readRawU32();

    final scale = (flags >> 16) & 0xFF;
    final isNeg = (flags & 0x80000000) != 0;

    final mantissa = lo | (mid << 32) | (hi << 64);
    var str = mantissa.toString();

    if (scale > 0) {
      while (str.length <= scale) {
        str = '0$str';
      }
      str = '${str.substring(0, str.length - scale)}.${str.substring(str.length - scale)}';
      // Remove trailing zeros after decimal point
      str = str.replaceAll(RegExp(r'\.?0+$'), '');
      if (str.isEmpty) str = '0';
    }

    if (isNeg && str != '0') {
      str = '-$str';
    }

    return BaboonDecimal(str);
  }

  int _readRawU32() {
    final v = _view.getUint32(_pos, Endian.little);
    _pos += 4;
    return v;
  }

  String readUuid() {
    final bytes = Uint8List.fromList(_buf.sublist(_pos, _pos + 16));
    _pos += 16;

    // .NET mixed-endian GUID format
    final hex = StringBuffer();
    hex.write(bytes[3].toRadixString(16).padLeft(2, '0'));
    hex.write(bytes[2].toRadixString(16).padLeft(2, '0'));
    hex.write(bytes[1].toRadixString(16).padLeft(2, '0'));
    hex.write(bytes[0].toRadixString(16).padLeft(2, '0'));
    hex.write('-');
    hex.write(bytes[5].toRadixString(16).padLeft(2, '0'));
    hex.write(bytes[4].toRadixString(16).padLeft(2, '0'));
    hex.write('-');
    hex.write(bytes[7].toRadixString(16).padLeft(2, '0'));
    hex.write(bytes[6].toRadixString(16).padLeft(2, '0'));
    hex.write('-');
    hex.write(bytes[8].toRadixString(16).padLeft(2, '0'));
    hex.write(bytes[9].toRadixString(16).padLeft(2, '0'));
    hex.write('-');
    for (var i = 10; i < 16; i++) {
      hex.write(bytes[i].toRadixString(16).padLeft(2, '0'));
    }
    return hex.toString();
  }

  DateTime readTsu() {
    final dotnetLocalMs = readI64();
    final offsetMs = readI64();
    final kind = readU8();
    assert(kind >= 0 && kind <= 2);
    final dotnetUtcMs = dotnetLocalMs - offsetMs;
    final epochMs = dotnetUtcMs - BaboonBinWriter._dotnetEpochOffsetMs;
    return DateTime.fromMillisecondsSinceEpoch(epochMs, isUtc: true);
  }

  BaboonDateTimeOffset readTso() {
    final dotnetLocalMs = readI64();
    final offsetMs = readI64();
    final kind = readU8();
    assert(kind >= 0 && kind <= 2);
    final dotnetUtcMs = dotnetLocalMs - offsetMs;
    final epochMs = dotnetUtcMs - BaboonBinWriter._dotnetEpochOffsetMs;
    return BaboonDateTimeOffset(
      epochMillis: epochMs,
      offsetMillis: offsetMs,
      kind: kind == 1 ? 'Utc' : 'Local',
    );
  }
}

// --- Binary Tools ---

class BaboonBinTools {
  static BaboonBinWriter createWriter([int initialCapacity = 256]) {
    return BaboonBinWriter(initialCapacity);
  }

  static BaboonBinReader createReader(Uint8List data) {
    return BaboonBinReader(data);
  }
}

// --- Time Formats ---

class BaboonTimeFormats {
  static String formatUtc(DateTime dt) {
    final utc = dt.toUtc();
    return utc.toIso8601String();
  }

  static String formatOffset(BaboonDateTimeOffset dto) {
    final dt = DateTime.fromMillisecondsSinceEpoch(dto.epochMillis, isUtc: true)
        .add(Duration(milliseconds: dto.offsetMillis));
    final base = _formatIso(dt);
    final sign = dto.offsetMillis >= 0 ? '+' : '-';
    final absOffset = dto.offsetMillis.abs();
    final hours = (absOffset ~/ 3600000).toString().padLeft(2, '0');
    final minutes = ((absOffset % 3600000) ~/ 60000).toString().padLeft(2, '0');
    return '$base$sign$hours:$minutes';
  }

  static DateTime parseUtc(String s) {
    return DateTime.parse(s).toUtc();
  }

  static BaboonDateTimeOffset parseOffset(String s) {
    final offsetPattern = RegExp(r'([+-])(\d{2}):(\d{2})$');
    final match = offsetPattern.firstMatch(s);
    if (match != null) {
      final sign = match.group(1) == '+' ? 1 : -1;
      final hours = int.parse(match.group(2)!);
      final minutes = int.parse(match.group(3)!);
      final offsetMillis = sign * (hours * 3600000 + minutes * 60000);

      final basePart = s.substring(0, match.start);
      // Force UTC interpretation to avoid local timezone interference
      final localDt = DateTime.parse('${basePart}Z');
      final epochMillis = localDt.millisecondsSinceEpoch - offsetMillis;

      return BaboonDateTimeOffset(
        epochMillis: epochMillis,
        offsetMillis: offsetMillis,
        kind: 'offset',
      );
    }
    if (s.endsWith('Z') || s.endsWith('z')) {
      final dt = DateTime.parse(s).toUtc();
      return BaboonDateTimeOffset(
        epochMillis: dt.millisecondsSinceEpoch,
        offsetMillis: 0,
        kind: 'utc',
      );
    }
    final dt = DateTime.parse(s);
    return BaboonDateTimeOffset(
      epochMillis: dt.millisecondsSinceEpoch,
      offsetMillis: 0,
      kind: 'local',
    );
  }

  static String _formatIso(DateTime dt) {
    final y = dt.year.toString().padLeft(4, '0');
    final m = dt.month.toString().padLeft(2, '0');
    final d = dt.day.toString().padLeft(2, '0');
    final h = dt.hour.toString().padLeft(2, '0');
    final min = dt.minute.toString().padLeft(2, '0');
    final sec = dt.second.toString().padLeft(2, '0');
    final ms = dt.millisecond.toString().padLeft(3, '0');
    return '$y-$m-${d}T$h:$min:$sec.${ms}';
  }
}

// --- Byte String Tools ---

class BaboonByteStringTools {
  static Uint8List fromHexString(String hex) {
    final length = hex.length ~/ 2;
    final result = Uint8List(length);
    for (var i = 0; i < length; i++) {
      result[i] = int.parse(hex.substring(i * 2, i * 2 + 2), radix: 16);
    }
    return result;
  }
}

extension BaboonByteStringExt on Uint8List {
  String toHexString() {
    return map((b) => b.toRadixString(16).padLeft(2, '0')).join();
  }
}

// --- Custom Types ---

class BaboonDecimal {
  final String value;

  const BaboonDecimal(this.value);

  static String _normalize(String s) {
    if (!s.contains('.')) return s;
    var result = s;
    while (result.endsWith('0')) {
      result = result.substring(0, result.length - 1);
    }
    if (result.endsWith('.')) result = result.substring(0, result.length - 1);
    return result;
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) || other is BaboonDecimal && _normalize(value) == _normalize(other.value);

  @override
  int get hashCode => _normalize(value).hashCode;

  @override
  String toString() => value;
}

class BaboonDateTimeOffset {
  final int epochMillis;
  final int offsetMillis;
  final String kind;

  const BaboonDateTimeOffset({
    required this.epochMillis,
    required this.offsetMillis,
    required this.kind,
  });

  DateTime toUtc() => DateTime.fromMillisecondsSinceEpoch(epochMillis, isUtc: true);

  DateTime toLocal() => DateTime.fromMillisecondsSinceEpoch(epochMillis, isUtc: true)
      .add(Duration(milliseconds: offsetMillis));

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is BaboonDateTimeOffset &&
          epochMillis == other.epochMillis &&
          offsetMillis == other.offsetMillis;

  @override
  int get hashCode => Object.hashAll([epochMillis, offsetMillis]);

  @override
  String toString() => BaboonTimeFormats.formatOffset(this);
}

// --- Conversions ---

abstract class AbstractConversion<F, T> {
  T doConvert(dynamic context, AbstractBaboonConversions conversions, F from);
  String get versionFrom;
  String get versionTo;
  String get typeId;
  String get toTypeId;
}

abstract class AbstractBaboonConversions {
  // Keyed (fromTypeId -> toTypeId -> conversion). Models with multiple distinct
  // conversions from a single source type would otherwise dispatch the wrong target
  // (PR-22-D05). `convertWithContext` honours both halves of the key.
  final Map<String, Map<String, AbstractConversion>> _registry = {};

  void register(AbstractConversion conversion) {
    final byTo = _registry.putIfAbsent(conversion.typeId, () => {});
    byTo[conversion.toTypeId] = conversion;
  }

  dynamic convertWithContext(dynamic context, dynamic from, String fromTypeId, String toTypeId) {
    final byTo = _registry[fromTypeId];
    if (byTo == null) {
      throw ArgumentError('No conversion registered for source typeId: $fromTypeId');
    }
    final conversion = byTo[toTypeId];
    if (conversion == null) {
      throw ArgumentError('No conversion from $fromTypeId to $toTypeId (source registered, target not)');
    }
    return conversion.doConvert(context, this, from);
  }

  List<String> get versionsFrom;
  String get versionTo;
}

/// Base for the per-domain-version codec registries. Generated `BaboonCodecs{Json,Ueba}` classes
/// extend [AbstractBaboonJsonCodecs] / [AbstractBaboonUebaCodecs] (which inherit this) and call
/// [register] for each codec on construction.
class AbstractBaboonCodecs {
  final Map<String, Lazy<BaboonCodecData>> _codecs = {};

  /// Codegen-friendly registration. The factory is wrapped in a [Lazy] so codec instantiation
  /// is deferred until the registry is first consulted via [tryFind] / [find].
  void register(String typeId, Function factory) {
    _codecs[typeId] = Lazy<BaboonCodecData>(() => factory() as BaboonCodecData);
  }

  /// Direct lazy registration — used by tests / advanced wiring that already produces a [Lazy].
  void registerLazy(String typeId, Lazy<BaboonCodecData> codec) {
    _codecs[typeId] = codec;
  }

  Lazy<BaboonCodecData>? tryFind(String typeId) => _codecs[typeId];

  Lazy<BaboonCodecData> find(String typeId) {
    final c = _codecs[typeId];
    if (c == null) throw StateError('Codec not found: $typeId');
    return c;
  }
}

class AbstractBaboonJsonCodecs extends AbstractBaboonCodecs {
  /// Legacy lookup — predates the facade port. Returns the JSON codec or null.
  BaboonJsonCodec? codecFor(String typeId) {
    final lazy = tryFind(typeId);
    return lazy?.value as BaboonJsonCodec?;
  }
}

class AbstractBaboonUebaCodecs extends AbstractBaboonCodecs {
  /// Legacy lookup — predates the facade port. Returns the binary codec or null.
  BaboonBinCodec? codecFor(String typeId) {
    final lazy = tryFind(typeId);
    return lazy?.value as BaboonBinCodec?;
  }
}

// --- Service Wiring ---

class BaboonMethodId {
  final String serviceId;
  final String methodName;

  const BaboonMethodId(this.serviceId, this.methodName);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is BaboonMethodId && serviceId == other.serviceId && methodName == other.methodName;

  @override
  int get hashCode => Object.hashAll([serviceId, methodName]);

  @override
  String toString() => '$serviceId.$methodName';
}

class BaboonWiringError {
  final String message;
  const BaboonWiringError(this.message);

  @override
  String toString() => 'BaboonWiringError: $message';
}

class BaboonWiringException implements Exception {
  final BaboonWiringError error;
  const BaboonWiringException(this.error);

  @override
  String toString() => error.toString();
}

// --- Either for Service Results ---

sealed class BaboonEither<L, R> {
  const BaboonEither();
}

class BaboonLeft<L, R> extends BaboonEither<L, R> {
  final L value;
  const BaboonLeft(this.value);

  @override
  bool operator ==(Object other) => other is BaboonLeft<L, R> && value == other.value;

  @override
  int get hashCode => value.hashCode;
}

class BaboonRight<L, R> extends BaboonEither<L, R> {
  final R value;
  const BaboonRight(this.value);

  @override
  bool operator ==(Object other) => other is BaboonRight<L, R> && value == other.value;

  @override
  int get hashCode => value.hashCode;
}

// --- Deep Equality for Collections ---

bool baboonDeepEquals(dynamic a, dynamic b) {
  if (identical(a, b)) return true;
  if (a is Map && b is Map) {
    if (a.length != b.length) return false;
    for (final key in a.keys) {
      if (!b.containsKey(key) || !baboonDeepEquals(a[key], b[key])) return false;
    }
    return true;
  }
  if (a is Set && b is Set) {
    if (a.length != b.length) return false;
    return a.containsAll(b);
  }
  if (a is List && b is List) {
    if (a.length != b.length) return false;
    for (var i = 0; i < a.length; i++) {
      if (!baboonDeepEquals(a[i], b[i])) return false;
    }
    return true;
  }
  return a == b;
}

// --- Lazy ---

/// Single-shot lazy initializer. Computes once on first [value] access and caches the result.
/// Mirrors Kotlin's `Lazy` semantics (the AtomicReference flavour used by other runtimes); Dart
/// is single-threaded per isolate so a plain field guard suffices.
class Lazy<T> {
  final T Function() _initializer;
  T? _value;
  bool _computed = false;

  Lazy(this._initializer);

  T get value {
    if (!_computed) {
      _value = _initializer();
      _computed = true;
    }
    return _value as T;
  }

  bool get isValueCreated => _computed;
}

// --- Exceptions ---

class BaboonException implements Exception {
  final String message;
  final Object? cause;
  const BaboonException(this.message, [this.cause]);

  @override
  String toString() => 'BaboonException: $message';
}

/// Sealed hierarchy mirroring Kotlin's `BaboonCodecException`. Codec/conversion failures thread
/// through `BaboonEither<BaboonCodecException, T>` per PR-04-D02 — JSON decode is user-facing
/// and never throws; binary decode trusts the wire and may throw.
sealed class BaboonCodecException implements Exception {
  final String message;
  final Object? cause;
  const BaboonCodecException(this.message, [this.cause]);

  @override
  String toString() => '$runtimeType: $message';
}

class BaboonEncoderFailure extends BaboonCodecException {
  const BaboonEncoderFailure(super.message, [super.cause]);
}

class BaboonDecoderFailure extends BaboonCodecException {
  const BaboonDecoderFailure(super.message, [super.cause]);
}

class BaboonConverterFailure extends BaboonCodecException {
  const BaboonConverterFailure(super.message, [super.cause]);
}

class BaboonCodecNotFound extends BaboonCodecException {
  const BaboonCodecNotFound(super.message);
}

class BaboonConversionNotFound extends BaboonCodecException {
  const BaboonConversionNotFound(super.message);
}

// --- Version / DomainVersion / TypeMeta ---

/// Per-version "what types are unchanged since me" lookup. Each generated `BaboonMetadata`
/// implementation provides this.
abstract class BaboonMeta {
  List<String> sameInVersions(String typeId);
}

/// Semver-shaped 3-tuple for `getCodec`'s version-window math. PR-19-D01 lesson: regex literals
/// in template files are read verbatim — escape `\d` once (single backslash); the embedSources
/// macro does NOT process Scala-string escapes on file bytes.
class BaboonVersion implements Comparable<BaboonVersion> {
  final int major;
  final int minor;
  final int patch;

  const BaboonVersion(this.major, this.minor, this.patch);

  static BaboonVersion from(String version) {
    final chunks = version.split('.');
    if (chunks.length != 3) {
      throw BaboonException('Expected to have version in format x.y.z, got $version');
    }
    final major = int.tryParse(chunks[0].trim());
    final minor = int.tryParse(chunks[1].trim());
    final patch = int.tryParse(chunks[2].trim());
    if (major == null) {
      throw BaboonException('Expected to have version in format x.y.z, got $version. Invalid major value.');
    }
    if (minor == null) {
      throw BaboonException('Expected to have version in format x.y.z, got $version. Invalid minor value.');
    }
    if (patch == null) {
      throw BaboonException('Expected to have version in format x.y.z, got $version. Invalid patch value.');
    }
    return BaboonVersion(major, minor, patch);
  }

  @override
  int compareTo(BaboonVersion other) {
    final c1 = major.compareTo(other.major);
    if (c1 != 0) return c1;
    final c2 = minor.compareTo(other.minor);
    if (c2 != 0) return c2;
    return patch.compareTo(other.patch);
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is BaboonVersion && major == other.major && minor == other.minor && patch == other.patch;

  @override
  int get hashCode => Object.hash(major, minor, patch);

  @override
  String toString() => '$major.$minor.$patch';
}

class BaboonDomainVersion {
  final String domainIdentifier;
  final String domainVersion;
  late final BaboonVersion _version = BaboonVersion.from(domainVersion);

  BaboonDomainVersion(this.domainIdentifier, this.domainVersion);

  BaboonVersion get version => _version;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is BaboonDomainVersion &&
          domainIdentifier == other.domainIdentifier &&
          domainVersion == other.domainVersion;

  @override
  int get hashCode => Object.hash(domainIdentifier, domainVersion);

  @override
  String toString() => '$domainIdentifier:$domainVersion';
}

/// On-wire type meta envelope. Mirrors Kotlin's `BaboonTypeMeta`.
class BaboonTypeMeta {
  final int metaVersion;
  final String domainIdentifier;
  final String domainVersion;
  final String domainVersionMinCompat;
  final String typeIdentifier;

  const BaboonTypeMeta(
    this.metaVersion,
    this.domainIdentifier,
    this.domainVersion,
    this.domainVersionMinCompat,
    this.typeIdentifier,
  );

  BaboonDomainVersion versionRef() => BaboonDomainVersion(domainIdentifier, domainVersion);

  BaboonDomainVersion? versionMinCompat() {
    if (domainVersionMinCompat.isEmpty) return null;
    if (domainVersionMinCompat == domainVersion) return null;
    return BaboonDomainVersion(domainIdentifier, domainVersionMinCompat);
  }

  void writeBin(BaboonBinWriter writer) {
    BaboonTypeMetaCodec.writeBin(this, writer);
  }

  Map<String, dynamic> writeJson() => BaboonTypeMetaCodec.writeJson(this);

  /// PR-08-D01: tolerate absent `\$mv` (treat as canonical) and reject explicit
  /// `\$mv != String(metaVersion)` (forward-compat: future meta versions return null so the
  /// facade reports a typed decode failure).
  static BaboonTypeMeta? readMetaJson(Object? json) {
    if (json is! Map) return null;
    final mv = json[r'$mv'];
    // PR-22-D01: spec says `$mv` is a JSON string (not number); Java/TS enforce string-only.
    // Tolerate absent (canonical by default) and string-form of canonical metaVersion.
    if (mv != null && mv != BaboonTypeMetaCodec.metaVersion.toString()) return null;
    final d = json[r'$d'];
    final v = json[r'$v'];
    final t = json[r'$t'];
    if (d is! String || v is! String || t is! String) return null;
    final uv = json[r'$uv'];
    final minCompat = (uv is String) ? uv : v;
    return BaboonTypeMeta(BaboonTypeMetaCodec.metaVersion, d, v, minCompat, t);
  }

  static BaboonTypeMeta? readMetaBin(BaboonBinReader reader) =>
      BaboonTypeMetaCodec.readMeta(reader);

  /// Build a meta from a generated value. Optionally use the ADT type identifier when encoding
  /// through an ADT-typed reference (PR-19-D02 — Dart, like TS, has no runtime generics so the
  /// caller must opt in).
  static BaboonTypeMeta from(BaboonGenerated value, {bool useAdtIdentifier = false}) {
    final meta = value as BaboonMetaProvider;
    final typeId = useAdtIdentifier && value is BaboonAdtMember
        ? (value as BaboonAdtMember).baboonAdtTypeIdentifier
        : meta.baboonTypeIdentifier;
    final sameIn = meta.baboonSameInVersions;
    // PR-08-D02 fail-fast: a generator emitting an empty `sameInVersions` is a bug; prefer a
    // typed throw at meta construction to "decoded null" later in the facade.
    if (sameIn.isEmpty) {
      throw BaboonException(
        'BaboonTypeMeta.from: empty baboonSameInVersions for type [${meta.baboonDomainIdentifier}.$typeId]',
      );
    }
    return BaboonTypeMeta(
      BaboonTypeMetaCodec.metaVersion,
      meta.baboonDomainIdentifier,
      meta.baboonDomainVersion,
      sameIn.first,
      typeId,
    );
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is BaboonTypeMeta &&
          metaVersion == other.metaVersion &&
          domainIdentifier == other.domainIdentifier &&
          domainVersion == other.domainVersion &&
          domainVersionMinCompat == other.domainVersionMinCompat &&
          typeIdentifier == other.typeIdentifier;

  @override
  int get hashCode => Object.hash(metaVersion, domainIdentifier, domainVersion, domainVersionMinCompat, typeIdentifier);

  @override
  String toString() => 'BaboonTypeMeta($domainIdentifier.$typeIdentifier@$domainVersion)';
}

class BaboonTypeMetaCodec {
  static const int metaVersion = 16;

  static void writeBin(BaboonTypeMeta meta, BaboonBinWriter writer) {
    writer.writeU8(metaVersion);
    writer.writeString(meta.domainIdentifier);
    writer.writeString(meta.domainVersion);
    if (meta.domainVersion == meta.domainVersionMinCompat) {
      writer.writeU8(0);
    } else {
      writer.writeU8(1);
      writer.writeString(meta.domainVersionMinCompat);
    }
    writer.writeString(meta.typeIdentifier);
  }

  static BaboonTypeMeta? readMeta(BaboonBinReader reader) {
    final v = reader.readU8();
    if (v == metaVersion) return _readMetaV1(reader);
    return null;
  }

  static BaboonTypeMeta _readMetaV1(BaboonBinReader reader) {
    final d = reader.readString();
    final dv = reader.readString();
    final hasMinCompat = reader.readU8();
    final mc = hasMinCompat == 1 ? reader.readString() : dv;
    final t = reader.readString();
    return BaboonTypeMeta(metaVersion, d, dv, mc, t);
  }

  static Map<String, dynamic> writeJson(BaboonTypeMeta meta) {
    // `$mv` elided for the canonical version (matches cs/java/rust/scala/ts/python writers).
    // Reader treats absent `$mv` as canonical.
    final obj = <String, dynamic>{
      r'$d': meta.domainIdentifier,
      r'$v': meta.domainVersion,
      r'$t': meta.typeIdentifier,
    };
    if (meta.domainVersion != meta.domainVersionMinCompat) {
      obj[r'$uv'] = meta.domainVersionMinCompat;
    }
    return obj;
  }
}

/// Shape exposed by every generated value. Dart-side generated DTOs already declare the three
/// `baboonDomain*`/`baboonType*` static-style instance accessors plus `baboonSameInVersions`
/// (added in PR 8.x). Cast through this typed surface keeps the facade-side meta extraction
/// clear at the call site rather than `dynamic` access.
abstract class BaboonMetaProvider {
  String get baboonDomainVersion;
  String get baboonDomainIdentifier;
  String get baboonTypeIdentifier;
  List<String> get baboonSameInVersions;
}

/// Implemented by generated ADT branches. Mirrors Kotlin's `BaboonAdtMemberMeta` for the
/// `useAdtIdentifier` path used when encoding through an ADT-typed reference (PR-19-D02).
abstract class BaboonAdtMember {
  String get baboonAdtTypeIdentifier;
}

int baboonDeepHashCode(dynamic value) {
  if (value == null) return 0;
  if (value is Map) {
    return Object.hashAllUnordered(
      value.entries.map((e) => Object.hashAll([baboonDeepHashCode(e.key), baboonDeepHashCode(e.value)])),
    );
  }
  if (value is Set) {
    return Object.hashAllUnordered(value.map((e) => baboonDeepHashCode(e)));
  }
  if (value is List) {
    return Object.hashAll(value.map((e) => baboonDeepHashCode(e)));
  }
  return value.hashCode;
}
