// Runtime helpers for the `id` toString / parseRepr machinery defined in
// docs/spec/identifier-repr.md. The Dart backend (PR-57d) uses these helpers
// from emitted code; conformance to the spec is the contract.
//
// Mirrors the JVM, Rust, Swift, and TS helpers in API and behaviour.
// Result type: `BaboonEither<L, R>` (matching project convention from
// `baboon_runtime.dart`). Errors are human-readable strings.

import 'dart:typed_data';

import 'baboon_runtime.dart';

// Defensive: numeric char-code constants so emitted source is robust against
// any future template-escape pass.
const int _bs = 0x5c; // '\\'
const int _hsh = 0x23; // '#'
const int _col = 0x3a; // ':'
const int _obr = 0x7b; // '{'
const int _cbr = 0x7d; // '}'

const String _bsCh = r'\';
const String _hshCh = '#';
const String _colCh = ':';

BaboonEither<String, T> _left<T>(String s) => BaboonLeft<String, T>(s);
BaboonEither<String, T> _right<T>(T t) => BaboonRight<String, T>(t);

class BaboonIdRepr {
  /// Backslash-escape the 5 metacharacters per spec §4.2.
  static String escapeStr(String s) {
    final buf = StringBuffer();
    for (var i = 0; i < s.length; i++) {
      final c = s.codeUnitAt(i);
      if (c == _bs || c == _hsh || c == _col || c == _obr || c == _cbr) {
        buf.write(_bsCh);
      }
      buf.write(s[i]);
    }
    return buf.toString();
  }

  /// Lowercase hex, no separators, per spec §3 / §4.4.
  static String bytesToHex(Uint8List bytes) {
    final buf = StringBuffer();
    for (var i = 0; i < bytes.length; i++) {
      final b = bytes[i];
      buf.write((b >> 4).toRadixString(16));
      buf.write((b & 0x0f).toRadixString(16));
    }
    return buf.toString();
  }

  /// Render a `tsu` per spec §3: RFC 3339 UTC, milliseconds, trailing `Z`,
  /// exactly 24 characters.
  static String tsuToString(DateTime dt) {
    final u = dt.toUtc();
    final y = u.year.toString().padLeft(4, '0');
    final m = u.month.toString().padLeft(2, '0');
    final d = u.day.toString().padLeft(2, '0');
    final h = u.hour.toString().padLeft(2, '0');
    final mi = u.minute.toString().padLeft(2, '0');
    final se = u.second.toString().padLeft(2, '0');
    final ms = u.millisecond.toString().padLeft(3, '0');
    return '$y-$m-${d}T$h:$mi:$se.${ms}Z';
  }

  /// Render a `tso` per spec §3: RFC 3339 with explicit `±HH:MM` offset,
  /// milliseconds, exactly 29 characters. NEVER emits `Z` shorthand.
  static String tsoToString(BaboonDateTimeOffset dto) {
    final localDt = DateTime.fromMillisecondsSinceEpoch(
            dto.epochMillis, isUtc: true)
        .add(Duration(milliseconds: dto.offsetMillis));
    final y = localDt.year.toString().padLeft(4, '0');
    final m = localDt.month.toString().padLeft(2, '0');
    final d = localDt.day.toString().padLeft(2, '0');
    final h = localDt.hour.toString().padLeft(2, '0');
    final mi = localDt.minute.toString().padLeft(2, '0');
    final se = localDt.second.toString().padLeft(2, '0');
    final ms = localDt.millisecond.toString().padLeft(3, '0');
    final off = dto.offsetMillis;
    final sign = off >= 0 ? '+' : '-';
    final absOff = off.abs();
    final oh = (absOff ~/ 3600000).toString().padLeft(2, '0');
    final om = ((absOff % 3600000) ~/ 60000).toString().padLeft(2, '0');
    return '$y-$m-${d}T$h:$mi:$se.$ms$sign$oh:$om';
  }

  /// Render an unsigned 64-bit value as decimal. Dart `int` is signed-64 on
  /// native and arbitrary-precision-on-overflow on JS (web). The project's
  /// UEBA codecs read u64 as a Dart `int`; for repr we want unsigned. The
  /// generated codecs always emit values in the documented range, but for
  /// values where the high bit is set on native (negative as `int`), we go
  /// via `BigInt.fromSigned`'s zero-extension to render the 2^64 form.
  static String u64ToString(int v) {
    if (v >= 0) {
      return v.toString();
    }
    // High bit set: zero-extend to 64 unsigned bits.
    final big = BigInt.from(v).toUnsigned(64);
    return big.toString();
  }

  /// Render a `bit` per spec §3 — exact lowercase ASCII.
  static String bitToString(bool b) {
    return b ? 'true' : 'false';
  }

  static BaboonEither<String, DateTime> parseTsuRepr(String s) {
    if (s.length != 24) {
      return _left('tsu repr must be 24 chars, got ${s.length}');
    }
    if (!s.endsWith('Z')) {
      return _left("tsu repr must end with 'Z', got: $s");
    }
    final re = RegExp(
        r'^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2})\.(\d{3})Z$');
    final m = re.firstMatch(s);
    if (m == null) {
      return _left('could not parse tsu: $s');
    }
    try {
      final dt = DateTime.utc(
        int.parse(m.group(1)!),
        int.parse(m.group(2)!),
        int.parse(m.group(3)!),
        int.parse(m.group(4)!),
        int.parse(m.group(5)!),
        int.parse(m.group(6)!),
        int.parse(m.group(7)!),
      );
      return _right(dt);
    } catch (_) {
      return _left('could not parse tsu: $s');
    }
  }

  static BaboonEither<String, BaboonDateTimeOffset> parseTsoRepr(String s) {
    if (s.length != 29) {
      return _left('tso repr must be 29 chars, got ${s.length}');
    }
    final re = RegExp(
        r'^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2})\.(\d{3})([+-])(\d{2}):(\d{2})$');
    final m = re.firstMatch(s);
    if (m == null) {
      return _left('could not parse tso: $s');
    }
    try {
      final localDt = DateTime.utc(
        int.parse(m.group(1)!),
        int.parse(m.group(2)!),
        int.parse(m.group(3)!),
        int.parse(m.group(4)!),
        int.parse(m.group(5)!),
        int.parse(m.group(6)!),
        int.parse(m.group(7)!),
      );
      final sign = m.group(8) == '+' ? 1 : -1;
      final oh = int.parse(m.group(9)!);
      final om = int.parse(m.group(10)!);
      final offsetMs = sign * (oh * 3600000 + om * 60000);
      final epochMs = localDt.millisecondsSinceEpoch - offsetMs;
      return _right(BaboonDateTimeOffset(
        epochMillis: epochMs,
        offsetMillis: offsetMs,
        kind: offsetMs == 0 ? 'utc' : 'offset',
      ));
    } catch (_) {
      return _left('could not parse tso: $s');
    }
  }

  /// Decode `bytes` from lowercase hex. Empty string is legal (empty bytes).
  static BaboonEither<String, Uint8List> parseBytesHex(String s) {
    if (s.isEmpty) {
      return _right(Uint8List(0));
    }
    if ((s.length & 1) != 0) {
      return _left('odd-length hex sequence: $s');
    }
    final out = Uint8List(s.length ~/ 2);
    for (var i = 0; i < s.length; i += 2) {
      final hi = _hexDigit(s.codeUnitAt(i));
      final lo = _hexDigit(s.codeUnitAt(i + 1));
      if (hi < 0 || lo < 0) {
        return _left('non-lowercase or non-hex character in: $s');
      }
      out[i ~/ 2] = (hi << 4) | lo;
    }
    return _right(out);
  }

  static int _hexDigit(int b) {
    if (b >= 0x30 && b <= 0x39) return b - 0x30;
    if (b >= 0x61 && b <= 0x66) return 10 + (b - 0x61);
    return -1;
  }

  static BaboonEither<String, bool> parseBit(String s) {
    if (s == 'true') return _right(true);
    if (s == 'false') return _right(false);
    return _left("expected 'true' or 'false' but found '$s'");
  }

  /// Lowercase canonical-form check for uid strings (spec §3 / §5.4).
  static bool isCanonicalUid(String s) {
    return RegExp(
            r'^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$')
        .hasMatch(s);
  }

  /// Validate header of an identifier-repr: `<simpleName>:<version>#`.
  static BaboonEither<String, void> parseHeader(
    BaboonIdReprCursor cursor,
    String expectedSimpleName,
    String expectedVersion,
  ) {
    final nameLit = cursor.readUntilStructural();
    if (nameLit != expectedSimpleName) {
      return _left("expected name '$expectedSimpleName' but found '$nameLit'");
    }
    final r1 = cursor.expect(_colCh);
    if (r1 is BaboonLeft<String, void>) return _left(r1.value);
    final verLit = cursor.readUntilStructural();
    if (verLit != expectedVersion) {
      return _left("expected version '$expectedVersion' but found '$verLit'");
    }
    return cursor.expect(_hshCh);
  }

  /// Validate field-name segment: `<expectedFieldName>:`.
  static BaboonEither<String, void> parseFieldName(
    BaboonIdReprCursor cursor,
    String expectedFieldName,
  ) {
    final name = cursor.readUntilStructural();
    if (name != expectedFieldName) {
      return _left(
          "expected field name '$expectedFieldName' but found '$name'");
    }
    return cursor.expect(_colCh);
  }
}

/// Cursor-based parser for parseRepr decoders. Schema-directed; the caller
/// (the emitted `<TypeName>Codec.parseRepr`) drives the field sequence.
class BaboonIdReprCursor {
  final String source;
  int _pos = 0;

  BaboonIdReprCursor(this.source);

  int get position => _pos;

  bool get atEnd => _pos >= source.length;

  BaboonEither<String, void> expect(String c) {
    if (_pos >= source.length) {
      return _left("expected '$c' at $_pos but reached end of input");
    }
    final ch = source[_pos];
    if (ch != c) {
      return _left("expected '$c' at $_pos but found '$ch'");
    }
    _pos += 1;
    return _right(null);
  }

  BaboonEither<String, void> expectLiteral(String lit) {
    if (_pos + lit.length > source.length) {
      return _left(
          "expected literal '$lit' at $_pos but reached end of input");
    }
    for (var i = 0; i < lit.length; i++) {
      if (source[_pos + i] != lit[i]) {
        return _left("expected literal '$lit' at $_pos");
      }
    }
    _pos += lit.length;
    return _right(null);
  }

  /// Read until the next bare metachar in `:#{}`. Backslash is NOT a stop
  /// here — see [readStrField] for str-field consumption with backslash
  /// handling.
  String readUntilStructural() {
    final start = _pos;
    while (_pos < source.length) {
      final c = source.codeUnitAt(_pos);
      if (c == _col || c == _hsh || c == _obr || c == _cbr) break;
      _pos += 1;
    }
    return source.substring(start, _pos);
  }

  /// Consume exactly n characters as a fixed-width lexeme (tsu/tso). The
  /// lexemes are pure ASCII so chars and code units coincide.
  BaboonEither<String, String> readFixed(int n) {
    if (_pos + n > source.length) {
      return _left(
          'expected $n chars at $_pos but only ${source.length - _pos} remain');
    }
    final out = source.substring(_pos, _pos + n);
    _pos += n;
    return _right(out);
  }

  /// Read a `str` field value with backslash-unescaping per spec §5.5.
  BaboonEither<String, String> readStrField() {
    final buf = StringBuffer();
    while (_pos < source.length) {
      final c = source.codeUnitAt(_pos);
      if (c == _col || c == _hsh || c == _obr || c == _cbr) {
        return _right(buf.toString());
      }
      if (c == _bs) {
        if (_pos + 1 >= source.length) {
          return _left('trailing backslash at $_pos');
        }
        final nxt = source.codeUnitAt(_pos + 1);
        if (nxt == _bs ||
            nxt == _hsh ||
            nxt == _col ||
            nxt == _obr ||
            nxt == _cbr) {
          buf.write(source[_pos + 1]);
          _pos += 2;
        } else {
          return _left('invalid escape at $_pos');
        }
      } else {
        buf.write(source[_pos]);
        _pos += 1;
      }
    }
    return _right(buf.toString());
  }
}
