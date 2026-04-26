// `any`-feature surface types for Dart. Mirrors C# `AnyOpaque.cs`, Java `BaboonAnyOpaque.java`,
// Kotlin `BaboonAnyOpaque.kt`, TypeScript `BaboonAnyOpaque.ts`. Container holds:
//   * `AnyMeta` — locked four-byte/six-kind meta envelope, with construction-time invariant checks
//   * `AnyOpaque` — Dart 3 sealed-class hierarchy (Ueba / Json) for `any`-typed payloads
//   * `AnyMetaCodec` — static helper for binary + JSON serialisation of the envelope
//
// Defects addressed:
//   PR-04-D01  reject reserved meta-kinds 0x04 / 0x05
//   PR-04-D02  `readJson` returns `BaboonEither` (user-facing) while `readBin` throws (wire-trust)
//   PR-05-D01  `readBinWithLength` reports bytes-consumed for forward-compat skip-trailer
//   PR-05-D08  `Uint8List` `==` is reference-based; `anyOpaqueEquals` does content compare
//   PR-12-D01  explicit u8 range check on kind so non-byte values can't slip through bit masks
//   PR-19-D01  Dart string literal `\$` is literal `$` — keys must be raw strings (`r'$ak'`) or
//              double-escaped to defeat dollar-interpolation. We use raw strings throughout.

import 'dart:typed_data';

import 'baboon_runtime.dart';

// --- AnyMeta -----------------------------------------------------------------------------------

/// Locked four-byte / six-kind meta envelope for `any`-typed payloads.
///
/// Invariants (enforced by the constructor):
///   - bit 2 (DOMAIN_BIT, 0x04) set <-> domain != null
///   - bit 1 (VERSION_BIT, 0x02) set <-> version != null
///   - bit 0 (TYPEID_BIT, 0x01) set <-> typeid != null
///   - kind in {0x00, 0x01, 0x02, 0x03, 0x06, 0x07} — 0x04 / 0x05 reserved (PR-04-D01)
///
/// Construction with a reserved or mismatched kind throws [ArgumentError].
class AnyMeta {
  final int kind;
  final String? domain;
  final String? version;
  final String? typeid;

  AnyMeta(this.kind, this.domain, this.version, this.typeid) {
    // PR-12-D01: Dart `int` is 64-bit; bit ops do not implicitly truncate. Range-check `kind`
    // to a u8 explicitly so a non-byte value (e.g. -1, 0x100) can't slip past the bit-mask
    // checks below.
    if (kind < 0 || kind > 0xFF) {
      throw ArgumentError.value(kind, 'kind', 'AnyMeta: kind must be a u8 byte 0x00..0xFF');
    }

    final domainBitSet = (kind & AnyMetaCodec.DOMAIN_BIT) != 0;
    if (domainBitSet != (domain != null)) {
      throw ArgumentError(
        'AnyMeta: domain presence (${domain != null}) does not match kind 0x${kind.toRadixString(16)} bit 2',
      );
    }
    final versionBitSet = (kind & AnyMetaCodec.VERSION_BIT) != 0;
    if (versionBitSet != (version != null)) {
      throw ArgumentError(
        'AnyMeta: version presence (${version != null}) does not match kind 0x${kind.toRadixString(16)} bit 1',
      );
    }
    final typeidBitSet = (kind & AnyMetaCodec.TYPEID_BIT) != 0;
    if (typeidBitSet != (typeid != null)) {
      throw ArgumentError(
        'AnyMeta: typeid presence (${typeid != null}) does not match kind 0x${kind.toRadixString(16)} bit 0',
      );
    }
    if (!AnyMetaCodec.VALID_KINDS.contains(kind)) {
      throw ArgumentError(
        'AnyMeta: reserved or invalid meta-kind byte: 0x${kind.toRadixString(16).padLeft(2, '0')}',
      );
    }
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is AnyMeta &&
          kind == other.kind &&
          domain == other.domain &&
          version == other.version &&
          typeid == other.typeid;

  @override
  int get hashCode => Object.hash(kind, domain, version, typeid);

  @override
  String toString() => 'AnyMeta(kind=0x${kind.toRadixString(16)}, domain=$domain, version=$version, typeid=$typeid)';
}

// --- AnyOpaque ---------------------------------------------------------------------------------

/// Language-surface ADT for `any`-typed fields. Carries a meta envelope plus a payload in either
/// binary (UEBA) or JSON form. Modelled as a Dart 3 `sealed class` so consumers can `switch (x)`
/// exhaustively over [AnyOpaqueUeba] / [AnyOpaqueJson].
sealed class AnyOpaque {
  AnyMeta get meta;
  const AnyOpaque();
}

/// UEBA-formed payload. PR-05-D08: `Uint8List` `==` is reference identity; we override
/// `==`/`hashCode` to compare bytes by content.
class AnyOpaqueUeba extends AnyOpaque {
  @override
  final AnyMeta meta;
  final Uint8List bytes;

  const AnyOpaqueUeba(this.meta, this.bytes);

  @override
  bool operator ==(Object other) {
    if (identical(this, other)) return true;
    if (other is! AnyOpaqueUeba) return false;
    if (meta != other.meta) return false;
    if (bytes.length != other.bytes.length) return false;
    for (var i = 0; i < bytes.length; i++) {
      if (bytes[i] != other.bytes[i]) return false;
    }
    return true;
  }

  @override
  int get hashCode {
    // Hash a fixed-size prefix so equality remains compatible without scanning huge payloads.
    var h = meta.hashCode;
    final n = bytes.length < 32 ? bytes.length : 32;
    for (var i = 0; i < n; i++) {
      h = 0x1fffffff & (h * 31 + bytes[i]);
    }
    return Object.hash(h, bytes.length);
  }

  @override
  String toString() => 'AnyOpaqueUeba(meta=$meta, bytes=Uint8List(${bytes.length}))';
}

/// JSON-formed payload. JSON content is compared via [baboonDeepEquals] (reused from the runtime)
/// — `Map`/`List` `==` in Dart is reference identity.
class AnyOpaqueJson extends AnyOpaque {
  @override
  final AnyMeta meta;
  final Object? json;

  const AnyOpaqueJson(this.meta, this.json);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is AnyOpaqueJson && meta == other.meta && baboonDeepEquals(json, other.json);

  @override
  int get hashCode => Object.hash(meta, baboonDeepHashCode(json));

  @override
  String toString() => 'AnyOpaqueJson(meta=$meta, json=$json)';
}

// --- AnyMetaCodec ------------------------------------------------------------------------------

/// Static helper for `AnyMeta` binary + JSON serialisation. Members keep PR-locked names: bit
/// masks ([DOMAIN_BIT] / [VERSION_BIT] / [TYPEID_BIT]) and JSON envelope keys (`$ak` / `$ad` /
/// `$av` / `$at` / `$c`) match every other runtime.
///
/// PR-19-D01 lesson applied: Dart string literals interpolate `$ident`; the JSON envelope keys
/// must be raw strings (`r'$ak'`) so the literal `$` survives. The `embedSources` macro reads
/// file bytes verbatim — what's in this file is what ships.
///
/// [readBin] / [writeBin] trust the wire and throw on bad input. [readJson] returns
/// `BaboonEither<BaboonCodecException, AnyMeta>` per PR-04-D02 — JSON decode is user-facing
/// and threads errors instead of throwing.
abstract class AnyMetaCodec {
  AnyMetaCodec._();

  static const int DOMAIN_BIT = 0x04;
  static const int VERSION_BIT = 0x02;
  static const int TYPEID_BIT = 0x01;

  static const String ANY_KIND_KEY = r'$ak';
  static const String ANY_DOMAIN_KEY = r'$ad';
  static const String ANY_VERSION_KEY = r'$av';
  static const String ANY_TYPEID_KEY = r'$at';
  static const String ANY_CONTENT_KEY = r'$c';

  static const Set<int> VALID_KINDS = {0x00, 0x01, 0x02, 0x03, 0x06, 0x07};

  static void writeBin(AnyMeta meta, BaboonBinWriter writer) {
    writer.writeU8(meta.kind & 0xFF);
    if (meta.domain != null) writer.writeString(meta.domain!);
    if (meta.version != null) writer.writeString(meta.version!);
    if (meta.typeid != null) writer.writeString(meta.typeid!);
  }

  static AnyMeta readBin(BaboonBinReader reader) {
    final kind = reader.readU8();
    final domain = (kind & DOMAIN_BIT) != 0 ? reader.readString() : null;
    final version = (kind & VERSION_BIT) != 0 ? reader.readString() : null;
    final typeid = (kind & TYPEID_BIT) != 0 ? reader.readString() : null;
    return AnyMeta(kind, domain, version, typeid);
  }

  /// PR-05-D01: read meta and report bytes consumed. Callers that know the on-wire `meta-length`
  /// window can skip any trailing bytes left in it — that's how the wire format keeps forward-
  /// compat with future meta extensions. We compute the byte-count via reader-position snapshot,
  /// mirroring TS / Kotlin.
  static (AnyMeta, int) readBinWithLength(BaboonBinReader reader) {
    final before = reader.position;
    final meta = readBin(reader);
    final after = reader.position;
    return (meta, after - before);
  }

  /// Always returns a plain `Map<String, dynamic>` (never null/list). The JSON encoder envelope
  /// build relies on this invariant — adding `$c` content into a non-object would silently lose
  /// the key (PR-08-D06 analog).
  static Map<String, dynamic> writeJson(AnyMeta meta) {
    final obj = <String, dynamic>{};
    obj[ANY_KIND_KEY] = meta.kind & 0xFF;
    if (meta.domain != null) obj[ANY_DOMAIN_KEY] = meta.domain;
    if (meta.version != null) obj[ANY_VERSION_KEY] = meta.version;
    if (meta.typeid != null) obj[ANY_TYPEID_KEY] = meta.typeid;
    return obj;
  }

  static BaboonEither<BaboonCodecException, AnyMeta> readJson(Object? json) {
    if (json is! Map) {
      return BaboonLeft(BaboonDecoderFailure(
        'AnyMetaCodec.readJson: expected JSON object, got ${json?.runtimeType ?? 'null'}',
      ));
    }
    final obj = json;

    final kindNode = obj[ANY_KIND_KEY];
    if (kindNode is! int) {
      return BaboonLeft(BaboonDecoderFailure(
        "AnyMetaCodec.readJson: missing or non-numeric '$ANY_KIND_KEY' field",
      ));
    }
    final kind = kindNode & 0xFF;

    final domainResult = _readOptString(obj, ANY_DOMAIN_KEY, kind, DOMAIN_BIT, 'domain');
    if (domainResult is BaboonLeft<BaboonCodecException, String?>) {
      return BaboonLeft(domainResult.value);
    }
    final domain = (domainResult as BaboonRight<BaboonCodecException, String?>).value;

    final versionResult = _readOptString(obj, ANY_VERSION_KEY, kind, VERSION_BIT, 'version');
    if (versionResult is BaboonLeft<BaboonCodecException, String?>) {
      return BaboonLeft(versionResult.value);
    }
    final version = (versionResult as BaboonRight<BaboonCodecException, String?>).value;

    final typeidResult = _readOptString(obj, ANY_TYPEID_KEY, kind, TYPEID_BIT, 'typeid');
    if (typeidResult is BaboonLeft<BaboonCodecException, String?>) {
      return BaboonLeft(typeidResult.value);
    }
    final typeid = (typeidResult as BaboonRight<BaboonCodecException, String?>).value;

    try {
      return BaboonRight(AnyMeta(kind, domain, version, typeid));
    } on ArgumentError catch (e) {
      return BaboonLeft(BaboonDecoderFailure(
        'AnyMetaCodec.readJson: invalid meta: ${e.message}',
      ));
    }
  }

  static BaboonEither<BaboonCodecException, String?> _readOptString(
    Map obj,
    String key,
    int kind,
    int bit,
    String name,
  ) {
    final present = (kind & bit) != 0;
    final token = obj[key];
    final value = token is String ? token : null;

    if (present && value != null) return BaboonRight(value);
    if (!present && value == null) return BaboonRight(null);
    if (present) {
      return BaboonLeft(BaboonDecoderFailure(
        "AnyMetaCodec.readJson: kind 0x${kind.toRadixString(16)} requires '$key' ($name) but it is missing",
      ));
    }
    return BaboonLeft(BaboonDecoderFailure(
      "AnyMetaCodec.readJson: kind 0x${kind.toRadixString(16)} forbids '$key' ($name) but it is present",
    ));
  }
}
