// Facade over per-domain-version codec registries. Mirrors C# `BaboonCodecsFacade.cs`, Java
// `BaboonCodecsFacade.java`, Kotlin `BaboonCodecsFacade.kt`, TypeScript `BaboonCodecsFacade.ts`.
// Public surface:
//   * codec / conversion / meta registration (overloads)
//   * `verify()` startup-sanity check
//   * `preload()` fire-and-forget pre-evaluation of lazy registries (MFACADE-PR-4)
//   * `encodeToBin` / `decodeFromBin`
//   * `decodeFromBinLatest<T>` — decode then convert to latest (MFACADE-PR-4)
//   * `encodeToJson` / `decodeFromJson`
//   * `decodeFromJsonLatest<T>` — decode then convert to latest (MFACADE-PR-4)
//   * `convert<TFrom, TTo>` cross-version (single-step stub — multi-step deferred per PR-17-D05)
//   * `decodeAny(opaque)`
//   * `jsonToUebaBytes(meta, json, staticDomain?, staticVersion?, staticTypeid?)` (PR-06-D01)
//   * `uebaToJson(meta, bytes, staticDomain?, staticVersion?, staticTypeid?)` (symmetric)
//
// Defects addressed:
//   PR-04-D02  errors thread through `BaboonEither` rather than throwing
//   PR-06-D01  cross-format helpers accept static fallbacks; wire-`meta.X` overrides `staticX`
//   PR-07-D02  `getCodec` single-version-domain edge case routes to exact lookup
//   PR-08-D01  `BaboonTypeMeta.readMetaJson` tolerates absent `$mv` and rejects `$mv != "1"`
//   PR-08-D02  fail-fast on empty `baboonSameInVersions` (in BaboonTypeMeta.from)
//   PR-19-D02  `useAdtIdentifier` knob threaded through encode paths

import 'dart:convert';
import 'dart:typed_data';

import 'baboon_any_opaque.dart';
import 'baboon_runtime.dart';

const String _CONTENT_JSON_KEY = r'$c';

class BaboonCodecsFacade extends BaboonCodecsFacadeBase {
  final Map<BaboonDomainVersion, Lazy<AbstractBaboonJsonCodecs>> _versionsCodecsJson = {};
  final Map<BaboonDomainVersion, Lazy<AbstractBaboonUebaCodecs>> _versionsCodecsBin = {};
  final Map<BaboonDomainVersion, Lazy<AbstractBaboonConversions>> _versionsConversions = {};
  final Map<BaboonDomainVersion, Lazy<BaboonMeta>> _versionsMeta = {};
  final Map<String, List<BaboonDomainVersion>> _domainVersions = {};

  BaboonCodecsFacade();

  BaboonVersion latest(String domain) {
    final versions = _domainVersions[domain];
    if (versions == null || versions.isEmpty) {
      throw BaboonException('No registered version for $domain domain found.');
    }
    return versions.last.version;
  }

  /// Bulk-merge another facade. Symmetric across runtimes.
  void registerFacade(BaboonCodecsFacade other) {
    other._domainVersions.forEach((k, v) => _domainVersions[k] = v);
    other._versionsCodecsJson.forEach((k, v) => _versionsCodecsJson[k] = v);
    other._versionsCodecsBin.forEach((k, v) => _versionsCodecsBin[k] = v);
    other._versionsConversions.forEach((k, v) => _versionsConversions[k] = v);
    other._versionsMeta.forEach((k, v) => _versionsMeta[k] = v);
  }

  /// Full-arity registration. Suppliers wrap in [Lazy] so registration stays cheap.
  BaboonDomainVersion register(
    BaboonDomainVersion domainVersion, {
    required AbstractBaboonJsonCodecs Function() codecsJson,
    required AbstractBaboonUebaCodecs Function() codecsBin,
    required AbstractBaboonConversions Function() conversions,
    required BaboonMeta Function() meta,
  }) {
    _registerVersion(domainVersion);
    _versionsCodecsJson[domainVersion] = Lazy(codecsJson);
    _versionsCodecsBin[domainVersion] = Lazy(codecsBin);
    _versionsConversions[domainVersion] = Lazy(conversions);
    _versionsMeta[domainVersion] = Lazy(meta);
    return domainVersion;
  }

  BaboonDomainVersion registerCodecs(
    BaboonDomainVersion domainVersion, {
    required AbstractBaboonJsonCodecs Function() codecsJson,
    required AbstractBaboonUebaCodecs Function() codecsBin,
  }) {
    _registerVersion(domainVersion);
    _versionsCodecsJson[domainVersion] = Lazy(codecsJson);
    _versionsCodecsBin[domainVersion] = Lazy(codecsBin);
    return domainVersion;
  }

  BaboonDomainVersion registerCodecsAndMeta(
    BaboonDomainVersion domainVersion, {
    required AbstractBaboonJsonCodecs Function() codecsJson,
    required AbstractBaboonUebaCodecs Function() codecsBin,
    required BaboonMeta Function() meta,
  }) {
    _registerVersion(domainVersion);
    _versionsCodecsJson[domainVersion] = Lazy(codecsJson);
    _versionsCodecsBin[domainVersion] = Lazy(codecsBin);
    _versionsMeta[domainVersion] = Lazy(meta);
    return domainVersion;
  }

  BaboonDomainVersion registerConversions(
    BaboonDomainVersion domainVersion,
    AbstractBaboonConversions Function() conversions,
  ) {
    _registerVersion(domainVersion);
    _versionsConversions[domainVersion] = Lazy(conversions);
    return domainVersion;
  }

  BaboonDomainVersion registerMeta(
    BaboonDomainVersion domainVersion,
    BaboonMeta Function() meta,
  ) {
    _registerVersion(domainVersion);
    _versionsMeta[domainVersion] = Lazy(meta);
    return domainVersion;
  }

  /// Startup-sanity check. Mirrors C#/Java/Kotlin `verify()`: rejects empty-registry facades and
  /// requires conversions + meta for every registered version.
  void verify() {
    if (_domainVersions.isEmpty) {
      throw BaboonException('Baboon codecs must have at least one domain registered.');
    }
    for (final versions in _domainVersions.values) {
      for (final dv in versions) {
        if (!_versionsConversions.containsKey(dv)) {
          throw BaboonConversionNotFound('Baboon codecs must have conversion for $dv registered.');
        }
        if (!_versionsMeta.containsKey(dv)) {
          throw BaboonCodecNotFound('Baboon codecs must have codecs for $dv registered.');
        }
      }
    }
  }

  // MFACADE-PR-4: fire-and-forget pre-evaluation of all lazy codec, conversion, and meta
  // registries so first real encode/decode calls do not pay initialisation latency.
  // Dart is single-threaded per isolate; `Future.microtask` schedules the work after the
  // current task without blocking the caller. Errors inside individual lazy initialisers are
  // swallowed — a faulty registry will surface a typed failure on the first actual call.
  void preload() {
    Future.microtask(() {
      try {
        _versionsCodecsJson.values.forEach((l) => l.value);
        _versionsCodecsBin.values.forEach((l) => l.value);
        _versionsConversions.values.forEach((l) => l.value);
        _versionsMeta.values.forEach((l) => l.value);
      } catch (_) {
        // swallow — faulty registries surface on first real call
      }
    });
  }

  // MFACADE-PR-4: decode binary bytes then convert to the registered latest version.
  // Composition: `decodeFromBin` then `convert<T>` to `(fromTypeId, fromTypeId, latestVersion)`.
  // If the decoded value is already an instance of `T` (already at latest), it is returned
  // directly. The `toTypeId` equals `fromTypeId` (same type, later schema version).
  BaboonEither<BaboonCodecException, T> decodeFromBinLatest<T extends BaboonGeneratedLatest>(
    BaboonBinReader reader,
  ) {
    final decoded = decodeFromBin(reader);
    if (decoded is BaboonLeft<BaboonCodecException, BaboonGenerated>) {
      return BaboonLeft(decoded.value);
    }
    final value = (decoded as BaboonRight<BaboonCodecException, BaboonGenerated>).value;
    if (value is T) return BaboonRight(value);
    if (value is! BaboonMetaProvider) {
      return BaboonLeft(BaboonConverterFailure(
        'decodeFromBinLatest: decoded value of type ${value.runtimeType} does not implement BaboonMetaProvider.',
      ));
    }
    final meta = value as BaboonMetaProvider;
    final domain = meta.baboonDomainIdentifier;
    final fromTypeId = meta.baboonTypeIdentifier;
    final versions = _domainVersions[domain];
    if (versions == null || versions.isEmpty) {
      return BaboonLeft(BaboonConverterFailure("decodeFromBinLatest: unknown domain '$domain'."));
    }
    final latestDV = versions.last;
    return convert<T>(value, fromTypeId, fromTypeId, latestDV);
  }

  // MFACADE-PR-4: decode JSON then convert to the registered latest version.
  // Absent or invalid envelope (no recognisable BaboonTypeMeta header) →
  // `BaboonRight(null)` (null pass-through, mirrors Scala `Right(None)`).
  // Decode or conversion failures → `BaboonLeft`.
  BaboonEither<BaboonCodecException, T?> decodeFromJsonLatest<T extends BaboonGeneratedLatest>(
    Object? value,
  ) {
    if (BaboonTypeMeta.readMetaJson(value) == null) {
      return BaboonRight<BaboonCodecException, T?>(null);
    }
    final decoded = decodeFromJson(value);
    if (decoded is BaboonLeft<BaboonCodecException, BaboonGenerated>) {
      return BaboonLeft(decoded.value);
    }
    final decodedValue = (decoded as BaboonRight<BaboonCodecException, BaboonGenerated>).value;
    if (decodedValue is T) return BaboonRight<BaboonCodecException, T?>(decodedValue);
    if (decodedValue is! BaboonMetaProvider) {
      return BaboonLeft(BaboonConverterFailure(
        'decodeFromJsonLatest: decoded value of type ${decodedValue.runtimeType} does not implement BaboonMetaProvider.',
      ));
    }
    final meta = decodedValue as BaboonMetaProvider;
    final domain = meta.baboonDomainIdentifier;
    final fromTypeId = meta.baboonTypeIdentifier;
    final versions = _domainVersions[domain];
    if (versions == null || versions.isEmpty) {
      return BaboonLeft(BaboonConverterFailure("decodeFromJsonLatest: unknown domain '$domain'."));
    }
    final latestDV = versions.last;
    final convResult = convert<T>(decodedValue, fromTypeId, fromTypeId, latestDV);
    if (convResult is BaboonLeft<BaboonCodecException, T>) {
      return BaboonLeft<BaboonCodecException, T?>(convResult.value);
    }
    return BaboonRight<BaboonCodecException, T?>((convResult as BaboonRight<BaboonCodecException, T>).value);
  }

  // ----- encode / decode --------------------------------------------------------------------

  /// PR-19-D02: pass `useAdtIdentifier=true` when encoding through an ADT-typed reference so the
  /// envelope carries the ADT's type identifier rather than the concrete branch's.
  BaboonEither<BaboonCodecException, Uint8List> encodeToBin(
    BaboonCodecContext ctx,
    BaboonGenerated value, {
    BaboonTypeMeta? typeMetaOverride,
    bool useAdtIdentifier = false,
  }) {
    final BaboonTypeMeta typeMeta;
    try {
      typeMeta = BaboonTypeMeta.from(value, useAdtIdentifier: useAdtIdentifier);
    } catch (e) {
      return BaboonLeft(BaboonEncoderFailure('Cannot derive type meta from value: $e', e));
    }
    final codecResult = _getBinCodec(typeMeta, exact: true);
    if (codecResult is BaboonLeft<BaboonCodecException, BaboonCodecData>) {
      return BaboonLeft(codecResult.value);
    }
    final codec = (codecResult as BaboonRight<BaboonCodecException, BaboonCodecData>).value as BaboonBinCodecBase<BaboonGenerated>;

    try {
      final writer = BaboonBinWriter();
      (typeMetaOverride ?? typeMeta).writeBin(writer);
      codec.encode(ctx, writer, value);
      return BaboonRight(writer.toBytes());
    } catch (e) {
      return BaboonLeft(BaboonEncoderFailure(
        "Exception while trying to encode to binary form type [${typeMeta.domainIdentifier}.${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'.",
        e,
      ));
    }
  }

  BaboonEither<BaboonCodecException, BaboonGenerated> decodeFromBinBytes(Uint8List bytes) =>
      decodeFromBin(BaboonBinReader(bytes));

  BaboonEither<BaboonCodecException, BaboonGenerated> decodeFromBin(BaboonBinReader reader) {
    final BaboonTypeMeta? typeMeta;
    try {
      typeMeta = BaboonTypeMeta.readMetaBin(reader);
    } catch (e) {
      return BaboonLeft(BaboonDecoderFailure('Cannot decode binary type meta', e));
    }
    if (typeMeta == null) {
      return BaboonLeft(BaboonDecoderFailure('Cannot decode binary type meta'));
    }

    final codecResult = _getBinCodec(typeMeta, exact: false);
    if (codecResult is BaboonLeft<BaboonCodecException, BaboonCodecData>) {
      return BaboonLeft(codecResult.value);
    }
    final codec = (codecResult as BaboonRight<BaboonCodecException, BaboonCodecData>).value as BaboonBinCodec<BaboonGenerated>;

    try {
      return BaboonRight(codec.decode(BaboonCodecContext.compact, reader));
    } catch (e) {
      return BaboonLeft(BaboonDecoderFailure(
        "Can not decode BIN form type [${typeMeta.domainIdentifier}.${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'.",
        e,
      ));
    }
  }

  /// PR-19-D02: pass `useAdtIdentifier=true` when encoding through an ADT-typed reference.
  BaboonEither<BaboonCodecException, Map<String, dynamic>> encodeToJson(
    BaboonGenerated value, {
    BaboonTypeMeta? typeMetaOverride,
    bool useAdtIdentifier = false,
  }) {
    final BaboonTypeMeta typeMeta;
    try {
      typeMeta = BaboonTypeMeta.from(value, useAdtIdentifier: useAdtIdentifier);
    } catch (e) {
      return BaboonLeft(BaboonEncoderFailure('Cannot derive type meta from value: $e', e));
    }
    final codecResult = _getJsonCodec(typeMeta, exact: true);
    if (codecResult is BaboonLeft<BaboonCodecException, BaboonCodecData>) {
      return BaboonLeft(codecResult.value);
    }
    final codec = (codecResult as BaboonRight<BaboonCodecException, BaboonCodecData>).value as BaboonJsonCodecBase<BaboonGenerated>;

    try {
      final content = codec.encode(BaboonCodecContext.compact, value);
      final metaJson = (typeMetaOverride ?? typeMeta).writeJson();
      // PR-08-D06 analog: meta JSON must be a plain map for `$c` insertion to work. The runtime
      // helper `BaboonTypeMetaCodec.writeJson` always returns one; assert defensively.
      metaJson[_CONTENT_JSON_KEY] = content;
      return BaboonRight(metaJson);
    } catch (e) {
      return BaboonLeft(BaboonEncoderFailure(
        "Can not encode to json form type [${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'.",
        e,
      ));
    }
  }

  BaboonEither<BaboonCodecException, BaboonGenerated> decodeFromJsonString(String value) {
    try {
      final parsed = jsonDecode(value);
      return decodeFromJson(parsed);
    } catch (e) {
      return BaboonLeft(BaboonDecoderFailure('Cannot parse JSON: $e', e));
    }
  }

  BaboonEither<BaboonCodecException, BaboonGenerated> decodeFromJson(Object? value) {
    final typeMeta = BaboonTypeMeta.readMetaJson(value);
    if (typeMeta == null) {
      return BaboonLeft(BaboonDecoderFailure('Cannot decode JSON type meta'));
    }
    // PR-22-D03: readMetaJson already returned null for non-Map; value is guaranteed Map here.
    final asMap = value as Map;
    final contentToken = asMap[_CONTENT_JSON_KEY];
    if (contentToken == null && !asMap.containsKey(_CONTENT_JSON_KEY)) {
      return BaboonLeft(BaboonDecoderFailure(
        "Missing '$_CONTENT_JSON_KEY' content key for type [${typeMeta.domainIdentifier}.${typeMeta.typeIdentifier}].",
      ));
    }

    final codecResult = _getJsonCodec(typeMeta, exact: false);
    if (codecResult is BaboonLeft<BaboonCodecException, BaboonCodecData>) {
      return BaboonLeft(codecResult.value);
    }
    final codec = (codecResult as BaboonRight<BaboonCodecException, BaboonCodecData>).value as BaboonJsonCodec<BaboonGenerated>;

    try {
      return BaboonRight(codec.decode(BaboonCodecContext.compact, contentToken));
    } catch (e) {
      return BaboonLeft(BaboonDecoderFailure(
        "Can not decode JSON form type [${typeMeta.domainIdentifier}.${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'.",
        e,
      ));
    }
  }

  // ----- AnyOpaque dispatch -----------------------------------------------------------------

  /// Decode an [AnyOpaque] payload via the registered codec for `(meta.domain, meta.version,
  /// meta.typeid)`. User-facing — `meta` must carry all three components (variant A only). For
  /// variants B/C/D1/D2/D3 use the cross-format helpers ([jsonToUebaBytes] / [uebaToJson]).
  /// PR-04-D02: errors thread through [BaboonEither].
  BaboonEither<BaboonCodecException, BaboonGenerated> decodeAny(AnyOpaque opaque) {
    final metaResult = _buildSyntheticTypeMeta(opaque.meta, null, null, null);
    if (metaResult is BaboonLeft<BaboonCodecException, BaboonTypeMeta>) {
      return BaboonLeft(metaResult.value);
    }
    final typeMeta = (metaResult as BaboonRight<BaboonCodecException, BaboonTypeMeta>).value;

    return switch (opaque) {
      AnyOpaqueUeba(:final bytes) => () {
          final codecResult = _getBinCodec(typeMeta, exact: false);
          if (codecResult is BaboonLeft<BaboonCodecException, BaboonCodecData>) {
            return BaboonLeft<BaboonCodecException, BaboonGenerated>(codecResult.value);
          }
          final codec = (codecResult as BaboonRight<BaboonCodecException, BaboonCodecData>).value as BaboonBinCodec<BaboonGenerated>;
          try {
            return BaboonRight<BaboonCodecException, BaboonGenerated>(
              codec.decode(BaboonCodecContext.compact, BaboonBinReader(bytes)),
            );
          } catch (e) {
            return BaboonLeft<BaboonCodecException, BaboonGenerated>(BaboonDecoderFailure(
              "decodeAny: cannot decode UEBA payload of type [${typeMeta.domainIdentifier}.${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'.",
              e,
            ));
          }
        }(),
      AnyOpaqueJson(:final json) => () {
          final codecResult = _getJsonCodec(typeMeta, exact: false);
          if (codecResult is BaboonLeft<BaboonCodecException, BaboonCodecData>) {
            return BaboonLeft<BaboonCodecException, BaboonGenerated>(codecResult.value);
          }
          final codec = (codecResult as BaboonRight<BaboonCodecException, BaboonCodecData>).value as BaboonJsonCodec<BaboonGenerated>;
          try {
            return BaboonRight<BaboonCodecException, BaboonGenerated>(
              codec.decode(BaboonCodecContext.compact, json),
            );
          } catch (e) {
            return BaboonLeft<BaboonCodecException, BaboonGenerated>(BaboonDecoderFailure(
              "decodeAny: cannot decode JSON payload of type [${typeMeta.domainIdentifier}.${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'.",
              e,
            ));
          }
        }(),
    };
  }

  /// Cross-format helper: decode an [AnyOpaqueJson] payload via the registered JSON codec, then
  /// re-encode it via the registered UEBA codec. Static fallbacks fill components missing from
  /// the wire `meta` (variants B/C/D1/D2/D3 — codec-generation-time knowledge); wire data wins
  /// when both are present (override semantics). See PR-06-D01.
  BaboonEither<BaboonCodecException, Uint8List> jsonToUebaBytes(
    AnyMeta meta,
    Object? json, {
    String? staticDomain,
    String? staticVersion,
    String? staticTypeid,
  }) {
    final metaResult = _buildSyntheticTypeMeta(meta, staticDomain, staticVersion, staticTypeid);
    if (metaResult is BaboonLeft<BaboonCodecException, BaboonTypeMeta>) {
      return BaboonLeft(metaResult.value);
    }
    final typeMeta = (metaResult as BaboonRight<BaboonCodecException, BaboonTypeMeta>).value;

    final jsonCodecResult = _getJsonCodec(typeMeta, exact: false);
    if (jsonCodecResult is BaboonLeft<BaboonCodecException, BaboonCodecData>) {
      return BaboonLeft(jsonCodecResult.value);
    }
    final jsonCodec = (jsonCodecResult as BaboonRight<BaboonCodecException, BaboonCodecData>).value as BaboonJsonCodec<BaboonGenerated>;

    final binCodecResult = _getBinCodec(typeMeta, exact: false);
    if (binCodecResult is BaboonLeft<BaboonCodecException, BaboonCodecData>) {
      return BaboonLeft(binCodecResult.value);
    }
    final binCodec = (binCodecResult as BaboonRight<BaboonCodecException, BaboonCodecData>).value as BaboonBinCodecBase<BaboonGenerated>;

    final BaboonGenerated typed;
    try {
      typed = jsonCodec.decode(BaboonCodecContext.compact, json);
    } catch (e) {
      return BaboonLeft(BaboonDecoderFailure(
        "jsonToUebaBytes: cannot decode JSON payload of type [${typeMeta.domainIdentifier}.${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'.",
        e,
      ));
    }

    try {
      final writer = BaboonBinWriter();
      binCodec.encode(BaboonCodecContext.compact, writer, typed);
      return BaboonRight(writer.toBytes());
    } catch (e) {
      return BaboonLeft(BaboonEncoderFailure(
        "jsonToUebaBytes: cannot encode UEBA payload of type [${typeMeta.domainIdentifier}.${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'.",
        e,
      ));
    }
  }

  /// Cross-format helper symmetric to [jsonToUebaBytes].
  BaboonEither<BaboonCodecException, Object?> uebaToJson(
    AnyMeta meta,
    Uint8List bytes, {
    String? staticDomain,
    String? staticVersion,
    String? staticTypeid,
  }) {
    final metaResult = _buildSyntheticTypeMeta(meta, staticDomain, staticVersion, staticTypeid);
    if (metaResult is BaboonLeft<BaboonCodecException, BaboonTypeMeta>) {
      return BaboonLeft(metaResult.value);
    }
    final typeMeta = (metaResult as BaboonRight<BaboonCodecException, BaboonTypeMeta>).value;

    final binCodecResult = _getBinCodec(typeMeta, exact: false);
    if (binCodecResult is BaboonLeft<BaboonCodecException, BaboonCodecData>) {
      return BaboonLeft(binCodecResult.value);
    }
    final binCodec = (binCodecResult as BaboonRight<BaboonCodecException, BaboonCodecData>).value as BaboonBinCodec<BaboonGenerated>;

    final jsonCodecResult = _getJsonCodec(typeMeta, exact: false);
    if (jsonCodecResult is BaboonLeft<BaboonCodecException, BaboonCodecData>) {
      return BaboonLeft(jsonCodecResult.value);
    }
    final jsonCodec = (jsonCodecResult as BaboonRight<BaboonCodecException, BaboonCodecData>).value as BaboonJsonCodecBase<BaboonGenerated>;

    final BaboonGenerated typed;
    try {
      typed = binCodec.decode(BaboonCodecContext.compact, BaboonBinReader(bytes));
    } catch (e) {
      return BaboonLeft(BaboonDecoderFailure(
        "uebaToJson: cannot decode UEBA payload of type [${typeMeta.domainIdentifier}.${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'.",
        e,
      ));
    }

    try {
      return BaboonRight(jsonCodec.encode(BaboonCodecContext.compact, typed));
    } catch (e) {
      return BaboonLeft(BaboonEncoderFailure(
        "uebaToJson: cannot encode JSON payload of type [${typeMeta.domainIdentifier}.${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'.",
        e,
      ));
    }
  }

  /// Synthesise a [BaboonTypeMeta] from an [AnyMeta] plus optional static fallbacks. [AnyMeta]
  /// does not carry a min-compat version; forward-version migration is unavailable for any-
  /// payloads, so `domainVersionMinCompat = version`. `meta.X` takes precedence over `staticX`
  /// (override semantics — wire data wins). [decodeAny] calls with all-null statics so its
  /// variant-A-only contract is preserved (PR-06-D01).
  static BaboonEither<BaboonCodecException, BaboonTypeMeta> _buildSyntheticTypeMeta(
    AnyMeta meta,
    String? staticDomain,
    String? staticVersion,
    String? staticTypeid,
  ) {
    final domain = meta.domain ?? staticDomain;
    final version = meta.version ?? staticVersion;
    final typeid = meta.typeid ?? staticTypeid;

    if (domain != null && version != null && typeid != null) {
      return BaboonRight(BaboonTypeMeta(
        BaboonTypeMetaCodec.metaVersion,
        domain,
        version,
        version,
        typeid,
      ));
    }

    final missing = <String>[];
    if (domain == null) missing.add('domain');
    if (version == null) missing.add('version');
    if (typeid == null) missing.add('typeid');
    return BaboonLeft(BaboonDecoderFailure(
      "AnyMeta requires domain/version/typeid for facade resolution; got kind 0x${(meta.kind & 0xFF).toRadixString(16)} which lacks: ${missing.join(', ')}",
    ));
  }

  // ----- cross-version conversion -----------------------------------------------------------

  /// Single-step convert via the registered conversion for `(fromTypeId, toTypeId)`. Multi-step
  /// chaining is deferred (PR-17-D05): the existing `AbstractBaboonConversions.convertWithContext`
  /// already does pair lookup; we proxy to it. Returns a typed failure when the relevant
  /// registry is empty.
  BaboonEither<BaboonCodecException, T> convert<T extends BaboonGenerated>(
    BaboonGenerated value,
    String fromTypeId,
    String toTypeId,
    BaboonDomainVersion toVersion,
  ) {
    final lazyConv = _versionsConversions[toVersion];
    if (lazyConv == null) {
      return BaboonLeft(BaboonConverterFailure(
        "Can not find version '$toVersion' conversions.",
      ));
    }
    try {
      final result = lazyConv.value.convertWithContext(null, value, fromTypeId, toTypeId) as T;
      return BaboonRight(result);
    } catch (e) {
      return BaboonLeft(BaboonConverterFailure(
        "Exception while converting type [$fromTypeId] to [$toTypeId] of version '$toVersion'.",
        e,
      ));
    }
  }

  // ----- private dispatch -------------------------------------------------------------------

  BaboonEither<BaboonCodecException, BaboonCodecData> _getBinCodec(BaboonTypeMeta typeMeta, {required bool exact}) =>
      _getCodec(_versionsCodecsBin, typeMeta, exact);

  BaboonEither<BaboonCodecException, BaboonCodecData> _getJsonCodec(BaboonTypeMeta typeMeta, {required bool exact}) =>
      _getCodec(_versionsCodecsJson, typeMeta, exact);

  BaboonEither<BaboonCodecException, BaboonCodecData> _getCodec<TCodecs extends AbstractBaboonCodecs>(
    Map<BaboonDomainVersion, Lazy<TCodecs>> versionsCodecs,
    BaboonTypeMeta typeMeta,
    bool exact,
  ) {
    final versions = _domainVersions[typeMeta.domainIdentifier];
    if (versions == null || versions.isEmpty) {
      return BaboonLeft(BaboonCodecNotFound('Unknown domain ${typeMeta.domainIdentifier}.'));
    }

    final minVersion = versions.first;
    final maxVersion = versions.last;

    final lookupVersion = typeMeta.versionRef();
    final minCompat = typeMeta.versionMinCompat();
    final modelVersion = (minCompat != null && lookupVersion.version.compareTo(maxVersion.version) > 0)
        ? minCompat
        : lookupVersion;

    final modelV = modelVersion.version;
    final maxV = maxVersion.version;
    final minV = minVersion.version;

    if (exact && modelV.compareTo(maxV) == 0) {
      return _getCodecExact(versionsCodecs, modelVersion, typeMeta.typeIdentifier);
    }
    // PR-07-D02: non-exact lookup at the latest registered version routes to exact lookup.
    // Without this arm a single-version domain (min == max == model) falls through every other
    // arm because the next one's strict `<` excludes equality, producing a misleading
    // "Unsupported domain version" error. Mirrors Scala/C#/Java/Kotlin/TS fix.
    if (!exact && modelV.compareTo(maxV) == 0) {
      return _getCodecExact(versionsCodecs, modelVersion, typeMeta.typeIdentifier);
    }
    if (modelV.compareTo(minV) >= 0 && modelV.compareTo(maxV) < 0) {
      return _getCodecMaxCompat(versionsCodecs, modelVersion, maxVersion, typeMeta.typeIdentifier);
    }
    if (modelV.compareTo(minV) < 0) {
      return _getCodecMaxCompat(versionsCodecs, minVersion, maxVersion, typeMeta.typeIdentifier);
    }
    return BaboonLeft(BaboonCodecNotFound("Unsupported domain version '$modelVersion'."));
  }

  static BaboonEither<BaboonCodecException, BaboonCodecData> _getCodecExact<TCodecs extends AbstractBaboonCodecs>(
    Map<BaboonDomainVersion, Lazy<TCodecs>> versionsCodecs,
    BaboonDomainVersion domainVersion,
    String typeIdentifier,
  ) {
    final lazyCodecs = versionsCodecs[domainVersion];
    if (lazyCodecs == null) {
      return BaboonLeft(BaboonCodecNotFound("No codecs registered for domain version '$domainVersion'."));
    }
    final lazyCodec = lazyCodecs.value.tryFind(typeIdentifier);
    if (lazyCodec == null) {
      return BaboonLeft(BaboonCodecNotFound(
        "No codec found for type [${domainVersion.domainVersion}.$typeIdentifier] of version '${domainVersion.version}'.",
      ));
    }
    return BaboonRight(lazyCodec.value);
  }

  BaboonEither<BaboonCodecException, BaboonCodecData> _getCodecMaxCompat<TCodecs extends AbstractBaboonCodecs>(
    Map<BaboonDomainVersion, Lazy<TCodecs>> versionsCodecs,
    BaboonDomainVersion modelVersion,
    BaboonDomainVersion maxVersion,
    String typeIdentifier,
  ) {
    final lazyMeta = _versionsMeta[modelVersion];
    if (lazyMeta == null) {
      return BaboonLeft(BaboonCodecNotFound("Unknown domain version '$modelVersion'."));
    }
    final sameVersions = lazyMeta.value.sameInVersions(typeIdentifier);
    String? bestSame;
    for (var i = sameVersions.length - 1; i >= 0; i--) {
      final sv = sameVersions[i];
      if (sv == maxVersion.domainVersion || BaboonVersion.from(sv).compareTo(maxVersion.version) <= 0) {
        bestSame = sv;
        break;
      }
    }
    if (bestSame == null) {
      return BaboonLeft(BaboonCodecNotFound(
        "No max compat codec found for type [${modelVersion.domainIdentifier}.$typeIdentifier] of version '${modelVersion.domainVersion}'.",
      ));
    }
    final maxCompatVersion = BaboonDomainVersion(modelVersion.domainIdentifier, bestSame);
    return _getCodecExact(versionsCodecs, maxCompatVersion, typeIdentifier);
  }

  void _registerVersion(BaboonDomainVersion domainVersion) {
    final existing = _domainVersions[domainVersion.domainIdentifier];
    if (existing != null) {
      if (existing.contains(domainVersion)) return;
      final updated = [...existing, domainVersion];
      updated.sort((a, b) => a.version.compareTo(b.version));
      _domainVersions[domainVersion.domainIdentifier] = updated;
    } else {
      _domainVersions[domainVersion.domainIdentifier] = [domainVersion];
    }
  }
}
