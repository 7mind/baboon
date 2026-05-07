// Facade over per-domain-version codec registries. Mirrors C# `BaboonCodecsFacade.cs`, Java
// `BaboonCodecsFacade.java`, Kotlin `BaboonCodecsFacade.kt`, TypeScript `BaboonCodecsFacade.ts`,
// Dart `baboon_codecs_facade.dart`. Public surface:
//   * codec / conversion / meta registration (overloads)
//   * `verify()` startup-sanity check
//   * `preload()` fire-and-forget pre-evaluation of lazy registries (MFACADE-PR-4)
//   * `encodeToBin` / `decodeFromBin`
//   * `decodeFromBinLatest<T: BaboonGeneratedLatest>` — decode then convert to latest (MFACADE-PR-4)
//   * `encodeToJson` / `decodeFromJson`
//   * `decodeFromJsonLatest<T: BaboonGeneratedLatest>` — decode then convert to latest (MFACADE-PR-4)
//   * `convert<TFrom, TTo>` cross-version (single-step stub — multi-step deferred per PR-17-D05)
//   * `decodeAny(opaque)`
//   * `jsonToUebaBytes(meta, json, staticDomain?, staticVersion?, staticTypeid?)` (PR-06-D01)
//   * `uebaToJson(meta, bytes, staticDomain?, staticVersion?, staticTypeid?)` (symmetric)
//
// Defects addressed:
//   PR-04-D02  errors thread through `Result<_, BaboonCodecException>` rather than throwing
//   PR-06-D01  cross-format helpers accept static fallbacks; wire-`meta.X` overrides `staticX`
//   PR-07-D02  `getCodec` single-version-domain edge case routes to exact lookup
//   PR-08-D01  `BaboonTypeMeta.readMetaJson` tolerates absent `$mv` and rejects `$mv != "1"`
//   PR-08-D02  fail-fast on empty `baboonSameInVersions` (in BaboonTypeMeta.from)
//   PR-19-D02  `useAdtIdentifier` knob threaded through encode paths
//   PR-19-D01/PR-20-D01 escape gotcha: `\(...)` for Swift interpolation; `$` literal in strings.
//   PR-22-D02  staged rollout closed in MFACADE-PR-E: generated DTOs now declare `BaboonMetaProvider`
//              conformance directly; the protocol's default extension forwards instance accessors
//              to the per-type static metadata, so no per-type instance impl is required.

import Foundation

private let CONTENT_JSON_KEY = "$c"

// MFACADE-PR-6: `open` so generated `Domain<X>Facade` subclasses (in user packages outside
// the BaboonRuntime module) can inherit. Swift's `public class` is closed by default;
// cross-module subclassing requires `open`.
open class BaboonCodecsFacade: BaboonCodecsFacadeBase {
    private var versionsCodecsJson: [BaboonDomainVersion: BaboonLazy<AbstractBaboonJsonCodecs>] = [:]
    private var versionsCodecsBin: [BaboonDomainVersion: BaboonLazy<AbstractBaboonUebaCodecs>] = [:]
    private var versionsConversions: [BaboonDomainVersion: BaboonLazy<AbstractBaboonConversions>] = [:]
    private var versionsMeta: [BaboonDomainVersion: BaboonLazy<BaboonMeta>] = [:]
    private var domainVersions: [String: [BaboonDomainVersion]] = [:]

    public override init() { super.init() }

    public func latest(_ domain: String) throws -> BaboonVersion {
        guard let versions = domainVersions[domain], !versions.isEmpty else {
            throw BaboonException("No registered version for \(domain) domain found.")
        }
        return try versions.last!.version()
    }

    // Bulk-merge another facade. Symmetric across runtimes.
    public func registerFacade(_ other: BaboonCodecsFacade) {
        for (k, v) in other.domainVersions { domainVersions[k] = v }
        for (k, v) in other.versionsCodecsJson { versionsCodecsJson[k] = v }
        for (k, v) in other.versionsCodecsBin { versionsCodecsBin[k] = v }
        for (k, v) in other.versionsConversions { versionsConversions[k] = v }
        for (k, v) in other.versionsMeta { versionsMeta[k] = v }
    }

    @discardableResult
    public func register(
        _ domainVersion: BaboonDomainVersion,
        codecsJson: @escaping () -> AbstractBaboonJsonCodecs,
        codecsBin: @escaping () -> AbstractBaboonUebaCodecs,
        conversions: @escaping () -> AbstractBaboonConversions,
        meta: @escaping () -> BaboonMeta
    ) -> BaboonDomainVersion {
        registerVersion(domainVersion)
        versionsCodecsJson[domainVersion] = BaboonLazy(codecsJson)
        versionsCodecsBin[domainVersion] = BaboonLazy(codecsBin)
        versionsConversions[domainVersion] = BaboonLazy(conversions)
        versionsMeta[domainVersion] = BaboonLazy(meta)
        return domainVersion
    }

    @discardableResult
    public func registerCodecs(
        _ domainVersion: BaboonDomainVersion,
        codecsJson: @escaping () -> AbstractBaboonJsonCodecs,
        codecsBin: @escaping () -> AbstractBaboonUebaCodecs
    ) -> BaboonDomainVersion {
        registerVersion(domainVersion)
        versionsCodecsJson[domainVersion] = BaboonLazy(codecsJson)
        versionsCodecsBin[domainVersion] = BaboonLazy(codecsBin)
        return domainVersion
    }

    @discardableResult
    public func registerCodecsAndMeta(
        _ domainVersion: BaboonDomainVersion,
        codecsJson: @escaping () -> AbstractBaboonJsonCodecs,
        codecsBin: @escaping () -> AbstractBaboonUebaCodecs,
        meta: @escaping () -> BaboonMeta
    ) -> BaboonDomainVersion {
        registerVersion(domainVersion)
        versionsCodecsJson[domainVersion] = BaboonLazy(codecsJson)
        versionsCodecsBin[domainVersion] = BaboonLazy(codecsBin)
        versionsMeta[domainVersion] = BaboonLazy(meta)
        return domainVersion
    }

    @discardableResult
    public func registerConversions(
        _ domainVersion: BaboonDomainVersion,
        _ conversions: @escaping () -> AbstractBaboonConversions
    ) -> BaboonDomainVersion {
        registerVersion(domainVersion)
        versionsConversions[domainVersion] = BaboonLazy(conversions)
        return domainVersion
    }

    @discardableResult
    public func registerMeta(
        _ domainVersion: BaboonDomainVersion,
        _ meta: @escaping () -> BaboonMeta
    ) -> BaboonDomainVersion {
        registerVersion(domainVersion)
        versionsMeta[domainVersion] = BaboonLazy(meta)
        return domainVersion
    }

    // Startup-sanity check. Mirrors C#/Java/Kotlin/Dart `verify()`: rejects empty-registry facades
    // and requires conversions + meta for every registered version.
    public func verify() throws {
        if domainVersions.isEmpty {
            throw BaboonException("Baboon codecs must have at least one domain registered.")
        }
        for versions in domainVersions.values {
            for dv in versions {
                if versionsConversions[dv] == nil {
                    throw BaboonCodecException.conversionNotFound(
                        "Baboon codecs must have conversion for \(dv) registered."
                    )
                }
                if versionsMeta[dv] == nil {
                    throw BaboonCodecException.codecNotFound(
                        "Baboon codecs must have codecs for \(dv) registered."
                    )
                }
            }
        }
    }

    // MFACADE-PR-4: fire-and-forget pre-evaluation of all lazy codec, conversion, and meta
    // registries so that first real encode/decode calls do not pay initialisation latency.
    // Errors inside individual lazy initialisers are swallowed — a faulty registry will
    // surface a typed failure on the first actual call.
    public func preload() {
        let jsonSnap = versionsCodecsJson
        let binSnap = versionsCodecsBin
        let convSnap = versionsConversions
        let metaSnap = versionsMeta
        DispatchQueue.global().async {
            for lazy in jsonSnap.values { _ = lazy.value }
            for lazy in binSnap.values { _ = lazy.value }
            for lazy in convSnap.values { _ = lazy.value }
            for lazy in metaSnap.values { _ = lazy.value }
        }
    }

    // MFACADE-PR-4: decode binary bytes then convert the result to the registered latest version.
    // Composition: `decodeFromBin` → `convert(fromTypeId, toTypeId=fromTypeId, latestVersion)`.
    // If the decoded value is already an instance of `T` (already at latest), it is returned
    // directly without a conversion round-trip.
    // The `toTypeId` is assumed equal to `fromTypeId` (same type, later version). Callers that
    // need to convert across different typeIds should use `convert` directly.
    public func decodeFromBinLatest<T: BaboonGeneratedLatest>(_ reader: BaboonBinReader) -> Result<T, BaboonCodecException> {
        let decoded = decodeFromBin(reader)
        switch decoded {
        case .failure(let e): return .failure(e)
        case .success(let value):
            if let already = value as? T { return .success(already) }
            guard let meta = value as? BaboonMetaProvider else {
                return .failure(.converterFailure(
                    "decodeFromBinLatest: decoded value of type \(type(of: value)) does not conform to BaboonMetaProvider.",
                    nil
                ))
            }
            let domain = meta.baboonDomainIdentifier
            let fromTypeId = meta.baboonTypeIdentifier
            guard let versions = domainVersions[domain], !versions.isEmpty else {
                return .failure(.converterFailure("decodeFromBinLatest: unknown domain '\(domain)'.", nil))
            }
            let latestDV = versions.last!
            let convResult = convert(value, fromTypeId, fromTypeId, latestDV)
            switch convResult {
            case .failure(let e): return .failure(e)
            case .success(let converted):
                guard let typed = converted as? T else {
                    return .failure(.converterFailure(
                        "decodeFromBinLatest: converted value of type \(type(of: converted)) cannot be cast to expected type.",
                        nil
                    ))
                }
                return .success(typed)
            }
        }
    }

    // MFACADE-PR-4: decode JSON then convert the result to the registered latest version.
    // Absent or invalid envelope (no recognisable `BaboonTypeMeta` header) → `nil` (null
    // pass-through, mirrors Scala `Right(None)`). Decode or conversion failure → throws.
    public func decodeFromJsonLatest<T: BaboonGeneratedLatest>(_ value: Any) throws -> T? {
        guard BaboonTypeMeta.readMetaJson(value) != nil else { return nil }
        let decoded = decodeFromJson(value)
        switch decoded {
        case .failure(let e): throw e
        case .success(let decodedValue):
            if let already = decodedValue as? T { return already }
            guard let meta = decodedValue as? BaboonMetaProvider else {
                throw BaboonCodecException.converterFailure(
                    "decodeFromJsonLatest: decoded value of type \(type(of: decodedValue)) does not conform to BaboonMetaProvider.",
                    nil
                )
            }
            let domain = meta.baboonDomainIdentifier
            let fromTypeId = meta.baboonTypeIdentifier
            guard let versions = domainVersions[domain], !versions.isEmpty else {
                throw BaboonCodecException.converterFailure(
                    "decodeFromJsonLatest: unknown domain '\(domain)'.", nil
                )
            }
            let latestDV = versions.last!
            let convResult = convert(decodedValue, fromTypeId, fromTypeId, latestDV)
            switch convResult {
            case .failure(let e): throw e
            case .success(let converted):
                guard let typed = converted as? T else {
                    throw BaboonCodecException.converterFailure(
                        "decodeFromJsonLatest: converted value of type \(type(of: converted)) cannot be cast to expected type.",
                        nil
                    )
                }
                return typed
            }
        }
    }

    // ----- encode / decode --------------------------------------------------------------------

    // PR-19-D02: pass `useAdtIdentifier=true` when encoding through an ADT-typed reference so the
    // envelope carries the ADT's type identifier rather than the concrete branch's.
    public func encodeToBin(
        _ ctx: BaboonCodecContext,
        _ value: Any,
        typeMetaOverride: BaboonTypeMeta? = nil,
        useAdtIdentifier: Bool = false
    ) -> Result<Data, BaboonCodecException> {
        let typeMeta: BaboonTypeMeta
        do {
            typeMeta = try BaboonTypeMeta.from(value, useAdtIdentifier: useAdtIdentifier)
        } catch {
            return .failure(.encoderFailure("Cannot derive type meta from value: \(error)", error))
        }
        let codecResult = getBinCodec(typeMeta, exact: true)
        switch codecResult {
        case .failure(let e): return .failure(e)
        case .success(let data):
            // The registry stores generated codec instances as `Any`; the cast must succeed for
            // an encoder-bearing variant. Encode itself does not throw on Swift; failures here
            // surface as runtime traps inside the typed encode body.
            guard let binEncoder = data as? AnyBaboonBinEncoder else {
                return .failure(.encoderFailure(
                    "Codec for \(typeMeta.typeIdentifier) does not support encoding (encoder-less codec or generic-mismatched).",
                    nil
                ))
            }
            let writer = BaboonBinWriter()
            (typeMetaOverride ?? typeMeta).writeBin(writer)
            binEncoder.encodeAnyValue(ctx, writer, value)
            return .success(writer.toData())
        }
    }

    public func decodeFromBinBytes(_ bytes: Data) -> Result<Any, BaboonCodecException> {
        return decodeFromBin(BaboonBinReader(bytes))
    }

    public func decodeFromBin(_ reader: BaboonBinReader) -> Result<Any, BaboonCodecException> {
        let typeMeta: BaboonTypeMeta?
        do {
            typeMeta = try BaboonTypeMeta.readMetaBin(reader)
        } catch {
            return .failure(.decoderFailure("Cannot decode binary type meta", error))
        }
        guard let tm = typeMeta else {
            return .failure(.decoderFailure("Cannot decode binary type meta", nil))
        }

        let codecResult = getBinCodec(tm, exact: false)
        switch codecResult {
        case .failure(let e): return .failure(e)
        case .success(let data):
            do {
                guard let binCodec = data as? AnyBaboonBinDecoder else {
                    return .failure(.decoderFailure(
                        "Codec for \(tm.typeIdentifier) is not a binary codec.",
                        nil
                    ))
                }
                return .success(try binCodec.decodeAnyValue(BaboonCodecContext.compact, reader))
            } catch {
                return .failure(.decoderFailure(
                    "Can not decode BIN form type [\(tm.domainIdentifier).\(tm.typeIdentifier)] of version '\(tm.domainVersion)'.",
                    error
                ))
            }
        }
    }

    // PR-19-D02: pass `useAdtIdentifier=true` when encoding through an ADT-typed reference.
    public func encodeToJson(
        _ value: Any,
        typeMetaOverride: BaboonTypeMeta? = nil,
        useAdtIdentifier: Bool = false
    ) -> Result<[String: Any], BaboonCodecException> {
        let typeMeta: BaboonTypeMeta
        do {
            typeMeta = try BaboonTypeMeta.from(value, useAdtIdentifier: useAdtIdentifier)
        } catch {
            return .failure(.encoderFailure("Cannot derive type meta from value: \(error)", error))
        }
        let codecResult = getJsonCodec(typeMeta, exact: true)
        switch codecResult {
        case .failure(let e): return .failure(e)
        case .success(let data):
            guard let jsonEncoder = data as? AnyBaboonJsonEncoder else {
                return .failure(.encoderFailure(
                    "Codec for \(typeMeta.typeIdentifier) does not support encoding (encoder-less codec or generic-mismatched).",
                    nil
                ))
            }
            let content = jsonEncoder.encodeAnyValue(BaboonCodecContext.compact, value)
            var metaJson = (typeMetaOverride ?? typeMeta).writeJson()
            metaJson[CONTENT_JSON_KEY] = content
            return .success(metaJson)
        }
    }

    public func decodeFromJsonString(_ value: String) -> Result<Any, BaboonCodecException> {
        guard let data = value.data(using: .utf8) else {
            return .failure(.decoderFailure("Cannot encode JSON string as UTF-8", nil))
        }
        do {
            let parsed = try JSONSerialization.jsonObject(with: data, options: [.fragmentsAllowed])
            return decodeFromJson(parsed)
        } catch {
            return .failure(.decoderFailure("Cannot parse JSON: \(error)", error))
        }
    }

    public func decodeFromJson(_ value: Any?) -> Result<Any, BaboonCodecException> {
        guard let typeMeta = BaboonTypeMeta.readMetaJson(value) else {
            return .failure(.decoderFailure("Cannot decode JSON type meta", nil))
        }
        guard let asMap = value as? [String: Any] else {
            return .failure(.decoderFailure("Cannot decode JSON type meta", nil))
        }
        let contentToken = asMap[CONTENT_JSON_KEY]
        if contentToken == nil && !asMap.keys.contains(CONTENT_JSON_KEY) {
            return .failure(.decoderFailure(
                "Missing '\(CONTENT_JSON_KEY)' content key for type [\(typeMeta.domainIdentifier).\(typeMeta.typeIdentifier)].",
                nil
            ))
        }

        let codecResult = getJsonCodec(typeMeta, exact: false)
        switch codecResult {
        case .failure(let e): return .failure(e)
        case .success(let data):
            do {
                guard let jsonCodec = data as? AnyBaboonJsonDecoder else {
                    return .failure(.decoderFailure(
                        "Codec for \(typeMeta.typeIdentifier) is not a JSON codec.",
                        nil
                    ))
                }
                return .success(try jsonCodec.decodeAnyValue(BaboonCodecContext.compact, contentToken as Any))
            } catch {
                return .failure(.decoderFailure(
                    "Can not decode JSON form type [\(typeMeta.domainIdentifier).\(typeMeta.typeIdentifier)] of version '\(typeMeta.domainVersion)'.",
                    error
                ))
            }
        }
    }

    // ----- AnyOpaque dispatch -----------------------------------------------------------------

    // Decode an `AnyOpaque` payload via the registered codec for `(meta.domain, meta.version,
    // meta.typeid)`. User-facing — `meta` must carry all three components (variant A only). For
    // variants B/C/D1/D2/D3 use the cross-format helpers (`jsonToUebaBytes` / `uebaToJson`).
    // PR-04-D02: errors thread through `Result`.
    public func decodeAny(_ opaque: AnyOpaque) -> Result<Any, BaboonCodecException> {
        let metaResult = buildSyntheticTypeMeta(opaque.meta, nil, nil, nil)
        let typeMeta: BaboonTypeMeta
        switch metaResult {
        case .failure(let e): return .failure(e)
        case .success(let m): typeMeta = m
        }

        switch opaque {
        case .ueba(_, let bytes):
            let codecResult = getBinCodec(typeMeta, exact: false)
            switch codecResult {
            case .failure(let e): return .failure(e)
            case .success(let data):
                do {
                    guard let binCodec = data as? AnyBaboonBinDecoder else {
                        return .failure(.decoderFailure(
                            "decodeAny: codec for \(typeMeta.typeIdentifier) is not a binary codec.",
                            nil
                        ))
                    }
                    return .success(try binCodec.decodeAnyValue(BaboonCodecContext.compact, BaboonBinReader(bytes)))
                } catch {
                    return .failure(.decoderFailure(
                        "decodeAny: cannot decode UEBA payload of type [\(typeMeta.domainIdentifier).\(typeMeta.typeIdentifier)] of version '\(typeMeta.domainVersion)'.",
                        error
                    ))
                }
            }
        case .json(_, let json):
            let codecResult = getJsonCodec(typeMeta, exact: false)
            switch codecResult {
            case .failure(let e): return .failure(e)
            case .success(let data):
                do {
                    guard let jsonCodec = data as? AnyBaboonJsonDecoder else {
                        return .failure(.decoderFailure(
                            "decodeAny: codec for \(typeMeta.typeIdentifier) is not a JSON codec.",
                            nil
                        ))
                    }
                    return .success(try jsonCodec.decodeAnyValue(BaboonCodecContext.compact, json as Any))
                } catch {
                    return .failure(.decoderFailure(
                        "decodeAny: cannot decode JSON payload of type [\(typeMeta.domainIdentifier).\(typeMeta.typeIdentifier)] of version '\(typeMeta.domainVersion)'.",
                        error
                    ))
                }
            }
        }
    }

    // Cross-format helper: decode an `AnyOpaque` JSON payload via the registered JSON codec, then
    // re-encode it via the registered UEBA codec. Static fallbacks fill components missing from
    // the wire `meta` (variants B/C/D1/D2/D3 — codec-generation-time knowledge); wire data wins
    // when both are present (override semantics). See PR-06-D01.
    public func jsonToUebaBytes(
        _ meta: AnyMeta,
        _ json: Any?,
        staticDomain: String? = nil,
        staticVersion: String? = nil,
        staticTypeid: String? = nil
    ) -> Result<Data, BaboonCodecException> {
        let metaResult = buildSyntheticTypeMeta(meta, staticDomain, staticVersion, staticTypeid)
        let typeMeta: BaboonTypeMeta
        switch metaResult {
        case .failure(let e): return .failure(e)
        case .success(let m): typeMeta = m
        }

        let jsonCodecResult = getJsonCodec(typeMeta, exact: false)
        let jsonCodec: AnyBaboonJsonDecoder
        switch jsonCodecResult {
        case .failure(let e): return .failure(e)
        case .success(let data):
            guard let c = data as? AnyBaboonJsonDecoder else {
                return .failure(.decoderFailure("Codec for \(typeMeta.typeIdentifier) is not a JSON codec.", nil))
            }
            jsonCodec = c
        }

        let binCodecResult = getBinCodec(typeMeta, exact: false)
        let binCodec: AnyBaboonBinEncoder
        switch binCodecResult {
        case .failure(let e): return .failure(e)
        case .success(let data):
            guard let c = data as? AnyBaboonBinEncoder else {
                return .failure(.encoderFailure("Codec for \(typeMeta.typeIdentifier) does not support encoding.", nil))
            }
            binCodec = c
        }

        let typed: Any
        do {
            typed = try jsonCodec.decodeAnyValue(BaboonCodecContext.compact, json as Any)
        } catch {
            return .failure(.decoderFailure(
                "jsonToUebaBytes: cannot decode JSON payload of type [\(typeMeta.domainIdentifier).\(typeMeta.typeIdentifier)] of version '\(typeMeta.domainVersion)'.",
                error
            ))
        }

        let writer = BaboonBinWriter()
        binCodec.encodeAnyValue(BaboonCodecContext.compact, writer, typed)
        return .success(writer.toData())
    }

    public func uebaToJson(
        _ meta: AnyMeta,
        _ bytes: Data,
        staticDomain: String? = nil,
        staticVersion: String? = nil,
        staticTypeid: String? = nil
    ) -> Result<Any, BaboonCodecException> {
        let metaResult = buildSyntheticTypeMeta(meta, staticDomain, staticVersion, staticTypeid)
        let typeMeta: BaboonTypeMeta
        switch metaResult {
        case .failure(let e): return .failure(e)
        case .success(let m): typeMeta = m
        }

        let binCodecResult = getBinCodec(typeMeta, exact: false)
        let binCodec: AnyBaboonBinDecoder
        switch binCodecResult {
        case .failure(let e): return .failure(e)
        case .success(let data):
            guard let c = data as? AnyBaboonBinDecoder else {
                return .failure(.decoderFailure("Codec for \(typeMeta.typeIdentifier) is not a binary codec.", nil))
            }
            binCodec = c
        }

        let jsonCodecResult = getJsonCodec(typeMeta, exact: false)
        let jsonCodec: AnyBaboonJsonEncoder
        switch jsonCodecResult {
        case .failure(let e): return .failure(e)
        case .success(let data):
            guard let c = data as? AnyBaboonJsonEncoder else {
                return .failure(.encoderFailure("Codec for \(typeMeta.typeIdentifier) does not support encoding.", nil))
            }
            jsonCodec = c
        }

        let typed: Any
        do {
            typed = try binCodec.decodeAnyValue(BaboonCodecContext.compact, BaboonBinReader(bytes))
        } catch {
            return .failure(.decoderFailure(
                "uebaToJson: cannot decode UEBA payload of type [\(typeMeta.domainIdentifier).\(typeMeta.typeIdentifier)] of version '\(typeMeta.domainVersion)'.",
                error
            ))
        }

        return .success(jsonCodec.encodeAnyValue(BaboonCodecContext.compact, typed))
    }

    // Synthesise a `BaboonTypeMeta` from an `AnyMeta` plus optional static fallbacks. `AnyMeta`
    // does not carry a min-compat version; forward-version migration is unavailable for any-
    // payloads, so `domainVersionMinCompat = version`. `meta.X` takes precedence over `staticX`
    // (override semantics — wire data wins). `decodeAny` calls with all-nil statics so its
    // variant-A-only contract is preserved (PR-06-D01).
    private func buildSyntheticTypeMeta(
        _ meta: AnyMeta,
        _ staticDomain: String?,
        _ staticVersion: String?,
        _ staticTypeid: String?
    ) -> Result<BaboonTypeMeta, BaboonCodecException> {
        let domain = meta.domain ?? staticDomain
        let version = meta.version ?? staticVersion
        let typeid = meta.typeid ?? staticTypeid

        if let d = domain, let v = version, let t = typeid {
            return .success(BaboonTypeMeta(BaboonTypeMetaCodec.metaVersion, d, v, v, t))
        }

        var missing: [String] = []
        if domain == nil { missing.append("domain") }
        if version == nil { missing.append("version") }
        if typeid == nil { missing.append("typeid") }
        return .failure(.decoderFailure(
            "AnyMeta requires domain/version/typeid for facade resolution; got kind 0x\(String(meta.kind, radix: 16)) which lacks: \(missing.joined(separator: ", "))",
            nil
        ))
    }

    // ----- cross-version conversion -----------------------------------------------------------

    // Single-step convert via the registered conversion for `(fromTypeId, toTypeId)`. Multi-step
    // chaining is deferred (PR-17-D05).
    public func convert(
        _ value: Any,
        _ fromTypeId: String,
        _ toTypeId: String,
        _ toVersion: BaboonDomainVersion
    ) -> Result<Any, BaboonCodecException> {
        guard let lazyConv = versionsConversions[toVersion] else {
            return .failure(.converterFailure(
                "Can not find version '\(toVersion)' conversions.",
                nil
            ))
        }
        // `convertWithContext` is generic and non-throwing in the existing runtime; under failure
        // it traps via `fatalError` (cannot recover). For PR 9.1 we surface only the "no
        // conversions registry" failure cleanly.
        let result: Any = lazyConv.value.convertWithContext(nil, value, fromTypeId, toTypeId)
        return .success(result)
    }

    // ----- private dispatch -------------------------------------------------------------------

    private func getBinCodec(_ typeMeta: BaboonTypeMeta, exact: Bool) -> Result<AnyObject, BaboonCodecException> {
        return getCodec(typeMeta, exact, { (k: BaboonDomainVersion) -> AnyObject? in
            guard let lazy = self.versionsCodecsBin[k] else { return nil }
            return lazy.value
        })
    }

    private func getJsonCodec(_ typeMeta: BaboonTypeMeta, exact: Bool) -> Result<AnyObject, BaboonCodecException> {
        return getCodec(typeMeta, exact, { (k: BaboonDomainVersion) -> AnyObject? in
            guard let lazy = self.versionsCodecsJson[k] else { return nil }
            return lazy.value
        })
    }

    private func getCodec(
        _ typeMeta: BaboonTypeMeta,
        _ exact: Bool,
        _ codecsLookup: (BaboonDomainVersion) -> AnyObject?
    ) -> Result<AnyObject, BaboonCodecException> {
        guard let versions = domainVersions[typeMeta.domainIdentifier], !versions.isEmpty else {
            return .failure(.codecNotFound("Unknown domain \(typeMeta.domainIdentifier)."))
        }

        let minVersion = versions.first!
        let maxVersion = versions.last!

        let lookupVersion = typeMeta.versionRef()
        let minCompat = typeMeta.versionMinCompat()

        let lookupV: BaboonVersion
        let maxV: BaboonVersion
        let minV: BaboonVersion
        do {
            lookupV = try lookupVersion.version()
            maxV = try maxVersion.version()
            minV = try minVersion.version()
        } catch let e as BaboonException {
            return .failure(.codecNotFound("Invalid version: \(e.message)"))
        } catch {
            return .failure(.codecNotFound("Invalid version: \(error)"))
        }

        let modelVersion: BaboonDomainVersion
        if let mc = minCompat, lookupV > maxV {
            modelVersion = mc
        } else {
            modelVersion = lookupVersion
        }
        let modelV: BaboonVersion
        do { modelV = try modelVersion.version() } catch {
            return .failure(.codecNotFound("Invalid version: \(error)"))
        }

        if exact && modelV == maxV {
            return getCodecExact(modelVersion, typeMeta.typeIdentifier, codecsLookup)
        }
        // PR-07-D02: non-exact lookup at the latest registered version routes to exact lookup.
        if !exact && modelV == maxV {
            return getCodecExact(modelVersion, typeMeta.typeIdentifier, codecsLookup)
        }
        if modelV >= minV && modelV < maxV {
            return getCodecMaxCompat(modelVersion, maxVersion, typeMeta.typeIdentifier, codecsLookup)
        }
        if modelV < minV {
            return getCodecMaxCompat(minVersion, maxVersion, typeMeta.typeIdentifier, codecsLookup)
        }
        return .failure(.codecNotFound("Unsupported domain version '\(modelVersion)'."))
    }

    private func getCodecExact(
        _ domainVersion: BaboonDomainVersion,
        _ typeIdentifier: String,
        _ codecsLookup: (BaboonDomainVersion) -> AnyObject?
    ) -> Result<AnyObject, BaboonCodecException> {
        guard let codecs = codecsLookup(domainVersion) else {
            return .failure(.codecNotFound("No codecs registered for domain version '\(domainVersion)'."))
        }
        // The lookup returned an `AbstractBaboonCodecs`-shaped object; ask for the typed codec.
        if let asJsonCodecs = codecs as? AbstractBaboonJsonCodecs,
           let codec = asJsonCodecs.codecFor(typeIdentifier) {
            return .success(codec as AnyObject)
        }
        if let asUebaCodecs = codecs as? AbstractBaboonUebaCodecs,
           let codec = asUebaCodecs.codecFor(typeIdentifier) {
            return .success(codec as AnyObject)
        }
        return .failure(.codecNotFound(
            "No codec found for type [\(domainVersion.domainIdentifier).\(typeIdentifier)] of version '\(domainVersion.domainVersion)'."
        ))
    }

    private func getCodecMaxCompat(
        _ modelVersion: BaboonDomainVersion,
        _ maxVersion: BaboonDomainVersion,
        _ typeIdentifier: String,
        _ codecsLookup: (BaboonDomainVersion) -> AnyObject?
    ) -> Result<AnyObject, BaboonCodecException> {
        guard let lazyMeta = versionsMeta[modelVersion] else {
            return .failure(.codecNotFound("Unknown domain version '\(modelVersion)'."))
        }
        let sameVersions = lazyMeta.value.sameInVersions(typeIdentifier)
        var bestSame: String? = nil
        let maxV: BaboonVersion
        do { maxV = try maxVersion.version() } catch {
            return .failure(.codecNotFound("Invalid maxVersion: \(error)"))
        }
        for sv in sameVersions.reversed() {
            if sv == maxVersion.domainVersion {
                bestSame = sv
                break
            }
            do {
                let svParsed = try BaboonVersion.from(sv)
                if svParsed <= maxV {
                    bestSame = sv
                    break
                }
            } catch {
                // skip malformed entries
            }
        }
        guard let chosen = bestSame else {
            return .failure(.codecNotFound(
                "No max compat codec found for type [\(modelVersion.domainIdentifier).\(typeIdentifier)] of version '\(modelVersion.domainVersion)'."
            ))
        }
        let maxCompatVersion = BaboonDomainVersion(modelVersion.domainIdentifier, chosen)
        return getCodecExact(maxCompatVersion, typeIdentifier, codecsLookup)
    }

    private func registerVersion(_ domainVersion: BaboonDomainVersion) {
        if let existing = domainVersions[domainVersion.domainIdentifier] {
            if existing.contains(domainVersion) { return }
            var updated = existing
            updated.append(domainVersion)
            updated.sort { (a, b) in
                guard let av = try? a.version(), let bv = try? b.version() else { return false }
                return av < bv
            }
            domainVersions[domainVersion.domainIdentifier] = updated
        } else {
            domainVersions[domainVersion.domainIdentifier] = [domainVersion]
        }
    }
}
