// Facade over per-domain-version codec registries. Mirrors C# `BaboonCodecsFacade.cs`, Java
// `BaboonCodecsFacade.java`, Kotlin `BaboonCodecsFacade.kt`. Public surface:
//   * codec / conversion / meta registration (overloads)
//   * `verify()` startup-sanity check
//   * `encodeToBin` / `decodeFromBin`
//   * `encodeToJson` / `decodeFromJson`
//   * `convert<TFrom, TTo>` cross-version (stub — see Java PR-17-D05 note)
//   * `decodeAny(opaque)`
//   * `jsonToUebaBytes(meta, json, staticDomain?, staticVersion?, staticTypeid?)` (PR-06-D01)
//   * `uebaToJson(meta, bytes, staticDomain?, staticVersion?, staticTypeid?)` (symmetric)
//
// Defects addressed:
//   PR-06-D01  cross-format helpers accept static fallbacks; wire-`meta.X` overrides `staticX`
//   PR-07-D02  `getCodec` single-version-domain edge case routes to exact lookup
//   PR-08-D02  `BaboonTypeMeta.from` indexes `[0]` directly so empty `sameInVersions` fails fast

import {
    AbstractBaboonCodecs,
    AbstractBaboonJsonCodecs,
    AbstractBaboonUebaCodecs,
    BaboonBinReader,
    BaboonBinWriter,
    BaboonCodecContext,
    BaboonCodecException,
    BaboonCodecNotFound,
    BaboonConversionNotFound,
    BaboonConverterFailure,
    BaboonDecoderFailure,
    BaboonDomainVersion,
    BaboonEncoderFailure,
    BaboonException,
    BaboonTypeMeta,
    BaboonTypeMetaCodec,
    BaboonVersion,
    Lazy,
} from "./BaboonSharedRuntime";
import type {
    AbstractBaboonConversions,
    BaboonBinCodec,
    BaboonCodecData,
    BaboonEither,
    BaboonGenerated,
    BaboonGeneratedLatest,
    BaboonJsonCodec,
    BaboonMeta,
} from "./BaboonSharedRuntime";

import { AnyMetaCodec } from "./BaboonAnyOpaque";
import type { AnyMeta, AnyOpaque } from "./BaboonAnyOpaque";

const CONTENT_JSON_KEY = "$c";

function leftCodecException<R>(err: BaboonCodecException): BaboonEither<BaboonCodecException, R> {
    return { tag: "Left", value: err };
}

function right<R>(value: R): BaboonEither<BaboonCodecException, R> {
    return { tag: "Right", value };
}

export class BaboonCodecsFacade {
    private readonly versionsCodecsJson: Map<string, Lazy<AbstractBaboonJsonCodecs>> = new Map();
    private readonly versionsCodecsBin: Map<string, Lazy<AbstractBaboonUebaCodecs>> = new Map();
    private readonly versionsConversions: Map<string, Lazy<AbstractBaboonConversions>> = new Map();
    private readonly versionsMeta: Map<string, Lazy<BaboonMeta>> = new Map();
    /** domainIdentifier -> sorted list of registered domain-versions. */
    private readonly domainVersions: Map<string, BaboonDomainVersion[]> = new Map();

    public latest(domain: string): BaboonVersion {
        const versions = this.domainVersions.get(domain);
        if (versions && versions.length > 0) {
            return versions[versions.length - 1]!.version();
        }
        throw new BaboonException(`No registered version for ${domain} domain found.`);
    }

    /** Bulk-merge another facade. Symmetric across runtimes. */
    public registerFacade(other: BaboonCodecsFacade): void {
        for (const [k, v] of other.domainVersions) this.domainVersions.set(k, v);
        for (const [k, v] of other.versionsCodecsJson) this.versionsCodecsJson.set(k, v);
        for (const [k, v] of other.versionsCodecsBin) this.versionsCodecsBin.set(k, v);
        for (const [k, v] of other.versionsConversions) this.versionsConversions.set(k, v);
        for (const [k, v] of other.versionsMeta) this.versionsMeta.set(k, v);
    }

    /**
     * Register a `(json, ueba, conversions, meta)` 4-tuple for a domain version. Mirrors the Java
     * full-arity overload. Suppliers are wrapped in `Lazy` so registration is cheap and the
     * generator-side codec maps don't allocate until a value of that type round-trips.
     */
    public register(
        domainVersion: BaboonDomainVersion,
        codecsJson: () => AbstractBaboonJsonCodecs,
        codecsBin: () => AbstractBaboonUebaCodecs,
        conversions: () => AbstractBaboonConversions,
        meta: () => BaboonMeta,
    ): BaboonDomainVersion {
        this.registerVersion(domainVersion);
        const key = domainVersion.key();
        this.versionsCodecsJson.set(key, new Lazy(codecsJson));
        this.versionsCodecsBin.set(key, new Lazy(codecsBin));
        this.versionsConversions.set(key, new Lazy(conversions));
        this.versionsMeta.set(key, new Lazy(meta));
        return domainVersion;
    }

    /** Codec-only registration. Conversions / meta can land in subsequent calls. */
    public registerCodecs(
        domainVersion: BaboonDomainVersion,
        codecsJson: () => AbstractBaboonJsonCodecs,
        codecsBin: () => AbstractBaboonUebaCodecs,
    ): BaboonDomainVersion {
        this.registerVersion(domainVersion);
        const key = domainVersion.key();
        this.versionsCodecsJson.set(key, new Lazy(codecsJson));
        this.versionsCodecsBin.set(key, new Lazy(codecsBin));
        return domainVersion;
    }

    public registerCodecsAndMeta(
        domainVersion: BaboonDomainVersion,
        codecsJson: () => AbstractBaboonJsonCodecs,
        codecsBin: () => AbstractBaboonUebaCodecs,
        meta: () => BaboonMeta,
    ): BaboonDomainVersion {
        this.registerVersion(domainVersion);
        const key = domainVersion.key();
        this.versionsCodecsJson.set(key, new Lazy(codecsJson));
        this.versionsCodecsBin.set(key, new Lazy(codecsBin));
        this.versionsMeta.set(key, new Lazy(meta));
        return domainVersion;
    }

    public registerConversions(
        domainVersion: BaboonDomainVersion,
        conversions: () => AbstractBaboonConversions,
    ): BaboonDomainVersion {
        this.registerVersion(domainVersion);
        this.versionsConversions.set(domainVersion.key(), new Lazy(conversions));
        return domainVersion;
    }

    public registerMeta(
        domainVersion: BaboonDomainVersion,
        meta: () => BaboonMeta,
    ): BaboonDomainVersion {
        this.registerVersion(domainVersion);
        this.versionsMeta.set(domainVersion.key(), new Lazy(meta));
        return domainVersion;
    }

    /**
     * Startup-sanity check. Mirrors C# / Java verify(): rejects empty-registry facades and
     * requires conversions + meta for every registered version.
     */
    public verify(): void {
        if (this.domainVersions.size === 0) {
            throw new BaboonException("Baboon codecs must have at least one domain registered.");
        }
        for (const versions of this.domainVersions.values()) {
            for (const dv of versions) {
                const key = dv.key();
                if (!this.versionsConversions.has(key)) {
                    throw new BaboonConversionNotFound(`Baboon codecs must have conversion for ${dv} registered.`);
                }
                if (!this.versionsMeta.has(key)) {
                    throw new BaboonCodecNotFound(`Baboon codecs must have codecs for ${dv} registered.`);
                }
            }
        }
    }

    // ----- encode / decode ---------------------------------------------------------------------

    /**
     * PR-19-D02: pass `useAdtIdentifier=true` when encoding a value through an ADT-typed reference
     * so the meta envelope carries the ADT's type identifier rather than the concrete branch's.
     * Java/C#/Kotlin select this automatically from the static declared type; TS has no runtime
     * generics, so the caller must opt in. Default `false` preserves concrete-branch semantics.
     */
    public encodeToBin(
        ctx: BaboonCodecContext,
        value: BaboonGenerated,
        typeMetaOverride?: BaboonTypeMeta,
        useAdtIdentifier: boolean = false,
    ): BaboonEither<BaboonCodecException, Uint8Array> {
        const typeMeta = BaboonTypeMeta.from(value, useAdtIdentifier);
        const codecResult = this.getBinCodec(typeMeta, true);
        if (codecResult.tag === "Left") return codecResult;
        const codec = codecResult.value as BaboonBinCodec<BaboonGenerated>;

        try {
            const writer = new BaboonBinWriter();
            (typeMetaOverride !== undefined ? typeMetaOverride : typeMeta).writeBin(writer);
            codec.encode(ctx, value, writer);
            return right(writer.toBytes());
        } catch (e) {
            return leftCodecException(new BaboonEncoderFailure(
                `Exception while trying to encode to binary form type [${typeMeta.domainIdentifier}.${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'.`,
                { cause: e },
            ));
        }
    }

    public decodeFromBin(input: BaboonBinReader | Uint8Array): BaboonEither<BaboonCodecException, BaboonGenerated> {
        const reader = input instanceof Uint8Array ? new BaboonBinReader(input) : input;
        let typeMeta: BaboonTypeMeta | undefined;
        try {
            typeMeta = BaboonTypeMeta.readMeta(reader);
        } catch (e) {
            return leftCodecException(new BaboonDecoderFailure("Cannot decode binary type meta", { cause: e }));
        }
        if (typeMeta === undefined) {
            return leftCodecException(new BaboonDecoderFailure("Cannot decode binary type meta"));
        }

        const codecResult = this.getBinCodec(typeMeta, false);
        if (codecResult.tag === "Left") return codecResult;
        const codec = codecResult.value as BaboonBinCodec<BaboonGenerated>;

        try {
            return right(codec.decode(BaboonCodecContext.Compact, reader));
        } catch (e) {
            return leftCodecException(new BaboonDecoderFailure(
                `Can not decode BIN form type [${typeMeta.domainIdentifier}.${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'.`,
                { cause: e },
            ));
        }
    }

    /**
     * PR-19-D02: pass `useAdtIdentifier=true` when encoding a value through an ADT-typed reference
     * so the envelope carries the ADT's type identifier rather than the concrete branch's. Java /
     * C# / Kotlin select this automatically from the static declared type; TS has no runtime
     * generics, so the caller must opt in. Default `false` preserves concrete-branch semantics.
     */
    public encodeToJson(
        value: BaboonGenerated,
        typeMetaOverride?: BaboonTypeMeta,
        useAdtIdentifier: boolean = false,
    ): BaboonEither<BaboonCodecException, Record<string, unknown>> {
        const typeMeta = BaboonTypeMeta.from(value, useAdtIdentifier);
        const codecResult = this.getJsonCodec(typeMeta, true);
        if (codecResult.tag === "Left") return codecResult;
        const codec = codecResult.value as BaboonJsonCodec<BaboonGenerated>;

        try {
            const content = codec.encode(BaboonCodecContext.Compact, value);
            const metaJson = (typeMetaOverride !== undefined ? typeMetaOverride : typeMeta).writeJson();
            // PR-08-D06 analog: if the meta JSON were ever a non-object, the `$c` set would
            // silently lose the key. `BaboonTypeMetaCodec.writeJson` always returns a plain
            // object, but assert defensively in case someone overrides via subclass.
            if (typeof metaJson !== "object" || metaJson === null || Array.isArray(metaJson)) {
                return leftCodecException(new BaboonEncoderFailure(
                    `BaboonTypeMeta.writeJson must return a plain object; got ${Array.isArray(metaJson) ? "array" : typeof metaJson}`,
                ));
            }
            (metaJson as Record<string, unknown>)[CONTENT_JSON_KEY] = content;
            return right(metaJson as Record<string, unknown>);
        } catch (e) {
            return leftCodecException(new BaboonEncoderFailure(
                `Can not encode to json form type [${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'.`,
                { cause: e },
            ));
        }
    }

    public decodeFromJson(value: unknown): BaboonEither<BaboonCodecException, BaboonGenerated> {
        // String input — parse first, then dispatch to the structured branch.
        if (typeof value === "string") {
            try {
                const parsed: unknown = JSON.parse(value);
                return this.decodeFromJson(parsed);
            } catch (e) {
                const msg = e instanceof Error ? e.message : String(e);
                return leftCodecException(new BaboonDecoderFailure(`Cannot parse JSON: ${msg}`, { cause: e }));
            }
        }

        const typeMeta = BaboonTypeMeta.readMetaJson(value);
        if (typeMeta === undefined) {
            // Mirror Java: an unrecognised meta is "I cannot decode this", surface a typed error.
            return leftCodecException(new BaboonDecoderFailure("Cannot decode JSON type meta"));
        }
        if (typeof value !== "object" || value === null || Array.isArray(value)) {
            return leftCodecException(new BaboonDecoderFailure("JSON value must be a plain object"));
        }
        const obj = value as Record<string, unknown>;
        const contentToken = obj[CONTENT_JSON_KEY];
        if (contentToken === undefined) {
            return leftCodecException(new BaboonDecoderFailure(
                `Missing '${CONTENT_JSON_KEY}' content key for type [${typeMeta.domainIdentifier}.${typeMeta.typeIdentifier}].`,
            ));
        }

        const codecResult = this.getJsonCodec(typeMeta, false);
        if (codecResult.tag === "Left") return codecResult;
        const codec = codecResult.value as BaboonJsonCodec<BaboonGenerated>;

        try {
            return right(codec.decode(BaboonCodecContext.Compact, contentToken));
        } catch (e) {
            return leftCodecException(new BaboonDecoderFailure(
                `Can not decode JSON form type [${typeMeta.domainIdentifier}.${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'.`,
                { cause: e },
            ));
        }
    }

    // ----- AnyOpaque dispatch ------------------------------------------------------------------

    /**
     * Decode an `AnyOpaque` payload via the registered codec for `(meta.domain, meta.version,
     * meta.typeid)`. User-facing — `meta` must carry all three components (variant A only). For
     * variants B/C/D1/D2/D3 use the cross-format helpers (`jsonToUebaBytes` / `uebaToJson`)
     * which accept static fallbacks. PR-04-D02: errors thread through `BaboonEither`.
     */
    public decodeAny(opaque: AnyOpaque): BaboonEither<BaboonCodecException, BaboonGenerated> {
        const metaResult = BaboonCodecsFacade.buildSyntheticTypeMeta(opaque.meta, undefined, undefined, undefined);
        if (metaResult.tag === "Left") return metaResult;
        const typeMeta = metaResult.value;

        if (opaque.tag === "Ueba") {
            const codecResult = this.getBinCodec(typeMeta, false);
            if (codecResult.tag === "Left") return codecResult;
            const codec = codecResult.value as BaboonBinCodec<BaboonGenerated>;
            try {
                const reader = new BaboonBinReader(opaque.bytes);
                return right(codec.decode(BaboonCodecContext.Compact, reader));
            } catch (e) {
                return leftCodecException(new BaboonDecoderFailure(
                    `decodeAny: cannot decode UEBA payload of type [${typeMeta.domainIdentifier}.${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'.`,
                    { cause: e },
                ));
            }
        }
        // opaque.tag === "Json"
        const codecResult = this.getJsonCodec(typeMeta, false);
        if (codecResult.tag === "Left") return codecResult;
        const codec = codecResult.value as BaboonJsonCodec<BaboonGenerated>;
        try {
            return right(codec.decode(BaboonCodecContext.Compact, opaque.json));
        } catch (e) {
            return leftCodecException(new BaboonDecoderFailure(
                `decodeAny: cannot decode JSON payload of type [${typeMeta.domainIdentifier}.${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'.`,
                { cause: e },
            ));
        }
    }

    /**
     * Cross-format helper: decode an `AnyOpaqueJson` payload via the registered JSON codec, then
     * re-encode it via the registered UEBA codec. The wire `meta` may omit components that the
     * field's static declaration already pins down (variants B/C/D1/D2/D3). The codec generator
     * passes the static fallbacks; runtime `meta.X` takes precedence over `staticX` (override
     * semantics — wire wins). Without static fallback only variant A would work — PR-06-D01.
     */
    public jsonToUebaBytes(
        meta: AnyMeta,
        json: unknown,
        staticDomain?: string,
        staticVersion?: string,
        staticTypeid?: string,
    ): BaboonEither<BaboonCodecException, Uint8Array> {
        const metaResult = BaboonCodecsFacade.buildSyntheticTypeMeta(meta, staticDomain, staticVersion, staticTypeid);
        if (metaResult.tag === "Left") return metaResult;
        const typeMeta = metaResult.value;

        const jsonCodecResult = this.getJsonCodec(typeMeta, false);
        if (jsonCodecResult.tag === "Left") return jsonCodecResult;
        const jsonCodec = jsonCodecResult.value as BaboonJsonCodec<BaboonGenerated>;

        const binCodecResult = this.getBinCodec(typeMeta, false);
        if (binCodecResult.tag === "Left") return binCodecResult;
        const binCodec = binCodecResult.value as BaboonBinCodec<BaboonGenerated>;

        let typed: BaboonGenerated;
        try {
            typed = jsonCodec.decode(BaboonCodecContext.Compact, json);
        } catch (e) {
            return leftCodecException(new BaboonDecoderFailure(
                `jsonToUebaBytes: cannot decode JSON payload of type [${typeMeta.domainIdentifier}.${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'.`,
                { cause: e },
            ));
        }

        try {
            const writer = new BaboonBinWriter();
            // PR 13.2 fix: TS UEBA codec signature is (ctx, value, writer), not (ctx, writer, value).
            // The interface declaration in `BaboonSharedRuntime.ts` is misleading; the codegen
            // (TsUEBACodecGenerator) emits `(ctx, value, writer)` and the interface should match.
            // Argument order corrected here so cross-format JSON→UEBA conversion works.
            binCodec.encode(BaboonCodecContext.Compact, typed, writer);
            return right(writer.toBytes());
        } catch (e) {
            return leftCodecException(new BaboonEncoderFailure(
                `jsonToUebaBytes: cannot encode UEBA payload of type [${typeMeta.domainIdentifier}.${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'.`,
                { cause: e },
            ));
        }
    }

    /** Cross-format helper symmetric to {@link jsonToUebaBytes}. See its doc for static-fallback contract. */
    public uebaToJson(
        meta: AnyMeta,
        bytes: Uint8Array,
        staticDomain?: string,
        staticVersion?: string,
        staticTypeid?: string,
    ): BaboonEither<BaboonCodecException, unknown> {
        const metaResult = BaboonCodecsFacade.buildSyntheticTypeMeta(meta, staticDomain, staticVersion, staticTypeid);
        if (metaResult.tag === "Left") return metaResult;
        const typeMeta = metaResult.value;

        const binCodecResult = this.getBinCodec(typeMeta, false);
        if (binCodecResult.tag === "Left") return binCodecResult;
        const binCodec = binCodecResult.value as BaboonBinCodec<BaboonGenerated>;

        const jsonCodecResult = this.getJsonCodec(typeMeta, false);
        if (jsonCodecResult.tag === "Left") return jsonCodecResult;
        const jsonCodec = jsonCodecResult.value as BaboonJsonCodec<BaboonGenerated>;

        let typed: BaboonGenerated;
        try {
            const reader = new BaboonBinReader(bytes);
            typed = binCodec.decode(BaboonCodecContext.Compact, reader);
        } catch (e) {
            return leftCodecException(new BaboonDecoderFailure(
                `uebaToJson: cannot decode UEBA payload of type [${typeMeta.domainIdentifier}.${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'.`,
                { cause: e },
            ));
        }

        try {
            return right(jsonCodec.encode(BaboonCodecContext.Compact, typed));
        } catch (e) {
            return leftCodecException(new BaboonEncoderFailure(
                `uebaToJson: cannot encode JSON payload of type [${typeMeta.domainIdentifier}.${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'.`,
                { cause: e },
            ));
        }
    }

    /**
     * Synthesise a `BaboonTypeMeta` from an `AnyMeta` plus optional static fallbacks. `AnyMeta`
     * does not carry a min-compat version; forward-version migration is unavailable for any-
     * payloads, so `domainVersionMinCompat = version`. `meta.X` takes precedence over `staticX`
     * (override semantics — wire data wins). `decodeAny` calls with all-undefined statics so its
     * variant-A-only contract is preserved.
     */
    private static buildSyntheticTypeMeta(
        meta: AnyMeta,
        staticDomain: string | undefined,
        staticVersion: string | undefined,
        staticTypeid: string | undefined,
    ): BaboonEither<BaboonCodecException, BaboonTypeMeta> {
        const domain = meta.domain !== null ? meta.domain : (staticDomain ?? null);
        const version = meta.version !== null ? meta.version : (staticVersion ?? null);
        const typeid = meta.typeid !== null ? meta.typeid : (staticTypeid ?? null);

        if (domain !== null && version !== null && typeid !== null) {
            return right(new BaboonTypeMeta(BaboonTypeMetaCodec.META_VERSION, domain, version, version, typeid));
        }

        const missing: string[] = [];
        if (domain === null) missing.push("domain");
        if (version === null) missing.push("version");
        if (typeid === null) missing.push("typeid");
        return leftCodecException(new BaboonDecoderFailure(
            `AnyMeta requires domain/version/typeid for facade resolution; got kind 0x${(meta.kind & 0xFF).toString(16)} which lacks: ${missing.join(", ")}`,
        ));
    }

    // ----- cross-version conversion ------------------------------------------------------------

    /**
     * Cross-version conversion stub. The current TS `AbstractBaboonConversions` runtime interface
     * is a thin shape (`versionsFrom()` / `versionTo()`) — it does not yet expose a generic
     * `convertWithContext(value, fromClass, toClass)` like Java/C#. PR-17-D05 lesson: the simpler
     * pair-lookup shape ships now and a multi-step walk lands once generated TS conversions
     * expose a `convert(from)` dispatch hook. Until then this method reports a typed failure
     * rather than silently masquerading as success. PR 7.3/7.4 will land the generator-side hook
     * and the matching facade implementation.
     */
    public convert<TFrom extends BaboonGenerated, TTo extends BaboonGenerated>(
        _value: TFrom,
        _fromTypeIdentifier: string,
        _toTypeIdentifier: string,
    ): BaboonEither<BaboonCodecException, TTo> {
        return leftCodecException(new BaboonConverterFailure(
            "BaboonCodecsFacade.convert is not yet implemented in the TypeScript runtime; lands with PR 7.3/7.4 once generated conversions expose a generic dispatch hook.",
        ));
    }

    /**
     * Pre-evaluate all registered `Lazy` codec/conversion/meta entries by forcing their `.value`
     * accessor. The entries are iterated synchronously inside a `Promise.resolve().then(...)` so
     * the call returns immediately (fire-and-forget). Any exception thrown by a lazy initializer
     * is swallowed — preload is best-effort warm-up.
     *
     * Mirrors Scala `preload()` (Future { ... }.recover).
     */
    public preload(): void {
        const jsonEntries = [...this.versionsCodecsJson.values()];
        const binEntries  = [...this.versionsCodecsBin.values()];
        const convEntries = [...this.versionsConversions.values()];
        const metaEntries = [...this.versionsMeta.values()];
        Promise.resolve().then(() => {
            try {
                for (const e of jsonEntries) { e.value; }
                for (const e of binEntries)  { e.value; }
                for (const e of convEntries) { e.value; }
                for (const e of metaEntries) { e.value; }
            } catch (_) {
                // swallow — preload is best-effort
            }
        });
    }

    /**
     * Decode a binary envelope and immediately convert the result to the latest registered
     * version of `T`. Mirrors Scala `decodeFromBinLatest[T <: BaboonGeneratedLatest](reader)`.
     *
     * TS has no ClassTag, so the caller supplies `targetTypeIdentifier` — the Baboon-domain type
     * identifier string for `T` (e.g. `"my.domain/:#MyType"`). This is forwarded to `convert`;
     * once PR 7.3/7.4 lands the actual conversion walk, the identifier will be used to locate
     * the target codec. Until then `convert` returns a `BaboonConverterFailure` (stub).
     */
    public decodeFromBinLatest<T extends BaboonGeneratedLatest>(
        input: BaboonBinReader | Uint8Array,
        targetTypeIdentifier: string,
    ): BaboonEither<BaboonCodecException, T> {
        const decoded = this.decodeFromBin(input);
        if (decoded.tag === "Left") return decoded;
        return this.convert<BaboonGenerated, T>(
            decoded.value,
            decoded.value.baboonTypeIdentifier(),
            targetTypeIdentifier,
        );
    }

    /**
     * Decode a JSON envelope and immediately convert the result to the latest registered version
     * of `T`. Mirrors Scala `decodeFromJsonLatest[T <: BaboonGeneratedLatest](value: Json)`.
     *
     * The TS `decodeFromJson` returns `Left` both for unrecognised envelopes and for codec
     * failures (no `Option` wrapper). `decodeFromJsonLatest` preserves that contract: `Left`
     * propagates, `Right` is forwarded to `convert`. `targetTypeIdentifier` is the Baboon-domain
     * type identifier for `T`, forwarded to `convert` (see `decodeFromBinLatest` for rationale).
     */
    public decodeFromJsonLatest<T extends BaboonGeneratedLatest>(
        value: unknown,
        targetTypeIdentifier: string,
    ): BaboonEither<BaboonCodecException, T> {
        const decoded = this.decodeFromJson(value);
        if (decoded.tag === "Left") return decoded as BaboonEither<BaboonCodecException, T>;
        const inner = decoded.value;
        return this.convert<BaboonGenerated, T>(
            inner,
            inner.baboonTypeIdentifier(),
            targetTypeIdentifier,
        );
    }

    // ----- private dispatch --------------------------------------------------------------------

    private getBinCodec(typeMeta: BaboonTypeMeta, exact: boolean): BaboonEither<BaboonCodecException, BaboonCodecData> {
        return this.getCodec(this.versionsCodecsBin, typeMeta, exact);
    }

    private getJsonCodec(typeMeta: BaboonTypeMeta, exact: boolean): BaboonEither<BaboonCodecException, BaboonCodecData> {
        return this.getCodec(this.versionsCodecsJson, typeMeta, exact);
    }

    private getCodec<TCodecs extends AbstractBaboonCodecs>(
        versionsCodecs: Map<string, Lazy<TCodecs>>,
        typeMeta: BaboonTypeMeta,
        exact: boolean,
    ): BaboonEither<BaboonCodecException, BaboonCodecData> {
        const versions = this.domainVersions.get(typeMeta.domainIdentifier);
        if (versions === undefined || versions.length === 0) {
            return leftCodecException(new BaboonCodecNotFound(`Unknown domain ${typeMeta.domainIdentifier}.`));
        }

        const minVersion = versions[0]!;
        const maxVersion = versions[versions.length - 1]!;

        const lookupVersion = typeMeta.versionRef();
        const minCompat = typeMeta.versionMinCompat();
        const modelVersion = (minCompat !== undefined && lookupVersion.version().compareTo(maxVersion.version()) > 0)
            ? minCompat
            : lookupVersion;

        const modelV = modelVersion.version();
        const maxV = maxVersion.version();
        const minV = minVersion.version();

        if (exact && modelV.compareTo(maxV) === 0) {
            return BaboonCodecsFacade.getCodecExact(versionsCodecs, modelVersion, typeMeta.typeIdentifier);
        }
        // PR-07-D02: non-exact lookup at the latest registered version routes to exact lookup.
        // Without this arm a single-version domain (min == max == model) falls through every
        // other arm because the next one's strict `<` excludes equality, producing a misleading
        // "Unsupported domain version" error. Mirrors Scala/C#/Kotlin/Java fix.
        if (!exact && modelV.compareTo(maxV) === 0) {
            return BaboonCodecsFacade.getCodecExact(versionsCodecs, modelVersion, typeMeta.typeIdentifier);
        }
        if (modelV.compareTo(minV) >= 0 && modelV.compareTo(maxV) < 0) {
            return this.getCodecMaxCompat(versionsCodecs, modelVersion, maxVersion, typeMeta.typeIdentifier);
        }
        if (modelV.compareTo(minV) < 0) {
            return this.getCodecMaxCompat(versionsCodecs, minVersion, maxVersion, typeMeta.typeIdentifier);
        }
        return leftCodecException(new BaboonCodecNotFound(`Unsupported domain version '${modelVersion}'.`));
    }

    private static getCodecExact<TCodecs extends AbstractBaboonCodecs>(
        versionsCodecs: Map<string, Lazy<TCodecs>>,
        domainVersion: BaboonDomainVersion,
        typeIdentifier: string,
    ): BaboonEither<BaboonCodecException, BaboonCodecData> {
        const lazyCodecs = versionsCodecs.get(domainVersion.key());
        if (lazyCodecs === undefined) {
            return leftCodecException(new BaboonCodecNotFound(
                `No codecs registered for domain version '${domainVersion}'.`,
            ));
        }
        const lazyCodec = lazyCodecs.value.tryFind(typeIdentifier);
        if (lazyCodec === undefined) {
            return leftCodecException(new BaboonCodecNotFound(
                `No codec found for type [${domainVersion.domainVersion}.${typeIdentifier}] of version '${domainVersion.version()}'.`,
            ));
        }
        return right(lazyCodec.value);
    }

    private getCodecMaxCompat<TCodecs extends AbstractBaboonCodecs>(
        versionsCodecs: Map<string, Lazy<TCodecs>>,
        modelVersion: BaboonDomainVersion,
        maxVersion: BaboonDomainVersion,
        typeIdentifier: string,
    ): BaboonEither<BaboonCodecException, BaboonCodecData> {
        const lazyMeta = this.versionsMeta.get(modelVersion.key());
        if (lazyMeta === undefined) {
            return leftCodecException(new BaboonCodecNotFound(`Unknown domain version '${modelVersion}'.`));
        }
        const sameVersions = lazyMeta.value.sameInVersions(typeIdentifier);
        let bestSame: string | undefined;
        for (let i = sameVersions.length - 1; i >= 0; i--) {
            const sv = sameVersions[i]!;
            if (sv === maxVersion.domainVersion || BaboonVersion.from(sv).compareTo(maxVersion.version()) <= 0) {
                bestSame = sv;
                break;
            }
        }
        if (bestSame === undefined) {
            return leftCodecException(new BaboonCodecNotFound(
                `No max compat codec found for type [${modelVersion.domainIdentifier}.${typeIdentifier}] of version '${modelVersion.domainVersion}'.`,
            ));
        }
        const maxCompatVersion = new BaboonDomainVersion(modelVersion.domainIdentifier, bestSame);
        return BaboonCodecsFacade.getCodecExact(versionsCodecs, maxCompatVersion, typeIdentifier);
    }

    private registerVersion(domainVersion: BaboonDomainVersion): void {
        const existing = this.domainVersions.get(domainVersion.domainIdentifier);
        if (existing !== undefined) {
            if (existing.some(d => d.equals(domainVersion))) return;
            const updated = [...existing, domainVersion];
            updated.sort((a, b) => a.version().compareTo(b.version()));
            this.domainVersions.set(domainVersion.domainIdentifier, updated);
        } else {
            this.domainVersions.set(domainVersion.domainIdentifier, [domainVersion]);
        }
    }
}
