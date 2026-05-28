#nullable enable

using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using Newtonsoft.Json.Linq;

// ReSharper disable UnusedTypeParameter
// ReSharper disable CheckNamespace
// ReSharper disable UnusedAutoPropertyAccessor.Global
// ReSharper disable MemberCanBeProtected.Global
// ReSharper disable MemberCanBePrivate.Global
// ReSharper disable ConvertToPrimaryConstructor
// ReSharper disable UnusedMember.Global
// ReSharper disable UnusedMemberInSuper.Global
// ReSharper disable ArrangeNamespaceBody
// ReSharper disable UnusedType.Global
// ReSharper disable InconsistentNaming
// ReSharper disable ClassCanBeSealed.Global

namespace Baboon.Runtime.Shared
{
    public class BaboonCodecsFacade
    {
        private const string CONTENT_JSON_KEY = "$c";

        private readonly ConcurrentDictionary<BaboonDomainVersion, Lazy<AbstractBaboonJsonCodecs>> _versionsCodecsJson = new();
        private readonly ConcurrentDictionary<BaboonDomainVersion, Lazy<AbstractBaboonUebaCodecs>> _versionsCodecsBin = new();
        private readonly ConcurrentDictionary<BaboonDomainVersion, Lazy<AbstractBaboonConversions>> _versionsConversions = new();
        private readonly ConcurrentDictionary<BaboonDomainVersion, Lazy<IBaboonMeta>> _versionsMeta = new();
        private readonly ConcurrentDictionary<string, List<BaboonDomainVersion>> _domainVersions = new();
        private readonly object _domainVersionsLock = new();

        /// <summary>
        /// Returns the latest registered version for the given domain identifier.
        /// </summary>
        /// <param name="domain">Domain identifier, e.g. <c>"FireSdk.Shared.Vault"</c>.</param>
        /// <returns>The highest registered version.</returns>
        /// <exception cref="Exception">No versions registered for the domain.</exception>
        public BaboonVersion Latest(string domain)
        {
            if (_domainVersions.TryGetValue(domain, out var versions) && versions.Count > 0)
            {
                return versions[^1].Version;
            }

            throw new Exception($"No registered version for {domain} domain found.");
        }

        /// <summary>
        /// Merges all registrations (codecs, conversions, meta) from another facade into this one.
        /// Used to chain child facades into a parent — e.g. registering a shared vault facade
        /// into a game-specific facade so all domain versions are accessible from a single entry point.
        /// </summary>
        /// <param name="other">Source facade whose registrations will be copied.</param>
        public void Register(BaboonCodecsFacade other)
        {
            foreach (var (id, versions) in other._domainVersions) _domainVersions[id] = versions;
            foreach (var (id, codec) in other._versionsCodecsJson) _versionsCodecsJson[id] = codec;
            foreach (var (id, codec) in other._versionsCodecsBin) _versionsCodecsBin[id] = codec;
            foreach (var (id, conv) in other._versionsConversions) _versionsConversions[id] = conv;
            foreach (var (id, meta) in other._versionsMeta) _versionsMeta[id] = meta;
        }

        /// <summary>
        /// Registers a complete domain version with all components: JSON codecs, binary codecs,
        /// conversions, and type metadata.
        /// </summary>
        /// <param name="domainVersion">The domain + version identifier to register.</param>
        /// <param name="codecsJson">Factory for lazily-initialized JSON codecs.</param>
        /// <param name="codecsBin">Factory for lazily-initialized UEBA binary codecs.</param>
        /// <param name="conversions">Factory for version conversion logic.</param>
        /// <param name="meta">Factory for type metadata (used for codec compatibility lookups).</param>
        /// <returns>The registered <c>domainVersion</c> for chaining.</returns>
        public BaboonDomainVersion Register(
            BaboonDomainVersion domainVersion,
            Func<AbstractBaboonJsonCodecs> codecsJson,
            Func<AbstractBaboonUebaCodecs> codecsBin,
            Func<AbstractBaboonConversions> conversions,
            Func<IBaboonMeta> meta
        )
        {
            RegisterVersion(domainVersion);
            _versionsCodecsJson[domainVersion] = new Lazy<AbstractBaboonJsonCodecs>(codecsJson);
            _versionsCodecsBin[domainVersion] = new Lazy<AbstractBaboonUebaCodecs>(codecsBin);
            _versionsConversions[domainVersion] = new Lazy<AbstractBaboonConversions>(conversions);
            _versionsMeta[domainVersion] = new Lazy<IBaboonMeta>(meta);
            return domainVersion;
        }

        /// <summary>
        /// Registers a domain version with JSON and binary codecs only (no conversions or meta).
        /// Useful for leaf versions that don't require conversion from prior versions.
        /// </summary>
        /// <param name="domainVersion">The domain + version identifier to register.</param>
        /// <param name="codecsJson">Factory for lazily-initialized JSON codecs.</param>
        /// <param name="codecsBin">Factory for lazily-initialized UEBA binary codecs.</param>
        /// <returns>The registered <c>domainVersion</c> for chaining.</returns>
        public BaboonDomainVersion Register(
            BaboonDomainVersion domainVersion,
            Func<AbstractBaboonJsonCodecs> codecsJson,
            Func<AbstractBaboonUebaCodecs> codecsBin
        )
        {
            RegisterVersion(domainVersion);
            _versionsCodecsJson[domainVersion] = new Lazy<AbstractBaboonJsonCodecs>(codecsJson);
            _versionsCodecsBin[domainVersion] = new Lazy<AbstractBaboonUebaCodecs>(codecsBin);
            return domainVersion;
        }

        /// <summary>
        /// Registers a domain version with JSON codecs, binary codecs, and type metadata
        /// but without conversions. Conversions can be registered separately via
        /// <c>Register(domainVersion, conversions)</c>.
        /// </summary>
        /// <param name="domainVersion">The domain + version identifier to register.</param>
        /// <param name="codecsJson">Factory for lazily-initialized JSON codecs.</param>
        /// <param name="codecsBin">Factory for lazily-initialized UEBA binary codecs.</param>
        /// <param name="meta">Factory for type metadata.</param>
        /// <returns>The registered <c>domainVersion</c> for chaining.</returns>
        public BaboonDomainVersion Register(
            BaboonDomainVersion domainVersion,
            Func<AbstractBaboonJsonCodecs> codecsJson,
            Func<AbstractBaboonUebaCodecs> codecsBin,
            Func<IBaboonMeta> meta
        )
        {
            RegisterVersion(domainVersion);
            _versionsCodecsJson[domainVersion] = new Lazy<AbstractBaboonJsonCodecs>(codecsJson);
            _versionsCodecsBin[domainVersion] = new Lazy<AbstractBaboonUebaCodecs>(codecsBin);
            _versionsMeta[domainVersion] = new Lazy<IBaboonMeta>(meta);
            return domainVersion;
        }

        /// <summary>
        /// Registers or overrides conversions for an already-registered domain version.
        /// Can be called after initial registration to supply or replace conversion logic.
        /// </summary>
        /// <param name="domainVersion">The domain + version to register conversions for.</param>
        /// <param name="conversions">Factory for the conversion logic.</param>
        /// <returns>The registered <c>domainVersion</c> for chaining.</returns>
        public BaboonDomainVersion RegisterConversions(
            BaboonDomainVersion domainVersion,
            Func<AbstractBaboonConversions> conversions
        )
        {
            RegisterVersion(domainVersion);
            _versionsConversions[domainVersion] = new Lazy<AbstractBaboonConversions>(conversions);
            return domainVersion;
        }

        /// <summary>
        /// Registers or overrides type metadata for an already-registered domain version.
        /// </summary>
        /// <param name="domainVersion">The domain + version to register metadata for.</param>
        /// <param name="meta">Factory for the type metadata.</param>
        /// <returns>The registered <c>domainVersion</c> for chaining.</returns>
        public BaboonDomainVersion RegisterMeta(
            BaboonDomainVersion domainVersion,
            Func<IBaboonMeta> meta
        )
        {
            RegisterVersion(domainVersion);
            _versionsMeta[domainVersion] = new Lazy<IBaboonMeta>(meta);
            return domainVersion;
        }

        /// <summary>
        /// Validates that every registered domain version has both conversions and type metadata.
        ///
        /// Call after all registrations are complete (typically in <c>VaultStore</c> initialization).
        /// Fails fast with a descriptive exception if any version is missing required components,
        /// preventing silent runtime failures during encode/decode/convert operations.
        /// </summary>
        /// <exception cref="BaboonCodecException.CodecNotFound">No domains registered, or metadata missing.</exception>
        /// <exception cref="BaboonCodecException.ConversionNotFound">Conversions missing for a version.</exception>
        public void Verify()
        {
            if (_domainVersions.IsEmpty)
            {
                throw new BaboonCodecException.CodecNotFound("Baboon codecs must have at least one domain registered.");
            }

            foreach (var dv in _domainVersions.Values.SelectMany(v => v))
            {
                if (!_versionsConversions.ContainsKey(dv))
                {
                    throw new BaboonCodecException.ConversionNotFound($"Baboon codecs must have conversion for {dv} registered.");
                }

                if (!_versionsMeta.ContainsKey(dv))
                {
                    throw new BaboonCodecException.CodecNotFound($"Baboon codecs must have codecs for {dv} registered.");
                }
            }
        }

        /// <summary>
        /// Eagerly initializes all registered JSON and binary codecs on a background thread.
        ///
        /// By default, codecs are lazily initialized on first encode/decode call. Preloading
        /// avoids latency spikes on the first operation by warming the caches ahead of time.
        /// Failures are silently ignored — codecs will be initialized on-demand as a fallback.
        /// </summary>
        public void Preload()
        {
            Task.Run(() =>
            {
                try
                {
                    foreach (var v in _versionsCodecsJson.Values) { _ = v.Value; }

                    foreach (var v in _versionsCodecsBin.Values) { _ = v.Value; }

                    foreach (var v in _versionsConversions.Values) { _ = v.Value; }

                    foreach (var v in _versionsMeta.Values) { _ = v.Value; }
                }
                catch
                {
                    // Preload is a hint; exceptions must not propagate.
                }
            });
        }

        /// <summary>
        /// Encodes a Baboon model to UEBA binary format and returns the byte array.
        /// </summary>
        /// <param name="ctx">Codec context.</param>
        /// <param name="value">The model to encode.</param>
        /// <returns>Encoded bytes including type metadata prefix.</returns>
        /// <exception cref="BaboonCodecException.EncoderFailure">Encoding failed.</exception>
        public Either<BaboonCodecException, byte[]> EncodeToBin<T>(BaboonCodecContext ctx, T value)
            where T : IBaboonGenerated
        {
            return EncodeToBin(ctx, value, null);
        }

        /// <summary>
        /// Encodes a Baboon model to UEBA binary format and returns the byte array.
        /// </summary>
        /// <param name="ctx">Codec context.</param>
        /// <param name="value">The model to encode.</param>
        /// <param name="typeMetaOverride">Optional override for the type metadata prefix.</param>
        /// <returns>Encoded bytes including type metadata prefix.</returns>
        /// <exception cref="BaboonCodecException.EncoderFailure">Encoding failed.</exception>
        public Either<BaboonCodecException, byte[]> EncodeToBin<T>(
            BaboonCodecContext ctx,
            T value,
            BaboonTypeMeta? typeMetaOverride
        )
            where T : IBaboonGenerated
        {
            using var ms = new MemoryStream();
            using var writer = new BinaryWriter(ms);
            var result = EncodeToBin(ctx, writer, value, typeMetaOverride);
            if (result.IsLeft)
            {
                return Either.Left<BaboonCodecException, byte[]>(result.GetLeft());
            }

            writer.Flush();
            return Either.Right<BaboonCodecException, byte[]>(ms.ToArray());
        }

        /// <summary>
        /// Encodes a Baboon model to UEBA binary format and writes it to the given writer.
        /// Type metadata is written first, followed by the encoded content.
        /// </summary>
        /// <param name="ctx">Codec context (e.g. <c>BaboonCodecContext.Compact</c>).</param>
        /// <param name="writer">Target binary writer.</param>
        /// <param name="value">The model to encode.</param>
        /// <exception cref="BaboonCodecException.EncoderFailure">Encoding failed.</exception>
        public Either<BaboonCodecException, Unit> EncodeToBin<T>(BaboonCodecContext ctx, BinaryWriter writer, T value)
            where T : IBaboonGenerated
        {
            return EncodeToBin(ctx, writer, value, null);
        }

        /// <summary>
        /// Encodes a Baboon model to UEBA binary format and writes it to the given writer.
        /// Type metadata is written first, followed by the encoded content.
        /// </summary>
        /// <param name="ctx">Codec context (e.g. <c>BaboonCodecContext.Compact</c>).</param>
        /// <param name="writer">Target binary writer.</param>
        /// <param name="value">The model to encode.</param>
        /// <param name="typeMetaOverride">Optional override for the type metadata prefix.</param>
        /// <exception cref="BaboonCodecException.EncoderFailure">Encoding failed.</exception>
        public Either<BaboonCodecException, Unit> EncodeToBin<T>(BaboonCodecContext ctx, BinaryWriter writer, T value, BaboonTypeMeta? typeMetaOverride)
            where T : IBaboonGenerated
        {
            var typeMeta = BaboonTypeMeta.From(value, typeof(T));
            var codecResult = GetBinCodec(typeMeta, exact: true);
            if (codecResult.IsLeft)
            {
                return Either.Left<BaboonCodecException, Unit>(codecResult.GetLeft());
            }

            var codec = codecResult.GetRight();
            try
            {
                (typeMetaOverride ?? typeMeta).WriteBin(writer);
                codec.Encode(ctx, writer, value);
                return Either.Right<BaboonCodecException, Unit>(Unit.Default);
            }
            catch (Exception e)
            {
                return Either.Left<BaboonCodecException, Unit>(new BaboonCodecException.EncoderFailure(
                    $"Exception while trying to encode to binary form type [{typeMeta.DomainIdentifier}.{typeMeta.TypeIdentifier}] of version '{typeMeta.DomainVersion}'.",
                    e
                ));
            }
        }

        /// <summary>
        /// Decodes an <c>AnyOpaque</c> payload by resolving its meta to a synthetic
        /// <c>BaboonTypeMeta</c> and dispatching to the matching JSON or UEBA codec.
        /// </summary>
        public Either<BaboonCodecException, IBaboonGenerated> DecodeAny(AnyOpaque opaque)
        {
            var meta = opaque.Meta;
            var metaResult = BuildSyntheticTypeMeta(meta);
            if (metaResult.IsLeft)
            {
                return Either.Left<BaboonCodecException, IBaboonGenerated>(metaResult.GetLeft());
            }

            var typeMeta = metaResult.GetRight();
            switch (opaque)
            {
                case AnyOpaqueUeba u:
                {
                    var codecResult = GetBinCodec(typeMeta, exact: false);
                    if (codecResult.IsLeft)
                    {
                        return Either.Left<BaboonCodecException, IBaboonGenerated>(codecResult.GetLeft());
                    }

                    var codec = codecResult.GetRight();
                    try
                    {
                        using var ms = new MemoryStream(u.Bytes);
                        using var reader = new BinaryReader(ms);
                        var decoded = codec.Decode(BaboonCodecContext.Compact, reader);
                        return Either.Right<BaboonCodecException, IBaboonGenerated>(decoded);
                    }
                    catch (Exception e)
                    {
                        return Either.Left<BaboonCodecException, IBaboonGenerated>(new BaboonCodecException.DecoderFailure(
                            $"DecodeAny: cannot decode UEBA payload of type [{typeMeta.DomainIdentifier}.{typeMeta.TypeIdentifier}] of version '{typeMeta.DomainVersion}'.",
                            e
                        ));
                    }
                }
                case AnyOpaqueJson j:
                {
                    var codecResult = GetJsonCodec(typeMeta, exact: false);
                    if (codecResult.IsLeft)
                    {
                        return Either.Left<BaboonCodecException, IBaboonGenerated>(codecResult.GetLeft());
                    }

                    var codec = codecResult.GetRight();
                    try
                    {
                        var decoded = codec.Decode(BaboonCodecContext.Compact, j.Json);
                        return Either.Right<BaboonCodecException, IBaboonGenerated>(decoded);
                    }
                    catch (Exception e)
                    {
                        return Either.Left<BaboonCodecException, IBaboonGenerated>(new BaboonCodecException.DecoderFailure(
                            $"DecodeAny: cannot decode JSON payload of type [{typeMeta.DomainIdentifier}.{typeMeta.TypeIdentifier}] of version '{typeMeta.DomainVersion}'.",
                            e
                        ));
                    }
                }
                default:
                    return Either.Left<BaboonCodecException, IBaboonGenerated>(
                        new BaboonCodecException.DecoderFailure(
                            $"DecodeAny: unknown AnyOpaque branch {opaque.GetType().Name}"));
            }
        }

        /// <summary>
        /// Cross-format helper: decode an <c>AnyOpaqueJson</c> payload via the registered JSON codec, then
        /// re-encode it via the registered UEBA codec. The wire <c>meta</c> may omit components that the
        /// field's static declaration already pins down (variants B/C/D1/D2/D3). The codec generator
        /// passes the static fallbacks; runtime <c>meta.X</c> takes precedence over <c>staticX</c> to preserve
        /// override semantics. PR-06-D01 fix: without static fallback only variant A would work.
        ///
        /// Default <c>null</c> values are deliberately permitted on this user-facing helper; the codec
        /// generator always supplies all three. User code calling this directly must provide whatever
        /// static context it has, or accept the all-null semantics (= no static fallback).
        /// </summary>
        public Either<BaboonCodecException, byte[]> JsonToUebaBytes(
            AnyMeta meta,
            JToken json,
            string? staticDomain = null,
            string? staticVersion = null,
            string? staticTypeid = null
        )
        {
            var metaResult = BuildSyntheticTypeMeta(meta, staticDomain, staticVersion, staticTypeid);
            if (metaResult.IsLeft)
            {
                return Either.Left<BaboonCodecException, byte[]>(metaResult.GetLeft());
            }

            var typeMeta = ((Either<BaboonCodecException, BaboonTypeMeta>.Right) metaResult).Value;

            var jsonCodecResult = GetJsonCodec(typeMeta, exact: false);
            if (jsonCodecResult.IsLeft)
            {
                return Either.Left<BaboonCodecException, byte[]>(jsonCodecResult.GetLeft());
            }

            var binCodecResult = GetBinCodec(typeMeta, exact: false);
            if (binCodecResult.IsLeft)
            {
                return Either.Left<BaboonCodecException, byte[]>(binCodecResult.GetLeft());
            }

            var jsonCodec = jsonCodecResult.GetRight();
            var binCodec = binCodecResult.GetRight();
            IBaboonGenerated typed;
            try
            {
                typed = jsonCodec.Decode(BaboonCodecContext.Compact, json);
            }
            catch (Exception e)
            {
                return Either.Left<BaboonCodecException, byte[]>(new BaboonCodecException.DecoderFailure(
                    $"JsonToUebaBytes: cannot decode JSON payload of type [{typeMeta.DomainIdentifier}.{typeMeta.TypeIdentifier}] of version '{typeMeta.DomainVersion}'.",
                    e
                ));
            }

            try
            {
                using var ms = new MemoryStream();
                using var writer = new BinaryWriter(ms);
                binCodec.Encode(BaboonCodecContext.Compact, writer, typed);
                writer.Flush();
                return Either.Right<BaboonCodecException, byte[]>(ms.ToArray());
            }
            catch (Exception e)
            {
                return Either.Left<BaboonCodecException, byte[]>(new BaboonCodecException.EncoderFailure(
                    $"JsonToUebaBytes: cannot encode UEBA payload of type [{typeMeta.DomainIdentifier}.{typeMeta.TypeIdentifier}] of version '{typeMeta.DomainVersion}'.",
                    e
                ));
            }
        }

        /// <summary>
        /// Cross-format helper symmetric to <c>JsonToUebaBytes</c>. See its documentation for the
        /// static-fallback contract.
        /// </summary>
        public Either<BaboonCodecException, JToken> UebaToJson(
            AnyMeta meta,
            byte[] bytes,
            string? staticDomain = null,
            string? staticVersion = null,
            string? staticTypeid = null
        )
        {
            var metaResult = BuildSyntheticTypeMeta(meta, staticDomain, staticVersion, staticTypeid);
            if (metaResult.IsLeft)
            {
                return Either.Left<BaboonCodecException, JToken>(metaResult.GetLeft());
            }

            var typeMeta = metaResult.GetRight();

            var binCodecResult = GetBinCodec(typeMeta, exact: false);
            if (binCodecResult.IsLeft)
            {
                return Either.Left<BaboonCodecException, JToken>(binCodecResult.GetLeft());
            }

            var jsonCodecResult = GetJsonCodec(typeMeta, exact: false);
            if (jsonCodecResult.IsLeft)
            {
                return Either.Left<BaboonCodecException, JToken>(jsonCodecResult.GetLeft());
            }

            var binCodec = binCodecResult.GetRight();
            var jsonCodec = jsonCodecResult.GetRight();

            IBaboonGenerated typed;
            try
            {
                using var ms = new MemoryStream(bytes);
                using var reader = new BinaryReader(ms);
                typed = binCodec.Decode(BaboonCodecContext.Compact, reader);
            }
            catch (Exception e)
            {
                return Either.Left<BaboonCodecException, JToken>(new BaboonCodecException.DecoderFailure(
                    $"UebaToJson: cannot decode UEBA payload of type [{typeMeta.DomainIdentifier}.{typeMeta.TypeIdentifier}] of version '{typeMeta.DomainVersion}'.",
                    e
                ));
            }

            try
            {
                var json = jsonCodec.Encode(BaboonCodecContext.Compact, typed);
                return Either.Right<BaboonCodecException, JToken>(json);
            }
            catch (Exception e)
            {
                return Either.Left<BaboonCodecException, JToken>(new BaboonCodecException.EncoderFailure(
                    $"UebaToJson: cannot encode JSON payload of type [{typeMeta.DomainIdentifier}.{typeMeta.TypeIdentifier}] of version '{typeMeta.DomainVersion}'.",
                    e
                ));
            }
        }

        /// <summary>
        /// Synthesise a <c>BaboonTypeMeta</c> from an <c>AnyMeta</c> plus optional static fallbacks.
        /// <c>AnyMeta</c> does not carry a min-compat version; forward-version migration is unavailable for
        /// any-payloads, so <c>domainVersionMinCompat = version</c>. <c>meta.X</c> takes precedence over <c>staticX</c>
        /// when both are present (override semantics — wire data wins). <c>DecodeAny</c> calls this with all
        /// null statics so its limitation to variant A is preserved.
        /// </summary>
        private static Either<BaboonCodecException, BaboonTypeMeta> BuildSyntheticTypeMeta(
            AnyMeta meta,
            string? staticDomain = null,
            string? staticVersion = null,
            string? staticTypeid = null
        )
        {
            var domain = meta.Domain ?? staticDomain;
            var version = meta.Version ?? staticVersion;
            var typeid = meta.Typeid ?? staticTypeid;

            if (domain is not null && version is not null && typeid is not null)
            {
                return Either.Right<BaboonCodecException, BaboonTypeMeta>(
                    new BaboonTypeMeta(BaboonTypeMetaCodec.META_VERSION, domain, version, version, typeid));
            }

            var missing = new List<string>();
            if (domain is null) missing.Add("domain");
            if (version is null) missing.Add("version");
            if (typeid is null) missing.Add("typeid");
            return Either.Left<BaboonCodecException, BaboonTypeMeta>(new BaboonCodecException.DecoderFailure(
                $"AnyMeta requires domain/version/typeid for facade resolution; got kind 0x{(meta.Kind & 0xFF):x} which lacks: {string.Join(", ", missing)}"
            ));
        }

        /// <summary>
        /// Decodes a Baboon model from UEBA binary format. Reads type metadata from the stream
        /// to determine the codec, then decodes the content. Returns the model at its original
        /// version — use <c>Convert</c> or <c>DecodeFromBinLatest</c> to upgrade.
        /// </summary>
        /// <param name="reader">Binary reader positioned at the start of the encoded data.</param>
        /// <returns>The decoded model at its original version.</returns>
        /// <exception cref="BaboonCodecException.CodecNotFound">No codec for the encoded type/version.</exception>
        /// <exception cref="BaboonCodecException.DecoderFailure">Decoding failed.</exception>
        public Either<BaboonCodecException, IBaboonGenerated> DecodeFromBin(BinaryReader reader)
        {
            var typeMeta = BaboonTypeMeta.ReadMeta(reader);
            if (typeMeta is null)
            {
                return Either.Left<BaboonCodecException, IBaboonGenerated>(new BaboonCodecException.DecoderFailure("Cannot decode binary type meta"));
            }

            var codecResult = GetBinCodec(typeMeta, exact: false);
            if (codecResult.IsLeft)
            {
                return Either.Left<BaboonCodecException, IBaboonGenerated>(codecResult.GetLeft());
            }

            var codec = codecResult.GetRight();
            try
            {
                var decoded = codec.Decode(BaboonCodecContext.Compact, reader);
                return Either.Right<BaboonCodecException, IBaboonGenerated>(decoded);
            }
            catch (Exception ex)
            {
                return Either.Left<BaboonCodecException, IBaboonGenerated>(new BaboonCodecException.DecoderFailure(
                    $"Can not decode BIN form type [{typeMeta.DomainIdentifier}.{typeMeta.TypeIdentifier}] of version '{typeMeta.DomainVersion}'.",
                    ex
                ));
            }
        }

        /// <summary>
        /// Decodes a Baboon model from a UEBA binary byte array at its original version.
        /// </summary>
        public Either<BaboonCodecException, IBaboonGenerated> DecodeFromBin(byte[] bytes)
        {
            using var ms = new MemoryStream(bytes);
            using var reader = new BinaryReader(ms);
            return DecodeFromBin(reader);
        }

        /// <summary>
        /// Decodes from a binary reader and converts to the latest version of <c>TTo</c>. Throws on failure.
        /// </summary>
        public Either<BaboonCodecException, TTo> DecodeFromBinLatest<TTo>(BinaryReader reader)
            where TTo : IBaboonGeneratedLatest
        {
            var decodeResult = DecodeFromBin(reader);
            if (decodeResult.IsLeft)
            {
                return Either.Left<BaboonCodecException, TTo>(decodeResult.GetLeft());
            }

            var decoded = decodeResult.GetRight();
            return Convert<IBaboonGenerated, TTo>(decoded);
        }

        /// <summary>
        /// Decodes from a binary byte array and converts to the latest version of <c>TTo</c>. Throws on failure.
        /// </summary>
        public Either<BaboonCodecException, TTo> DecodeFromBinLatest<TTo>(byte[] bytes)
            where TTo : IBaboonGeneratedLatest
        {
            using var ms = new MemoryStream(bytes);
            using var reader = new BinaryReader(ms);
            return DecodeFromBinLatest<TTo>(reader);
        }

        /// <summary>
        /// Encodes a Baboon model to JSON. The result is a <c>JObject</c> containing type metadata
        /// fields and a <c>$c</c> key with the encoded content.
        /// </summary>
        /// <param name="value">The model to encode.</param>
        /// <returns>JSON token with metadata and encoded content.</returns>
        /// <exception cref="BaboonCodecException.EncoderFailure">Encoding failed.</exception>
        public Either<BaboonCodecException, JToken> EncodeToJson<T>(T value)
            where T : IBaboonGenerated
        {
            return EncodeToJson(value, null);
        }

        /// <summary>
        /// Encodes a Baboon model to JSON. The result is a <c>JObject</c> containing type metadata
        /// fields and a <c>$c</c> key with the encoded content.
        /// </summary>
        /// <param name="value">The model to encode.</param>
        /// <param name="typeMetaOverride">Optional override for the embedded type metadata.</param>
        /// <returns>JSON token with metadata and encoded content.</returns>
        /// <exception cref="BaboonCodecException.EncoderFailure">Encoding failed.</exception>
        public Either<BaboonCodecException, JToken> EncodeToJson<T>(T value, BaboonTypeMeta? typeMetaOverride)
            where T : IBaboonGenerated
        {
            var typeMeta = BaboonTypeMeta.From(value, typeof(T));
            var codecResult = GetJsonCodec(typeMeta, exact: true);
            if (codecResult.IsLeft)
            {
                return Either.Left<BaboonCodecException, JToken>(codecResult.GetLeft());
            }

            var codec = codecResult.GetRight();
            try
            {
                var content = codec.Encode(BaboonCodecContext.Compact, value);
                var metaJson = (typeMetaOverride ?? typeMeta).WriteJson();
                if (metaJson is not JObject metaObj)
                {
                    return Either.Left<BaboonCodecException, JToken>(new BaboonCodecException.EncoderFailure(
                        $"BaboonTypeMeta.WriteJson must return a JObject; got {metaJson.Type}"
                    ));
                }

                metaObj[CONTENT_JSON_KEY] = content;
                return Either.Right<BaboonCodecException, JToken>(metaObj);
            }
            catch (Exception e)
            {
                return Either.Left<BaboonCodecException, JToken>(new BaboonCodecException.EncoderFailure(
                    $"Can not encode to json form type [{value.BaboonTypeIdentifier()}] of version '{value.BaboonDomainVersion()}'.",
                    e
                ));
            }
        }

        /// <summary>
        /// Decodes a Baboon model from a JSON token at its original version.
        ///
        /// Expects a <c>JObject</c> with type metadata fields and a <c>$c</c> content key
        /// (the format produced by <c>EncodeToJson</c>). Returns <c>null</c> if the JSON
        /// doesn't match the expected Baboon envelope format.
        /// </summary>
        /// <param name="value">JSON token to decode.</param>
        /// <returns>The decoded model at its original version, or <c>null</c> if not a valid Baboon envelope.</returns>
        /// <exception cref="BaboonCodecException.CodecNotFound">No codec for the type/version in the JSON.</exception>
        /// <exception cref="BaboonCodecException.DecoderFailure">Decoding failed.</exception>
        public Either<BaboonCodecException, IBaboonGenerated?> DecodeFromJson(JToken value)
        {
            // Mirrors Scala: returns Right(None) when the JSON is not a meta envelope at all
            // (no readable type meta or no $c content key). Codec lookup failures are still Left.
            var typeMeta = BaboonTypeMeta.ReadMeta(value);
            if (typeMeta is null) return Either.Right<BaboonCodecException, IBaboonGenerated?>(null);

            if (value is not JObject jObj || jObj[CONTENT_JSON_KEY] is null)
            {
                return Either.Right<BaboonCodecException, IBaboonGenerated?>(null);
            }

            var contentToken = jObj[CONTENT_JSON_KEY]!;

            var codecResult = GetJsonCodec(typeMeta, exact: false);
            if (codecResult.IsLeft)
            {
                return Either.Left<BaboonCodecException, IBaboonGenerated?>(codecResult.GetLeft());
            }

            var codec = codecResult.GetRight();
            try
            {
                var decoded = codec.Decode(BaboonCodecContext.Compact, contentToken);
                return Either.Right<BaboonCodecException, IBaboonGenerated?>(decoded);
            }
            catch (Exception e)
            {
                return Either.Left<BaboonCodecException, IBaboonGenerated?>(new BaboonCodecException.DecoderFailure(
                    $"Can not decode JSON form type [{typeMeta.DomainIdentifier}.{typeMeta.TypeIdentifier}] of version '{typeMeta.DomainVersion}'. JSON: {value}",
                    e
                ));
            }
        }

        /// <summary>
        /// Decodes a Baboon model from a JSON string. Returns Right(null) when the input is not a meta envelope.
        /// </summary>
        public Either<BaboonCodecException, IBaboonGenerated?> DecodeFromJson(string value)
        {
            JToken parsed;
            try
            {
                parsed = JToken.Parse(value);
            }
            catch (Exception e)
            {
                return Either.Left<BaboonCodecException, IBaboonGenerated?>(new BaboonCodecException.DecoderFailure(
                    $"Cannot parse JSON: {e.Message}", e
                ));
            }

            return DecodeFromJson(parsed);
        }

        /// <summary>
        /// Decodes from a JSON token and converts to the latest version of <c>TTo</c>. Throws on failure.
        /// </summary>
        public Either<BaboonCodecException, TTo?> DecodeFromJsonLatest<TTo>(JToken value)
            where TTo : class, IBaboonGeneratedLatest
        {
            var decodeResult = DecodeFromJson(value);
            if (decodeResult.IsLeft)
            {
                return Either.Left<BaboonCodecException, TTo?>(decodeResult.GetLeft());
            }

            var decoded = decodeResult.GetRight();
            if (decoded is null)
            {
                return Either.Right<BaboonCodecException, TTo?>(null);
            }

            var convertResult = Convert<IBaboonGenerated, TTo>(decoded);
            if (convertResult.IsLeft)
            {
                return Either.Left<BaboonCodecException, TTo?>(convertResult.GetLeft());
            }

            return Either.Right<BaboonCodecException, TTo?>(convertResult.GetRight());
        }

        /// <summary>
        /// Decodes from JSON string and converts to the latest version of <c>TTo</c>. Throws on failure.
        /// </summary>
        public Either<BaboonCodecException, TTo?> DecodeFromJsonLatest<TTo>(string value)
            where TTo : class, IBaboonGeneratedLatest
        {
            JToken parsed;
            try
            {
                parsed = JToken.Parse(value);
            }
            catch (Exception e)
            {
                return Either.Left<BaboonCodecException, TTo?>(new BaboonCodecException.DecoderFailure(
                    $"Cannot parse JSON: {e.Message}",
                    e
                ));
            }

            return DecodeFromJsonLatest<TTo>(parsed);
        }

        public Either<BaboonCodecException, TTo> Convert<TFrom, TTo>(TFrom value)
            where TFrom : IBaboonGenerated
            where TTo : IBaboonGenerated
        {
            // Identity short-circuit: target already matches.
            if (value is TTo direct) return Either.Right<BaboonCodecException, TTo>(direct);

            var dvFrom = new BaboonDomainVersion(value.BaboonDomainIdentifier(), value.BaboonDomainVersion());

            if (!_domainVersions.TryGetValue(dvFrom.DomainIdentifier, out var versions) || versions.Count == 0)
            {
                return Either.Left<BaboonCodecException, TTo>(new BaboonCodecException.ConverterFailure(
                    $"Unknown domain '{dvFrom.DomainIdentifier}'."
                ));
            }

            if (versions.All(v => v != dvFrom))
            {
                return Either.Left<BaboonCodecException, TTo>(new BaboonCodecException.ConverterFailure(
                    $"Unknown domain version' {dvFrom}'."
                ));
            }

            // Iterate from first to latest, applying each step's conversion when needed.
            IBaboonGenerated current = value;
            foreach (var toVersion in versions)
            {
                // Mid-walk early-exit: if the current value already matches the target type
                // (e.g. an intermediate non-latest TO has been reached), stop walking and return.
                // Matches OLD facade semantics for non-latest TO calls.
                if (current is TTo midResult) return Either.Right<BaboonCodecException, TTo>(midResult);

                if (
                    current.BaboonDomainVersion() == toVersion.DomainVersion ||
                    new BaboonDomainVersion(current.BaboonDomainIdentifier(), current.BaboonDomainVersion()).Version >= toVersion.Version
                )
                {
                    continue;
                }

                if (!_versionsConversions.TryGetValue(toVersion, out var lazyConv))
                {
                    return Either.Left<BaboonCodecException, TTo>(new BaboonCodecException.ConverterFailure(
                        $"Can not find version '{toVersion}' conversions."
                    ));
                }

                var conversions = lazyConv.Value;
                var typeConversions = conversions.FindConversions(current);
                IConversion? bestConversion = null;
                BaboonVersion? bestVersion = null;
                foreach (var c in typeConversions)
                {
                    var matchesType = c.TypeFrom() == current.GetType() || IsAdtConversion(current, c);
                    if (!matchesType) continue;
                    var v = BaboonVersion.From(c.VersionTo());
                    if (bestVersion is null || v > bestVersion)
                    {
                        bestVersion = v;
                        bestConversion = c;
                    }
                }

                if (bestConversion is null)
                {
                    return Either.Left<BaboonCodecException, TTo>(new BaboonCodecException.ConverterFailure(
                        $"Can not find version '{toVersion}' type [{current.GetType().FullName}] conversions."
                    ));
                }

                try
                {
                    current = conversions.Convert(current, bestConversion);
                }
                catch (Exception e)
                {
                    return Either.Left<BaboonCodecException, TTo>(new BaboonCodecException.ConverterFailure(
                        $"Exception while converting type [{current.GetType().FullName}] of version '{current.BaboonDomainVersion()}' to version '{toVersion}'.",
                        e
                    ));
                }
            }

            if (current is TTo result) return Either.Right<BaboonCodecException, TTo>(result);
            return Either.Left<BaboonCodecException, TTo>(new BaboonCodecException.ConverterFailure(
                $"Expected to have type [{typeof(TTo).FullName}] at the end, but got [{current.GetType().FullName}]."
            ));
        }

        /// <summary>
        /// Convert and classify failures: <c>Deprecated</c> = source type removed in a newer
        /// version (no conversion path), <c>Failed</c> = any other converter/codec exception,
        /// <c>Success</c> = converted value.
        /// </summary>
        public BaboonConvertResult<TTo> ConvertClassified<TFrom, TTo>(TFrom value)
            where TFrom : class, IBaboonGenerated
            where TTo : class, IBaboonGeneratedLatest
        {
            if (IsDeprecated(value))
            {
                return new BaboonConvertResult<TTo>.Deprecated();
            }

            var result = Convert<TFrom, TTo>(value);
            switch (result)
            {
                case Either<BaboonCodecException, TTo>.Right right:
                    return new BaboonConvertResult<TTo>.Success(right.Value);
                case Either<BaboonCodecException, TTo>.Left left:
                    return new BaboonConvertResult<TTo>.Failed(left.Value);
                default:
                    return new BaboonConvertResult<TTo>.Failed(
                        new BaboonCodecException.ConverterFailure($"Unexpected Either variant from Convert: {result.GetType().FullName}")
                    );
            }
        }

        /// <summary>
        /// Walks the registered conversion chain without executing converters: returns <c>true</c>
        /// when any step is missing the type (no path to latest). Unknown domains and already-latest
        /// models return <c>false</c>.
        /// </summary>
        public bool IsDeprecated(IBaboonGenerated value)
        {
            if (value is IBaboonGeneratedLatest) return false;

            var dvFrom = new BaboonDomainVersion(value.BaboonDomainIdentifier(), value.BaboonDomainVersion());
            if (!_domainVersions.TryGetValue(dvFrom.DomainIdentifier, out var versions) || versions.Count == 0)
            {
                return false;
            }

            var currentType = value.GetType();
            var adtType = (value as IBaboonAdtMemberMeta)?.BaboonAdtType();

            foreach (var toVersion in versions)
            {
                if (dvFrom.Version >= toVersion.Version) continue;

                if (!_versionsConversions.TryGetValue(toVersion, out var lazyConversions))
                {
                    return true;
                }

                var typeConversions = lazyConversions.Value.FindConversions(value);
                var hasMatch = false;
                foreach (var typeConversion in typeConversions)
                {
                    var typeFrom = typeConversion.TypeFrom();
                    if (typeFrom == currentType || (adtType != null && typeFrom == adtType))
                    {
                        hasMatch = true;
                        break;
                    }
                }

                if (!hasMatch) return true;
            }

            return false;
        }

        private static bool IsAdtConversion(IBaboonGenerated instance, IConversion conversion)
        {
            return instance is IBaboonAdtMemberMeta adt && adt.BaboonAdtType() == conversion.TypeFrom();
        }

        private Either<BaboonCodecException, IBaboonStreamCodec<IBaboonGenerated, BinaryWriter, BinaryReader>> GetBinCodec(BaboonTypeMeta typeMeta, bool exact)
        {
            var codec = GetCodec(_versionsCodecsBin, typeMeta, exact);
            if (codec.IsLeft)
            {
                return Either.Left<BaboonCodecException, IBaboonStreamCodec<IBaboonGenerated, BinaryWriter, BinaryReader>>(
                    codec.GetLeft()
                );
            }

            if (codec.GetRight() is not IBaboonStreamCodec<IBaboonGenerated, BinaryWriter, BinaryReader> binCodec)
            {
                return Either.Left<BaboonCodecException, IBaboonStreamCodec<IBaboonGenerated, BinaryWriter, BinaryReader>>(new BaboonCodecException.CodecNotFound(
                    $"Unexpected codec type [{codec.GetType().FullName}]."
                ));
            }

            return Either.Right<BaboonCodecException, IBaboonStreamCodec<IBaboonGenerated, BinaryWriter, BinaryReader>>(binCodec);
        }

        private Either<BaboonCodecException, IBaboonValueCodec<IBaboonGenerated, JToken>> GetJsonCodec(BaboonTypeMeta typeMeta, bool exact)
        {
            var codec = GetCodec(_versionsCodecsJson, typeMeta, exact);
            if (codec.IsLeft)
            {
                return Either.Left<BaboonCodecException, IBaboonValueCodec<IBaboonGenerated, JToken>>(
                    codec.GetLeft()
                );
            }

            if (codec.GetRight() is not IBaboonValueCodec<IBaboonGenerated, JToken> jsonCodec)
            {
                return Either.Left<BaboonCodecException, IBaboonValueCodec<IBaboonGenerated, JToken>>(new BaboonCodecException.CodecNotFound(
                    $"Unexpected codec type [{codec.GetType().FullName}]."
                ));
            }

            return Either.Right<BaboonCodecException, IBaboonValueCodec<IBaboonGenerated, JToken>>(jsonCodec);
        }

        private Either<BaboonCodecException, IBaboonCodecData> GetCodec<TCodecs>(
            ConcurrentDictionary<BaboonDomainVersion, Lazy<TCodecs>> versionsCodecs,
            BaboonTypeMeta typeMeta,
            bool exact
        )
            where TCodecs : AbstractBaboonCodecs
        {
            if (!_domainVersions.TryGetValue(typeMeta.DomainIdentifier, out var versions) || versions.Count == 0)
            {
                return Either.Left<BaboonCodecException, IBaboonCodecData>(new BaboonCodecException.CodecNotFound($"Unknown domain {typeMeta.DomainIdentifier}."));
            }

            var minVersion = versions[0];
            var maxVersion = versions[^1];

            BaboonDomainVersion modelVersion;
            var lookupVersion = typeMeta.VersionRef;
            var minCompat = typeMeta.VersionMinCompat;
            if (minCompat is not null && lookupVersion.Version > maxVersion.Version)
            {
                modelVersion = minCompat;
            }
            else
            {
                modelVersion = lookupVersion;
            }

            var modelV = modelVersion.Version;
            var maxV = maxVersion.Version;
            var minV = minVersion.Version;

            // exact=true: caller knows the model version and wants its codec — never substitute.
            // exact=false at the latest registered version (PR-07-D02): single-version-domain case;
            // would otherwise fall through every range arm because the strictly-less-than bound on
            // the next arm excludes equality.
            if (exact || modelV.CompareTo(maxV) == 0)
            {
                return GetCodecExact(versionsCodecs, modelVersion, typeMeta.TypeIdentifier);
            }

            if (modelV >= minV && modelV < maxV)
            {
                return GetCodecMaxCompat(versionsCodecs, modelVersion, maxVersion, typeMeta.TypeIdentifier);
            }

            if (modelV < minV)
            {
                return GetCodecMaxCompat(versionsCodecs, minVersion, maxVersion, typeMeta.TypeIdentifier);
            }

            return Either.Left<BaboonCodecException, IBaboonCodecData>(new BaboonCodecException.CodecNotFound(
                $"Unsupported domain version '{modelVersion}'."
            ));
        }

        private static Either<BaboonCodecException, IBaboonCodecData> GetCodecExact<TCodecs>(
            ConcurrentDictionary<BaboonDomainVersion, Lazy<TCodecs>> versionsCodecs,
            BaboonDomainVersion domainVersion,
            string typeIdentifier
        )
            where TCodecs : AbstractBaboonCodecs
        {
            if (!versionsCodecs.TryGetValue(domainVersion, out var lazyCodecs))
            {
                return Either.Left<BaboonCodecException, IBaboonCodecData>(new BaboonCodecException.CodecNotFound(
                    $"No codecs registered for domain version '{domainVersion}'."
                ));
            }

            if (!lazyCodecs.Value.TryFind(typeIdentifier, out var lazyCodec) || lazyCodec is null)
            {
                return Either.Left<BaboonCodecException, IBaboonCodecData>(new BaboonCodecException.CodecNotFound(
                    $"No codec found for type [{domainVersion.DomainIdentifier}.{typeIdentifier}] of version '{domainVersion.Version}'."
                ));
            }

            return Either.Right<BaboonCodecException, IBaboonCodecData>(lazyCodec.Value);
        }

        private Either<BaboonCodecException, IBaboonCodecData> GetCodecMaxCompat<TCodecs>(
            ConcurrentDictionary<BaboonDomainVersion, Lazy<TCodecs>> versionsCodecs,
            BaboonDomainVersion modelVersion,
            BaboonDomainVersion maxVersion,
            string typeIdentifier
        )
            where TCodecs : AbstractBaboonCodecs
        {
            if (!_versionsMeta.TryGetValue(modelVersion, out var lazyMeta))
            {
                return Either.Left<BaboonCodecException, IBaboonCodecData>(new BaboonCodecException.CodecNotFound(
                    $"Unknown domain version '{modelVersion}'."
                ));
            }

            var sameVersions = lazyMeta.Value.SameInVersions(typeIdentifier);
            string? bestSame = null;
            for (var i = sameVersions.Count - 1; i >= 0; i--)
            {
                var sv = sameVersions[i];
                if (sv == maxVersion.DomainVersion || BaboonVersion.From(sv) <= maxVersion.Version)
                {
                    bestSame = sv;
                    break;
                }
            }

            if (bestSame is null)
            {
                return Either.Left<BaboonCodecException, IBaboonCodecData>(new BaboonCodecException.CodecNotFound(
                    $"No max compat codec found for type [{modelVersion.DomainIdentifier}.{typeIdentifier}] of version '{modelVersion.DomainVersion}'."
                ));
            }

            var maxCompatVersion = new BaboonDomainVersion(modelVersion.DomainIdentifier, bestSame);
            return GetCodecExact(versionsCodecs, maxCompatVersion, typeIdentifier);
        }

        private void RegisterVersion(BaboonDomainVersion domainVersion)
        {
            // Sort-by-version on insert so getCodec can read first/last as min/max in one pass.
            // Compute under a lock to avoid two concurrent inserts producing an unsorted list.
            lock (_domainVersionsLock)
            {
                if (_domainVersions.TryGetValue(domainVersion.DomainIdentifier, out var existing))
                {
                    if (existing.Contains(domainVersion)) return;
                    var updated = new List<BaboonDomainVersion>(existing) {domainVersion};
                    updated.Sort((a, b) => a.Version.CompareTo(b.Version));
                    _domainVersions[domainVersion.DomainIdentifier] = updated;
                }
                else
                {
                    _domainVersions[domainVersion.DomainIdentifier] = new List<BaboonDomainVersion> {domainVersion};
                }
            }
        }
    }

    /// <summary>
    /// Closed-hierarchy result of <c>BaboonCodecsFacade.ConvertClassified</c>.
    /// <c>Success</c> = converted value at the latest version.
    /// <c>Deprecated</c> = source type removed in a newer version (permanent).
    /// <c>Failed</c> = any other exception (treat as transient).
    /// </summary>
    public abstract record BaboonConvertResult<TO> where TO : class
    {
        private BaboonConvertResult()
        {
        }

        public sealed record Success(TO Value) : BaboonConvertResult<TO>;

        public sealed record Deprecated : BaboonConvertResult<TO>;

        public sealed record Failed(Exception Exception) : BaboonConvertResult<TO>;
    }
}
