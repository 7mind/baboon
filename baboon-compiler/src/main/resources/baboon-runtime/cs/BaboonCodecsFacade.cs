#nullable enable

using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;
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

        public BaboonVersion Latest(string domain)
        {
            if (_domainVersions.TryGetValue(domain, out var versions) && versions.Count > 0)
            {
                return versions[^1].Version;
            }
            throw new Exception($"No registered version for {domain} domain found.");
        }

        public void Register(BaboonCodecsFacade other)
        {
            foreach (var (id, versions) in other._domainVersions) _domainVersions[id] = versions;
            foreach (var (id, codec) in other._versionsCodecsJson) _versionsCodecsJson[id] = codec;
            foreach (var (id, codec) in other._versionsCodecsBin) _versionsCodecsBin[id] = codec;
            foreach (var (id, conv) in other._versionsConversions) _versionsConversions[id] = conv;
            foreach (var (id, meta) in other._versionsMeta) _versionsMeta[id] = meta;
        }

        public BaboonDomainVersion Register(
            BaboonDomainVersion domainVersion,
            Func<AbstractBaboonJsonCodecs> codecsJson,
            Func<AbstractBaboonUebaCodecs> codecsBin,
            Func<AbstractBaboonConversions> conversions,
            Func<IBaboonMeta> meta)
        {
            RegisterVersion(domainVersion);
            _versionsCodecsJson[domainVersion] = new Lazy<AbstractBaboonJsonCodecs>(codecsJson);
            _versionsCodecsBin[domainVersion] = new Lazy<AbstractBaboonUebaCodecs>(codecsBin);
            _versionsConversions[domainVersion] = new Lazy<AbstractBaboonConversions>(conversions);
            _versionsMeta[domainVersion] = new Lazy<IBaboonMeta>(meta);
            return domainVersion;
        }

        public BaboonDomainVersion Register(
            BaboonDomainVersion domainVersion,
            Func<AbstractBaboonJsonCodecs> codecsJson,
            Func<AbstractBaboonUebaCodecs> codecsBin)
        {
            RegisterVersion(domainVersion);
            _versionsCodecsJson[domainVersion] = new Lazy<AbstractBaboonJsonCodecs>(codecsJson);
            _versionsCodecsBin[domainVersion] = new Lazy<AbstractBaboonUebaCodecs>(codecsBin);
            return domainVersion;
        }

        public BaboonDomainVersion Register(
            BaboonDomainVersion domainVersion,
            Func<AbstractBaboonJsonCodecs> codecsJson,
            Func<AbstractBaboonUebaCodecs> codecsBin,
            Func<IBaboonMeta> meta)
        {
            RegisterVersion(domainVersion);
            _versionsCodecsJson[domainVersion] = new Lazy<AbstractBaboonJsonCodecs>(codecsJson);
            _versionsCodecsBin[domainVersion] = new Lazy<AbstractBaboonUebaCodecs>(codecsBin);
            _versionsMeta[domainVersion] = new Lazy<IBaboonMeta>(meta);
            return domainVersion;
        }

        public BaboonDomainVersion RegisterConversions(
            BaboonDomainVersion domainVersion,
            Func<AbstractBaboonConversions> conversions)
        {
            RegisterVersion(domainVersion);
            _versionsConversions[domainVersion] = new Lazy<AbstractBaboonConversions>(conversions);
            return domainVersion;
        }

        public BaboonDomainVersion RegisterMeta(
            BaboonDomainVersion domainVersion,
            Func<IBaboonMeta> meta)
        {
            RegisterVersion(domainVersion);
            _versionsMeta[domainVersion] = new Lazy<IBaboonMeta>(meta);
            return domainVersion;
        }

        public void Verify()
        {
            if (_domainVersions.IsEmpty)
            {
                throw new Exception("Baboon codecs must have at least one domain registered.");
            }

            foreach (var dv in _domainVersions.Values.SelectMany(v => v))
            {
                if (!_versionsConversions.ContainsKey(dv))
                {
                    throw new BaboonCodecException.ConversionNotFound(
                        $"Baboon codecs must have conversion for {dv} registered.");
                }
                if (!_versionsMeta.ContainsKey(dv))
                {
                    throw new BaboonCodecException.CodecNotFound(
                        $"Baboon codecs must have codecs for {dv} registered.");
                }
            }
        }

        public Either<BaboonCodecException, byte[]> EncodeToBin<T>(BaboonCodecContext ctx, T value)
            where T : IBaboonGenerated
        {
            return EncodeToBin(ctx, value, null);
        }

        public Either<BaboonCodecException, byte[]> EncodeToBin<T>(
            BaboonCodecContext ctx,
            T value,
            BaboonTypeMeta? typeMetaOverride)
            where T : IBaboonGenerated
        {
            using var ms = new MemoryStream();
            using var writer = new BinaryWriter(ms);
            var result = EncodeToBin(ctx, writer, value, typeMetaOverride);
            if (result is Either<BaboonCodecException, Unit>.Left l)
            {
                return Either.Left<BaboonCodecException, byte[]>(l.Value);
            }
            writer.Flush();
            return Either.Right<BaboonCodecException, byte[]>(ms.ToArray());
        }

        public Either<BaboonCodecException, Unit> EncodeToBin<T>(
            BaboonCodecContext ctx,
            BinaryWriter writer,
            T value)
            where T : IBaboonGenerated
        {
            return EncodeToBin(ctx, writer, value, null);
        }

        public Either<BaboonCodecException, Unit> EncodeToBin<T>(
            BaboonCodecContext ctx,
            BinaryWriter writer,
            T value,
            BaboonTypeMeta? typeMetaOverride)
            where T : IBaboonGenerated
        {
            var typeMeta = BaboonTypeMeta.From(value, typeof(T));
            var codecResult = GetBinCodec(typeMeta, exact: true);
            if (codecResult is Either<BaboonCodecException, IBaboonCodecData>.Left l)
            {
                return Either.Left<BaboonCodecException, Unit>(l.Value);
            }
            var codec = ((Either<BaboonCodecException, IBaboonCodecData>.Right)codecResult).Value;

            try
            {
                (typeMetaOverride ?? typeMeta).WriteBin(writer);
                ((IBaboonStreamCodec<IBaboonGenerated, BinaryWriter, BinaryReader>)codec).Encode(ctx, writer, value);
                return Either.Right<BaboonCodecException, Unit>(Unit.Default);
            }
            catch (Exception e)
            {
                return Either.Left<BaboonCodecException, Unit>(
                    new BaboonCodecException.EncoderFailure(
                        $"Exception while trying to encode to binary form type [{typeMeta.DomainIdentifier}.{typeMeta.TypeIdentifier}] of version '{typeMeta.DomainVersion}'.",
                        e));
            }
        }

        public Either<BaboonCodecException, IBaboonGenerated> DecodeAny(AnyOpaque opaque)
        {
            var meta = opaque.Meta;
            var metaResult = BuildSyntheticTypeMeta(meta);
            if (metaResult is Either<BaboonCodecException, BaboonTypeMeta>.Left l)
            {
                return Either.Left<BaboonCodecException, IBaboonGenerated>(l.Value);
            }
            var typeMeta = ((Either<BaboonCodecException, BaboonTypeMeta>.Right)metaResult).Value;

            switch (opaque)
            {
                case AnyOpaqueUeba u:
                {
                    var codecResult = GetBinCodec(typeMeta, exact: false);
                    if (codecResult is Either<BaboonCodecException, IBaboonCodecData>.Left cl)
                    {
                        return Either.Left<BaboonCodecException, IBaboonGenerated>(cl.Value);
                    }
                    var codec = ((Either<BaboonCodecException, IBaboonCodecData>.Right)codecResult).Value;
                    try
                    {
                        using var ms = new MemoryStream(u.Bytes);
                        using var reader = new BinaryReader(ms);
                        var decoded = ((IBaboonStreamCodec<IBaboonGenerated, BinaryWriter, BinaryReader>)codec)
                            .Decode(BaboonCodecContext.Compact, reader);
                        return Either.Right<BaboonCodecException, IBaboonGenerated>(decoded);
                    }
                    catch (Exception e)
                    {
                        return Either.Left<BaboonCodecException, IBaboonGenerated>(
                            new BaboonCodecException.DecoderFailure(
                                $"DecodeAny: cannot decode UEBA payload of type [{typeMeta.DomainIdentifier}.{typeMeta.TypeIdentifier}] of version '{typeMeta.DomainVersion}'.",
                                e));
                    }
                }
                case AnyOpaqueJson j:
                {
                    var codecResult = GetJsonCodec(typeMeta, exact: false);
                    if (codecResult is Either<BaboonCodecException, IBaboonCodecData>.Left cl)
                    {
                        return Either.Left<BaboonCodecException, IBaboonGenerated>(cl.Value);
                    }
                    var codec = ((Either<BaboonCodecException, IBaboonCodecData>.Right)codecResult).Value;
                    try
                    {
                        var decoded = ((IBaboonValueCodec<IBaboonGenerated, JToken>)codec)
                            .Decode(BaboonCodecContext.Compact, j.Json);
                        return Either.Right<BaboonCodecException, IBaboonGenerated>(decoded);
                    }
                    catch (Exception e)
                    {
                        return Either.Left<BaboonCodecException, IBaboonGenerated>(
                            new BaboonCodecException.DecoderFailure(
                                $"DecodeAny: cannot decode JSON payload of type [{typeMeta.DomainIdentifier}.{typeMeta.TypeIdentifier}] of version '{typeMeta.DomainVersion}'.",
                                e));
                    }
                }
                default:
                    return Either.Left<BaboonCodecException, IBaboonGenerated>(
                        new BaboonCodecException.DecoderFailure(
                            $"DecodeAny: unknown AnyOpaque branch {opaque.GetType().Name}"));
            }
        }

        // Cross-format helper: decode an `AnyOpaqueJson` payload via the registered JSON codec, then
        // re-encode it via the registered UEBA codec. The wire `meta` may omit components that the
        // field's static declaration already pins down (variants B/C/D1/D2/D3). The codec generator
        // passes the static fallbacks; runtime `meta.X` takes precedence over `staticX` to preserve
        // override semantics. PR-06-D01 fix: without static fallback only variant A would work.
        //
        // Default `null` values are deliberately permitted on this user-facing helper; the codec
        // generator always supplies all three. User code calling this directly must provide whatever
        // static context it has, or accept the all-null semantics (= no static fallback).
        public Either<BaboonCodecException, byte[]> JsonToUebaBytes(
            AnyMeta meta,
            JToken json,
            string? staticDomain = null,
            string? staticVersion = null,
            string? staticTypeid = null)
        {
            var metaResult = BuildSyntheticTypeMeta(meta, staticDomain, staticVersion, staticTypeid);
            if (metaResult is Either<BaboonCodecException, BaboonTypeMeta>.Left ml)
            {
                return Either.Left<BaboonCodecException, byte[]>(ml.Value);
            }
            var typeMeta = ((Either<BaboonCodecException, BaboonTypeMeta>.Right)metaResult).Value;

            var jsonCodecResult = GetJsonCodec(typeMeta, exact: false);
            if (jsonCodecResult is Either<BaboonCodecException, IBaboonCodecData>.Left jl)
            {
                return Either.Left<BaboonCodecException, byte[]>(jl.Value);
            }
            var jsonCodec = ((Either<BaboonCodecException, IBaboonCodecData>.Right)jsonCodecResult).Value;

            var binCodecResult = GetBinCodec(typeMeta, exact: false);
            if (binCodecResult is Either<BaboonCodecException, IBaboonCodecData>.Left bl)
            {
                return Either.Left<BaboonCodecException, byte[]>(bl.Value);
            }
            var binCodec = ((Either<BaboonCodecException, IBaboonCodecData>.Right)binCodecResult).Value;

            IBaboonGenerated typed;
            try
            {
                typed = ((IBaboonValueCodec<IBaboonGenerated, JToken>)jsonCodec)
                    .Decode(BaboonCodecContext.Compact, json);
            }
            catch (Exception e)
            {
                return Either.Left<BaboonCodecException, byte[]>(
                    new BaboonCodecException.DecoderFailure(
                        $"JsonToUebaBytes: cannot decode JSON payload of type [{typeMeta.DomainIdentifier}.{typeMeta.TypeIdentifier}] of version '{typeMeta.DomainVersion}'.",
                        e));
            }

            try
            {
                using var ms = new MemoryStream();
                using var writer = new BinaryWriter(ms);
                ((IBaboonStreamCodec<IBaboonGenerated, BinaryWriter, BinaryReader>)binCodec)
                    .Encode(BaboonCodecContext.Compact, writer, typed);
                writer.Flush();
                return Either.Right<BaboonCodecException, byte[]>(ms.ToArray());
            }
            catch (Exception e)
            {
                return Either.Left<BaboonCodecException, byte[]>(
                    new BaboonCodecException.EncoderFailure(
                        $"JsonToUebaBytes: cannot encode UEBA payload of type [{typeMeta.DomainIdentifier}.{typeMeta.TypeIdentifier}] of version '{typeMeta.DomainVersion}'.",
                        e));
            }
        }

        // Cross-format helper symmetric to JsonToUebaBytes. See its documentation for the
        // static-fallback contract.
        public Either<BaboonCodecException, JToken> UebaToJson(
            AnyMeta meta,
            byte[] bytes,
            string? staticDomain = null,
            string? staticVersion = null,
            string? staticTypeid = null)
        {
            var metaResult = BuildSyntheticTypeMeta(meta, staticDomain, staticVersion, staticTypeid);
            if (metaResult is Either<BaboonCodecException, BaboonTypeMeta>.Left ml)
            {
                return Either.Left<BaboonCodecException, JToken>(ml.Value);
            }
            var typeMeta = ((Either<BaboonCodecException, BaboonTypeMeta>.Right)metaResult).Value;

            var binCodecResult = GetBinCodec(typeMeta, exact: false);
            if (binCodecResult is Either<BaboonCodecException, IBaboonCodecData>.Left bl)
            {
                return Either.Left<BaboonCodecException, JToken>(bl.Value);
            }
            var binCodec = ((Either<BaboonCodecException, IBaboonCodecData>.Right)binCodecResult).Value;

            var jsonCodecResult = GetJsonCodec(typeMeta, exact: false);
            if (jsonCodecResult is Either<BaboonCodecException, IBaboonCodecData>.Left jl)
            {
                return Either.Left<BaboonCodecException, JToken>(jl.Value);
            }
            var jsonCodec = ((Either<BaboonCodecException, IBaboonCodecData>.Right)jsonCodecResult).Value;

            IBaboonGenerated typed;
            try
            {
                using var ms = new MemoryStream(bytes);
                using var reader = new BinaryReader(ms);
                typed = ((IBaboonStreamCodec<IBaboonGenerated, BinaryWriter, BinaryReader>)binCodec)
                    .Decode(BaboonCodecContext.Compact, reader);
            }
            catch (Exception e)
            {
                return Either.Left<BaboonCodecException, JToken>(
                    new BaboonCodecException.DecoderFailure(
                        $"UebaToJson: cannot decode UEBA payload of type [{typeMeta.DomainIdentifier}.{typeMeta.TypeIdentifier}] of version '{typeMeta.DomainVersion}'.",
                        e));
            }

            try
            {
                var json = ((IBaboonValueCodec<IBaboonGenerated, JToken>)jsonCodec)
                    .Encode(BaboonCodecContext.Compact, typed);
                return Either.Right<BaboonCodecException, JToken>(json);
            }
            catch (Exception e)
            {
                return Either.Left<BaboonCodecException, JToken>(
                    new BaboonCodecException.EncoderFailure(
                        $"UebaToJson: cannot encode JSON payload of type [{typeMeta.DomainIdentifier}.{typeMeta.TypeIdentifier}] of version '{typeMeta.DomainVersion}'.",
                        e));
            }
        }

        // Synthesise a `BaboonTypeMeta` from an `AnyMeta` plus optional static fallbacks.
        // `AnyMeta` does not carry a min-compat version; forward-version migration is unavailable for
        // any-payloads, so `domainVersionMinCompat = version`. `meta.X` takes precedence over `staticX`
        // when both are present (override semantics — wire data wins). `DecodeAny` calls this with all
        // null statics so its limitation to variant A is preserved.
        private static Either<BaboonCodecException, BaboonTypeMeta> BuildSyntheticTypeMeta(
            AnyMeta meta,
            string? staticDomain = null,
            string? staticVersion = null,
            string? staticTypeid = null)
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
            return Either.Left<BaboonCodecException, BaboonTypeMeta>(
                new BaboonCodecException.DecoderFailure(
                    $"AnyMeta requires domain/version/typeid for facade resolution; got kind 0x{(meta.Kind & 0xFF):x} which lacks: {string.Join(", ", missing)}"));
        }

        public Either<BaboonCodecException, IBaboonGenerated> DecodeFromBin(BinaryReader reader)
        {
            var typeMeta = BaboonTypeMeta.ReadMeta(reader);
            if (typeMeta is null)
            {
                return Either.Left<BaboonCodecException, IBaboonGenerated>(
                    new BaboonCodecException.DecoderFailure("Cannot decode binary type meta"));
            }

            var codecResult = GetBinCodec(typeMeta, exact: false);
            if (codecResult is Either<BaboonCodecException, IBaboonCodecData>.Left l)
            {
                return Either.Left<BaboonCodecException, IBaboonGenerated>(l.Value);
            }
            var codec = ((Either<BaboonCodecException, IBaboonCodecData>.Right)codecResult).Value;

            try
            {
                var decoded = ((IBaboonStreamCodec<IBaboonGenerated, BinaryWriter, BinaryReader>)codec)
                    .Decode(BaboonCodecContext.Compact, reader);
                return Either.Right<BaboonCodecException, IBaboonGenerated>(decoded);
            }
            catch (Exception e)
            {
                return Either.Left<BaboonCodecException, IBaboonGenerated>(
                    new BaboonCodecException.DecoderFailure(
                        $"Can not decode BIN form type [{typeMeta.DomainIdentifier}.{typeMeta.TypeIdentifier}] of version '{typeMeta.DomainVersion}'.",
                        e));
            }
        }

        public Either<BaboonCodecException, IBaboonGenerated> DecodeFromBin(byte[] bytes)
        {
            using var ms = new MemoryStream(bytes);
            using var reader = new BinaryReader(ms);
            return DecodeFromBin(reader);
        }

        public Either<BaboonCodecException, JToken> EncodeToJson<T>(T value)
            where T : IBaboonGenerated
        {
            return EncodeToJson(value, null);
        }

        public Either<BaboonCodecException, JToken> EncodeToJson<T>(T value, BaboonTypeMeta? typeMetaOverride)
            where T : IBaboonGenerated
        {
            var typeMeta = BaboonTypeMeta.From(value, typeof(T));
            var codecResult = GetJsonCodec(typeMeta, exact: true);
            if (codecResult is Either<BaboonCodecException, IBaboonCodecData>.Left l)
            {
                return Either.Left<BaboonCodecException, JToken>(l.Value);
            }
            var codec = ((Either<BaboonCodecException, IBaboonCodecData>.Right)codecResult).Value;

            try
            {
                var content = ((IBaboonValueCodec<IBaboonGenerated, JToken>)codec)
                    .Encode(BaboonCodecContext.Compact, value);
                var metaJson = (typeMetaOverride ?? typeMeta).WriteJson();
                if (metaJson is not JObject metaObj)
                {
                    return Either.Left<BaboonCodecException, JToken>(
                        new BaboonCodecException.EncoderFailure(
                            $"BaboonTypeMeta.WriteJson must return a JObject; got {metaJson.Type}"));
                }
                metaObj[CONTENT_JSON_KEY] = content;
                return Either.Right<BaboonCodecException, JToken>(metaObj);
            }
            catch (Exception e)
            {
                return Either.Left<BaboonCodecException, JToken>(
                    new BaboonCodecException.EncoderFailure(
                        $"Can not encode to json form type [{value.BaboonTypeIdentifier()}] of version '{value.BaboonDomainVersion()}'.",
                        e));
            }
        }

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
            if (codecResult is Either<BaboonCodecException, IBaboonCodecData>.Left l)
            {
                return Either.Left<BaboonCodecException, IBaboonGenerated?>(l.Value);
            }
            var codec = ((Either<BaboonCodecException, IBaboonCodecData>.Right)codecResult).Value;

            try
            {
                var decoded = ((IBaboonValueCodec<IBaboonGenerated, JToken>)codec)
                    .Decode(BaboonCodecContext.Compact, contentToken);
                return Either.Right<BaboonCodecException, IBaboonGenerated?>(decoded);
            }
            catch (Exception e)
            {
                return Either.Left<BaboonCodecException, IBaboonGenerated?>(
                    new BaboonCodecException.DecoderFailure(
                        $"Can not decode JSON form type [{typeMeta.DomainIdentifier}.{typeMeta.TypeIdentifier}] of version '{typeMeta.DomainVersion}'. JSON: {value}",
                        e));
            }
        }

        public Either<BaboonCodecException, IBaboonGenerated?> DecodeFromJson(string value)
        {
            JToken parsed;
            try
            {
                parsed = JToken.Parse(value);
            }
            catch (Exception e)
            {
                return Either.Left<BaboonCodecException, IBaboonGenerated?>(
                    new BaboonCodecException.DecoderFailure($"Cannot parse JSON: {e.Message}", e));
            }
            return DecodeFromJson(parsed);
        }

        public Either<BaboonCodecException, TTo> Convert<TFrom, TTo>(TFrom value)
            where TFrom : IBaboonGenerated
            where TTo : IBaboonGeneratedLatest
        {
            // Identity short-circuit: target already matches.
            if (value is TTo direct) return Either.Right<BaboonCodecException, TTo>(direct);

            var dvFrom = new BaboonDomainVersion(value.BaboonDomainIdentifier(), value.BaboonDomainVersion());

            if (!_domainVersions.TryGetValue(dvFrom.DomainIdentifier, out var versions) || versions.Count == 0)
            {
                return Either.Left<BaboonCodecException, TTo>(
                    new BaboonCodecException.ConverterFailure($"Unknown domain '{dvFrom.DomainIdentifier}'."));
            }

            if (versions.All(v => v != dvFrom))
            {
                return Either.Left<BaboonCodecException, TTo>(
                    new BaboonCodecException.ConverterFailure($"Unknown domain version' {dvFrom}'."));
            }

            // Iterate from first to latest, applying each step's conversion when needed.
            IBaboonGenerated current = value;
            foreach (var toVersion in versions)
            {
                if (current.BaboonDomainVersion() == toVersion.DomainVersion ||
                    new BaboonDomainVersion(current.BaboonDomainIdentifier(), current.BaboonDomainVersion()).Version >= toVersion.Version)
                {
                    continue;
                }

                if (!_versionsConversions.TryGetValue(toVersion, out var lazyConv))
                {
                    return Either.Left<BaboonCodecException, TTo>(
                        new BaboonCodecException.ConverterFailure($"Can not find version '{toVersion}' conversions."));
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
                    return Either.Left<BaboonCodecException, TTo>(
                        new BaboonCodecException.ConverterFailure(
                            $"Can not find version '{toVersion}' type [{current.GetType().FullName}] conversions."));
                }

                try
                {
                    current = conversions.Convert(current, bestConversion);
                }
                catch (Exception e)
                {
                    return Either.Left<BaboonCodecException, TTo>(
                        new BaboonCodecException.ConverterFailure(
                            $"Exception while converting type [{current.GetType().FullName}] of version '{current.BaboonDomainVersion()}' to version '{toVersion}'.",
                            e));
                }
            }

            if (current is TTo result) return Either.Right<BaboonCodecException, TTo>(result);
            return Either.Left<BaboonCodecException, TTo>(
                new BaboonCodecException.ConverterFailure(
                    $"Expected to have type [{typeof(TTo).FullName}] at the end, but got [{current.GetType().FullName}]."));
        }

        private static bool IsAdtConversion(IBaboonGenerated instance, IConversion conversion)
        {
            return instance is IBaboonAdtMemberMeta adt && adt.BaboonAdtType() == conversion.TypeFrom();
        }

        private Either<BaboonCodecException, IBaboonCodecData> GetBinCodec(BaboonTypeMeta typeMeta, bool exact)
        {
            return GetCodec(_versionsCodecsBin, typeMeta, exact);
        }

        private Either<BaboonCodecException, IBaboonCodecData> GetJsonCodec(BaboonTypeMeta typeMeta, bool exact)
        {
            return GetCodec(_versionsCodecsJson, typeMeta, exact);
        }

        private Either<BaboonCodecException, IBaboonCodecData> GetCodec<TCodecs>(
            ConcurrentDictionary<BaboonDomainVersion, Lazy<TCodecs>> versionsCodecs,
            BaboonTypeMeta typeMeta,
            bool exact)
            where TCodecs : AbstractBaboonCodecs
        {
            if (!_domainVersions.TryGetValue(typeMeta.DomainIdentifier, out var versions) || versions.Count == 0)
            {
                return Either.Left<BaboonCodecException, IBaboonCodecData>(
                    new BaboonCodecException.CodecNotFound($"Unknown domain {typeMeta.DomainIdentifier}."));
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

            if (exact && modelV.CompareTo(maxV) == 0)
            {
                return GetCodecExact(versionsCodecs, modelVersion, typeMeta.TypeIdentifier);
            }
            // PR-07-D02: non-exact lookup at the latest registered version routes to exact lookup.
            // Without this arm a single-version domain (minVersion == maxVersion == modelVersion)
            // falls through every other arm and yields "Unsupported domain version" because the
            // strictly-less-than bound on the next arm excludes equality.
            if (!exact && modelV.CompareTo(maxV) == 0)
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
            return Either.Left<BaboonCodecException, IBaboonCodecData>(
                new BaboonCodecException.CodecNotFound($"Unsupported domain version '{modelVersion}'."));
        }

        private static Either<BaboonCodecException, IBaboonCodecData> GetCodecExact<TCodecs>(
            ConcurrentDictionary<BaboonDomainVersion, Lazy<TCodecs>> versionsCodecs,
            BaboonDomainVersion domainVersion,
            string typeIdentifier)
            where TCodecs : AbstractBaboonCodecs
        {
            if (!versionsCodecs.TryGetValue(domainVersion, out var lazyCodecs))
            {
                return Either.Left<BaboonCodecException, IBaboonCodecData>(
                    new BaboonCodecException.CodecNotFound($"No codecs registered for domain version '{domainVersion}'."));
            }
            if (!lazyCodecs.Value.TryFind(typeIdentifier, out var lazyCodec) || lazyCodec is null)
            {
                return Either.Left<BaboonCodecException, IBaboonCodecData>(
                    new BaboonCodecException.CodecNotFound(
                        $"No codec found for type [{domainVersion.DomainVersion}.{typeIdentifier}] of version '{domainVersion.Version}'."));
            }
            return Either.Right<BaboonCodecException, IBaboonCodecData>(lazyCodec.Value);
        }

        private Either<BaboonCodecException, IBaboonCodecData> GetCodecMaxCompat<TCodecs>(
            ConcurrentDictionary<BaboonDomainVersion, Lazy<TCodecs>> versionsCodecs,
            BaboonDomainVersion modelVersion,
            BaboonDomainVersion maxVersion,
            string typeIdentifier)
            where TCodecs : AbstractBaboonCodecs
        {
            if (!_versionsMeta.TryGetValue(modelVersion, out var lazyMeta))
            {
                return Either.Left<BaboonCodecException, IBaboonCodecData>(
                    new BaboonCodecException.CodecNotFound($"Unknown domain version '{modelVersion}'."));
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
                return Either.Left<BaboonCodecException, IBaboonCodecData>(
                    new BaboonCodecException.CodecNotFound(
                        $"No max compat codec found for type [{modelVersion.DomainIdentifier}.{typeIdentifier}] of version '{modelVersion.DomainVersion}'."));
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
                    var updated = new List<BaboonDomainVersion>(existing) { domainVersion };
                    updated.Sort((a, b) => a.Version.CompareTo(b.Version));
                    _domainVersions[domainVersion.DomainIdentifier] = updated;
                }
                else
                {
                    _domainVersions[domainVersion.DomainIdentifier] = new List<BaboonDomainVersion> { domainVersion };
                }
            }
        }
    }
}
