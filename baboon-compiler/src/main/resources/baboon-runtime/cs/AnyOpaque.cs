#nullable enable

using System;
using System.Collections.Generic;
using System.IO;
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
    public sealed record AnyMeta
    {
        public byte Kind { get; }
        public string? Domain { get; }
        public string? Version { get; }
        public string? Typeid { get; }

        public AnyMeta(byte kind, string? domain, string? version, string? typeid)
        {
            // Mirror Scala's four `require`s: Option presence × bit alignment × 3, plus VALID_KINDS
            // membership rejecting reserved bytes 0x04/0x05. Throws ArgumentException to match Scala's
            // IllegalArgumentException — both surface as "bad argument from caller".
            var domainBitSet = (kind & AnyMetaCodec.DomainBit) != 0;
            if (domainBitSet != (domain is not null))
            {
                throw new ArgumentException(
                    $"AnyMeta: domain presence ({domain is not null}) does not match kind 0x{(kind & 0xFF):x} bit 2");
            }
            var versionBitSet = (kind & AnyMetaCodec.VersionBit) != 0;
            if (versionBitSet != (version is not null))
            {
                throw new ArgumentException(
                    $"AnyMeta: version presence ({version is not null}) does not match kind 0x{(kind & 0xFF):x} bit 1");
            }
            var typeidBitSet = (kind & AnyMetaCodec.TypeidBit) != 0;
            if (typeidBitSet != (typeid is not null))
            {
                throw new ArgumentException(
                    $"AnyMeta: typeid presence ({typeid is not null}) does not match kind 0x{(kind & 0xFF):x} bit 0");
            }
            if (!AnyMetaCodec.ValidKinds.Contains(kind))
            {
                throw new ArgumentException(
                    $"AnyMeta: reserved or invalid meta-kind byte: 0x{(kind & 0xFF):x2}");
            }

            Kind = kind;
            Domain = domain;
            Version = version;
            Typeid = typeid;
        }
    }

    // Sealed-hierarchy via abstract class + sealed subclasses, NOT records. Records' compiler-
    // generated Equals walks fields with EqualityComparer<T>.Default, which on byte[] reduces to
    // reference identity — that defeats round-trip equality on AnyOpaqueUeba. See PR-05-D08 (Scala
    // analog had the same problem). Manual Equals/GetHashCode lets us compare bytes by content.
    public abstract class AnyOpaque
    {
        public abstract AnyMeta Meta { get; }
    }

    public sealed class AnyOpaqueUeba : AnyOpaque
    {
        public override AnyMeta Meta { get; }
        public byte[] Bytes { get; }

        public AnyOpaqueUeba(AnyMeta meta, byte[] bytes)
        {
            Meta = meta;
            Bytes = bytes;
        }

        public override bool Equals(object? other)
        {
            if (ReferenceEquals(this, other)) return true;
            if (other is not AnyOpaqueUeba that) return false;
            return Meta.Equals(that.Meta) && ByteArrayEquals(Bytes, that.Bytes);
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(Meta, ByteArrayHashCode(Bytes));
        }

        private static bool ByteArrayEquals(byte[] a, byte[] b)
        {
            if (ReferenceEquals(a, b)) return true;
            if (a.Length != b.Length) return false;
            for (var i = 0; i < a.Length; i++)
            {
                if (a[i] != b[i]) return false;
            }
            return true;
        }

        private static int ByteArrayHashCode(byte[] a)
        {
            // Stable content hash for the bytes; order matters.
            unchecked
            {
                var h = 17;
                foreach (var b in a) h = h * 31 + b;
                return h;
            }
        }
    }

    public sealed class AnyOpaqueJson : AnyOpaque
    {
        public override AnyMeta Meta { get; }
        public JToken Json { get; }

        public AnyOpaqueJson(AnyMeta meta, JToken json)
        {
            Meta = meta;
            Json = json;
        }

        public override bool Equals(object? other)
        {
            if (ReferenceEquals(this, other)) return true;
            if (other is not AnyOpaqueJson that) return false;
            return Meta.Equals(that.Meta) && JToken.DeepEquals(Json, that.Json);
        }

        public override int GetHashCode()
        {
            // JToken doesn't expose a structural hash; combine Meta with Json.ToString() as a stable
            // (if not the cheapest) content hash. Equality remains structural via DeepEquals.
            return HashCode.Combine(Meta, Json.ToString(Newtonsoft.Json.Formatting.None));
        }
    }

    public static class AnyMetaCodec
    {
        public const byte DomainBit = 0x04;
        public const byte VersionBit = 0x02;
        public const byte TypeidBit = 0x01;

        public const string AnyKindKey = "$ak";
        public const string AnyDomainKey = "$ad";
        public const string AnyVersionKey = "$av";
        public const string AnyTypeidKey = "$at";

        public static readonly HashSet<byte> ValidKinds = new HashSet<byte>
        {
            0x00, 0x01, 0x02, 0x03, 0x06, 0x07,
        };

        public static void WriteBin(AnyMeta meta, BinaryWriter writer)
        {
            writer.Write((byte)(meta.Kind & 0xFF));
            if (meta.Domain is not null) BaboonBinTools.WriteString(writer, meta.Domain);
            if (meta.Version is not null) BaboonBinTools.WriteString(writer, meta.Version);
            if (meta.Typeid is not null) BaboonBinTools.WriteString(writer, meta.Typeid);
        }

        public static AnyMeta ReadBin(BinaryReader reader)
        {
            var kind = reader.ReadByte();
            string? domain = (kind & DomainBit) != 0 ? BaboonBinTools.ReadString(reader) : null;
            string? version = (kind & VersionBit) != 0 ? BaboonBinTools.ReadString(reader) : null;
            string? typeid = (kind & TypeidBit) != 0 ? BaboonBinTools.ReadString(reader) : null;
            return new AnyMeta(kind, domain, version, typeid);
        }

        // Reads meta and returns (meta, bytesRead). Callers that know the on-wire `meta-length`
        // window can skip any extra bytes left in the window — that's how forward-compat with future
        // meta extensions works. Mirrors Scala's `readBinWithLength` (PR-05-D01).
        public static (AnyMeta meta, int bytesRead) ReadBinWithLength(BinaryReader reader)
        {
            var startPos = reader.BaseStream.Position;
            var meta = ReadBin(reader);
            var bytesRead = (int)(reader.BaseStream.Position - startPos);
            return (meta, bytesRead);
        }

        public static JToken WriteJson(AnyMeta meta)
        {
            // Always returns a JObject — invariant relied on by the JSON encoder envelope build,
            // which adds a "$c" content key via JObject API.
            var obj = new JObject
            {
                [AnyKindKey] = meta.Kind & 0xFF,
            };
            if (meta.Domain is not null) obj[AnyDomainKey] = meta.Domain;
            if (meta.Version is not null) obj[AnyVersionKey] = meta.Version;
            if (meta.Typeid is not null) obj[AnyTypeidKey] = meta.Typeid;
            return obj;
        }

        // Returns Either to mirror Scala's user-facing JSON parse contract: binary decode trusts the
        // wire and throws; JSON decode is user-facing and threads errors as Either.
        public static Either<BaboonCodecException, AnyMeta> ReadJson(JToken json)
        {
            if (json is not JObject obj)
            {
                return Either.Left<BaboonCodecException, AnyMeta>(
                    new BaboonCodecException.DecoderFailure(
                        $"AnyMetaCodec.ReadJson: expected object, got {json.Type}"));
            }

            var kindToken = obj[AnyKindKey];
            if (kindToken is null || kindToken.Type != JTokenType.Integer)
            {
                return Either.Left<BaboonCodecException, AnyMeta>(
                    new BaboonCodecException.DecoderFailure(
                        $"AnyMetaCodec.ReadJson: missing or non-numeric '{AnyKindKey}' field"));
            }

            byte kind;
            try
            {
                kind = (byte)kindToken.Value<int>();
            }
            catch (Exception e)
            {
                return Either.Left<BaboonCodecException, AnyMeta>(
                    new BaboonCodecException.DecoderFailure(
                        $"AnyMetaCodec.ReadJson: invalid '{AnyKindKey}' integer value: {e.Message}"));
            }

            var domainResult = ReadOptString(obj, AnyDomainKey, kind, DomainBit, "domain");
            if (domainResult is Either<BaboonCodecException, string?>.Left dl)
            {
                return Either.Left<BaboonCodecException, AnyMeta>(dl.Value);
            }
            var domainVal = ((Either<BaboonCodecException, string?>.Right)domainResult).Value;

            var versionResult = ReadOptString(obj, AnyVersionKey, kind, VersionBit, "version");
            if (versionResult is Either<BaboonCodecException, string?>.Left vl)
            {
                return Either.Left<BaboonCodecException, AnyMeta>(vl.Value);
            }
            var versionVal = ((Either<BaboonCodecException, string?>.Right)versionResult).Value;

            var typeidResult = ReadOptString(obj, AnyTypeidKey, kind, TypeidBit, "typeid");
            if (typeidResult is Either<BaboonCodecException, string?>.Left tl)
            {
                return Either.Left<BaboonCodecException, AnyMeta>(tl.Value);
            }
            var typeidVal = ((Either<BaboonCodecException, string?>.Right)typeidResult).Value;

            try
            {
                return Either.Right<BaboonCodecException, AnyMeta>(new AnyMeta(kind, domainVal, versionVal, typeidVal));
            }
            catch (ArgumentException e)
            {
                return Either.Left<BaboonCodecException, AnyMeta>(
                    new BaboonCodecException.DecoderFailure(
                        $"AnyMetaCodec.ReadJson: invalid meta: {e.Message}"));
            }
        }

        private static Either<BaboonCodecException, string?> ReadOptString(
            JObject obj,
            string key,
            byte kind,
            byte bit,
            string name)
        {
            var present = (kind & bit) != 0;
            var token = obj[key];
            var value = token is { Type: JTokenType.String } ? token.Value<string>() : null;

            if (present && value is not null) return Either.Right<BaboonCodecException, string?>(value);
            if (!present && value is null) return Either.Right<BaboonCodecException, string?>(null);
            if (present)
            {
                return Either.Left<BaboonCodecException, string?>(
                    new BaboonCodecException.DecoderFailure(
                        $"AnyMetaCodec.ReadJson: kind 0x{(kind & 0xFF):x} requires '{key}' ({name}) but it is missing"));
            }
            return Either.Left<BaboonCodecException, string?>(
                new BaboonCodecException.DecoderFailure(
                    $"AnyMetaCodec.ReadJson: kind 0x{(kind & 0xFF):x} forbids '{key}' ({name}) but it is present"));
        }
    }
}
