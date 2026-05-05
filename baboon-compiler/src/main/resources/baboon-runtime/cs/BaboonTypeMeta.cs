#nullable enable

using System;
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
    public sealed record BaboonVersion(int Major, int Minor, int Patch) : IComparable<BaboonVersion>
    {
        public int CompareTo(BaboonVersion? other)
        {
            if (other is null) return 1;
            var c = Major.CompareTo(other.Major);
            if (c != 0) return c;
            c = Minor.CompareTo(other.Minor);
            if (c != 0) return c;
            return Patch.CompareTo(other.Patch);
        }

        public static BaboonVersion From(string version)
        {
            var chunks = version.Split('.');
            if (chunks.Length < 3)
            {
                throw new Exception($"Expected to have version in format x.y.z, got {version}");
            }

            int Parse(string s, string slot)
            {
                if (!int.TryParse(s.Trim(), out var v))
                {
                    throw new Exception($"Expected to have version in format x.y.z, got {version}. Invalid {slot} value.");
                }
                return v;
            }

            return new BaboonVersion(Parse(chunks[0], "major"), Parse(chunks[1], "minor"), Parse(chunks[2], "patch"));
        }

        public static bool operator <(BaboonVersion a, BaboonVersion b) => a.CompareTo(b) < 0;
        public static bool operator >(BaboonVersion a, BaboonVersion b) => a.CompareTo(b) > 0;
        public static bool operator <=(BaboonVersion a, BaboonVersion b) => a.CompareTo(b) <= 0;
        public static bool operator >=(BaboonVersion a, BaboonVersion b) => a.CompareTo(b) >= 0;
    }

    public sealed record BaboonDomainVersion(string DomainIdentifier, string DomainVersion)
    {
        // Parses on each access; versions are small strings and the parse is cheap. Avoiding a
        // backing field keeps the positional-record shape simple and value-equality intact.
        public BaboonVersion Version => BaboonVersion.From(DomainVersion);
    }

    public sealed record BaboonTypeMeta(
        byte MetaVersion,
        string DomainIdentifier,
        string DomainVersion,
        string DomainVersionMinCompat,
        string TypeIdentifier
    )
    {
        public BaboonDomainVersion VersionRef => new BaboonDomainVersion(DomainIdentifier, DomainVersion);

        public BaboonDomainVersion? VersionMinCompat
        {
            get
            {
                if (string.IsNullOrEmpty(DomainVersionMinCompat)) return null;
                if (DomainVersionMinCompat == DomainVersion) return null;
                return new BaboonDomainVersion(DomainIdentifier, DomainVersionMinCompat);
            }
        }

        public void WriteBin(BinaryWriter writer) => BaboonTypeMetaCodec.WriteBin(this, writer);

        public JToken WriteJson() => BaboonTypeMetaCodec.WriteJson(this);

        public static BaboonTypeMeta From(IBaboonGenerated value, Type? declaredType)
        {
            // Codec discovery with ADT awareness, mirroring Scala's BaboonTypeMeta.from:
            // when the user-declared static type is the ADT trait/interface, use the ADT's
            // type identifier so the encoder can wrap with the ADT meta envelope. When the
            // user-declared type is the concrete branch, use the branch identifier directly.
            string typeIdentifier;
            if (value is IBaboonAdtMemberMeta adt && declaredType is { IsInterface: true })
            {
                typeIdentifier = adt.BaboonAdtTypeIdentifier();
            }
            else
            {
                typeIdentifier = value.BaboonTypeIdentifier();
            }

            // Codegen invariant (mirrors Scala BaboonRuntimeShared.scala:138): BaboonSameInVersions
            // is always non-empty. Index directly so a violation throws IndexOutOfRangeException
            // rather than silently masquerading as a same-version meta.
            var minCompat = value.BaboonSameInVersions()[0];

            return new BaboonTypeMeta(
                BaboonTypeMetaCodec.META_VERSION,
                value.BaboonDomainIdentifier(),
                value.BaboonDomainVersion(),
                minCompat,
                typeIdentifier
            );
        }

        public static BaboonTypeMeta? ReadMeta(BinaryReader reader) => BaboonTypeMetaCodec.ReadMeta(reader);

        public static BaboonTypeMeta? ReadMeta(JToken json) => BaboonTypeMetaCodec.ReadMeta(json);
    }

    public static class BaboonTypeMetaCodec
    {
        public const byte META_VERSION_1 = 16;
        public const byte META_VERSION = META_VERSION_1;

        public const string META_VERSION_KEY = "$mv";
        public const string DOMAIN_IDENTIFIER_KEY = "$d";
        public const string DOMAIN_VERSION_KEY = "$v";
        public const string DOMAIN_VERSION_MIN_COMPAT_KEY = "$uv";
        public const string TYPE_IDENTIFIER_KEY = "$t";

        public static void WriteBin(BaboonTypeMeta meta, BinaryWriter writer)
        {
            writer.Write(META_VERSION);
            writer.Write(meta.DomainIdentifier);
            writer.Write(meta.DomainVersion);
            if (meta.DomainVersion == meta.DomainVersionMinCompat)
            {
                writer.Write((byte)0);
            }
            else
            {
                writer.Write((byte)1);
                writer.Write(meta.DomainVersionMinCompat);
            }
            writer.Write(meta.TypeIdentifier);
        }

        public static JToken WriteJson(BaboonTypeMeta meta)
        {
            var obj = new JObject
            {
                [DOMAIN_IDENTIFIER_KEY] = meta.DomainIdentifier,
                [DOMAIN_VERSION_KEY] = meta.DomainVersion,
                [TYPE_IDENTIFIER_KEY] = meta.TypeIdentifier,
            };
            if (meta.DomainVersion != meta.DomainVersionMinCompat)
            {
                obj[DOMAIN_VERSION_MIN_COMPAT_KEY] = meta.DomainVersionMinCompat;
            }
            return obj;
        }

        public static BaboonTypeMeta? ReadMeta(BinaryReader reader)
        {
            var metaVersion = reader.ReadByte();
            if (metaVersion != META_VERSION_1) return null;

            var domainIdentifier = reader.ReadString();
            var domainVersion = reader.ReadString();
            var hasMinCompat = reader.ReadByte();
            var domainVersionMinCompat = hasMinCompat == 1 ? reader.ReadString() : domainVersion;
            var typeIdentifier = reader.ReadString();

            return new BaboonTypeMeta(
                META_VERSION,
                domainIdentifier,
                domainVersion,
                domainVersionMinCompat,
                typeIdentifier
            );
        }

        public static BaboonTypeMeta? ReadMeta(JToken json)
        {
            if (json is not JObject obj) return null;

            // Mirror Scala readMeta(json) (BaboonRuntimeShared.scala:197-205): if $mv is present
            // and not "1", reject; if absent, fall through to v1 read.
            var mvToken = obj[META_VERSION_KEY];
            if (mvToken != null)
            {
                if (mvToken.Type != JTokenType.String) return null;
                var mvStr = mvToken.Value<string>();
                if (mvStr is null) return null;
                if (!byte.TryParse(mvStr, out var mv) || mv != META_VERSION_1) return null;
            }

            var d = obj[DOMAIN_IDENTIFIER_KEY]?.Value<string>();
            var v = obj[DOMAIN_VERSION_KEY]?.Value<string>();
            var t = obj[TYPE_IDENTIFIER_KEY]?.Value<string>();

            if (d is null || v is null || t is null) return null;

            var uv = obj[DOMAIN_VERSION_MIN_COMPAT_KEY]?.Value<string>() ?? v;

            return new BaboonTypeMeta(META_VERSION, d, v, uv, t);
        }
    }
}
