#nullable enable

// ReSharper disable CheckNamespace
// ReSharper disable UnusedMember.Global
// ReSharper disable UnusedType.Global

namespace Baboon.Runtime.Shared
{
    public static class BaboonExt
    {
        public static BaboonDomainVersion DomainVersion(this IBaboonGenerated g)
        {
            return new BaboonDomainVersion(g.BaboonDomainIdentifier(), g.BaboonDomainVersion());
        }

        public static string BaboonUnmodifiedSinceVersion(this IBaboonGenerated g)
        {
            return g.BaboonSameInVersions()[0];
        }

        public static string UnmodifiedSinceVersion(this IBaboonMeta g, string tpe)
        {
            return g.SameInVersions(tpe)[0];
        }
    }
}
