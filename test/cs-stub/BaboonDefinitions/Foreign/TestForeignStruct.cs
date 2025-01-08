// ReSharper disable RedundantTypeDeclarationBody

using System;

namespace BaboonDefinitions.Foreign
{
    public struct TestForeignStruct : IEquatable<TestForeignStruct>
    {
        public bool Equals(TestForeignStruct other)
        {
            return true;
        }

        public override bool Equals(object? obj)
        {
            return obj is TestForeignStruct other && Equals(other);
        }

        public override int GetHashCode()
        {
            return 0;
        }
    }
}