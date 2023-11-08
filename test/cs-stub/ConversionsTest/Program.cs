using System;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using Newtonsoft.Json.Linq;
using Testpkg.Pkg0.v1_0_0;

namespace Main
{
    class Program
    {
        static void Main(string[] args)
        {
            var builder = ImmutableDictionary.CreateBuilder<T13_1, int>();
            builder.Add(T13_1.A1, 99);
            builder.Add(T13_1.A2, 99);
            var result = builder.ToImmutable();
            var g = Guid.NewGuid();
            var i1 = new Testpkg.Pkg0.v1_0_0.T13_2(g, result);
            using (MemoryStream stream = new MemoryStream())
            {
                using (BinaryWriter writer = new BinaryWriter(stream))
                {
                    i1.Codec_UEBA().Encode(writer, i1);
                }
                stream.Flush();
                byte[] bytes = stream.GetBuffer();
                var s2 = new MemoryStream(bytes);
                var r = new BinaryReader(s2);
                var i2 = i1.Codec_UEBA().Decode(r);
                Console.WriteLine(g);
                Console.WriteLine(i2.F[T13_1.A2]);
                
            }
        }
    }
}
