using System;
using System.IO;
using Newtonsoft.Json.Linq;

namespace Main
{
    interface IBaboonCodec<Instance>
    {

    }

    interface IBaboonValueCodec<Instance, Wire> : IBaboonCodec<Instance>
    {
        Wire Encode(Instance instance);
        Instance Decode(Wire wire);
    }
    
    interface IBaboonJsonCodec<Instance> : IBaboonValueCodec<Instance, JToken>
    {
        
    }
    
    interface IBaboonStreamCodec<Instance, OutStream, InStream> : IBaboonCodec<Instance>
    {
        void Encode(OutStream writer, Instance instance);
        Instance Decode(InStream wire);
    }
    

    interface IBaboonBinCodec<Instance> : IBaboonStreamCodec<Instance, BinaryWriter, BinaryReader>
    {

    }
    
    class Program
    {
        static void Main(string[] args)
        {
            var v1 = JValue.CreateString("a");
            var v2 = new JArray(v1);
            var v3 = new JObject(new JProperty("test", v2));
            JToken v4 = v3;
            Console.WriteLine(v4);
        }
    }
}
