using System;

namespace ServiceTest;

public static class Program
{
    public static void Main(string[] args)
    {
        if (args.Length == 0)
        {
            Console.Error.WriteLine("Usage: ServiceTest <server|client> --host <host> --port <port>");
            Environment.Exit(1);
        }

        string mode = args[0];
        string host = "127.0.0.1";
        int port = 18080;

        int i = 1;
        while (i < args.Length)
        {
            if (args[i] == "--host" && i + 1 < args.Length)
            {
                host = args[i + 1];
                i += 2;
            }
            else if (args[i] == "--port" && i + 1 < args.Length)
            {
                port = int.Parse(args[i + 1]);
                i += 2;
            }
            else
            {
                i++;
            }
        }

        switch (mode)
        {
            case "server":
                PetStoreServer.Start(host, port);
                break;
            case "client":
                PetStoreClient.Run(host, port);
                break;
            default:
                Console.Error.WriteLine($"Unknown mode: {mode}");
                Environment.Exit(1);
                break;
        }
    }
}
