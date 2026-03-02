using System;
using System.IO;
using System.Net;
using System.Threading;
using Baboon.Runtime.Shared;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;

namespace ServiceTest;

public static class PetStoreServer
{
    public static void Start(string host, int port)
    {
        PetStoreImpl impl = new PetStoreImpl();
        BaboonCodecContext ctx = BaboonCodecContext.Default;

        HttpListener listener = new HttpListener();
        string prefix = $"http://{host}:{port}/";
        listener.Prefixes.Add(prefix);
        listener.Start();

        ManualResetEventSlim shutdown = new ManualResetEventSlim(false);

        Console.CancelKeyPress += (_, e) =>
        {
            e.Cancel = true;
            shutdown.Set();
        };

        Console.WriteLine($"Listening on {host}:{port}");
        Console.Out.Flush();

        Thread listenerThread = new Thread(() =>
        {
            while (listener.IsListening)
            {
                HttpListenerContext? context;
                try
                {
                    context = listener.GetContext();
                }
                catch (HttpListenerException)
                {
                    break;
                }

                try
                {
                    HandleRequest(context, impl, ctx, shutdown);
                }
                catch (Exception ex)
                {
                    try
                    {
                        context.Response.StatusCode = 500;
                        using StreamWriter sw = new StreamWriter(context.Response.OutputStream);
                        sw.Write(ex.ToString());
                    }
                    catch
                    {
                        // response already sent
                    }
                }
                finally
                {
                    context.Response.Close();
                }
            }
        });
        listenerThread.IsBackground = true;
        listenerThread.Start();

        shutdown.Wait();
        listener.Stop();
    }

    private static string InvokeJson(string method, string data, PetStoreImpl impl, BaboonCodecContext ctx)
    {
        JToken wire = JToken.Parse(data);
        switch (method)
        {
            case "addPet":
            {
                Petstore.Api.PetStore.addPet.In decoded = Petstore.Api.PetStore.addPet.In_JsonCodec.Instance.Decode(ctx, wire);
                Petstore.Api.PetStore.addPet.Out result = impl.addPet(decoded);
                JToken encoded = Petstore.Api.PetStore.addPet.Out_JsonCodec.Instance.Encode(ctx, result);
                return encoded.ToString(Formatting.None);
            }
            case "getPet":
            {
                Petstore.Api.PetStore.getPet.In decoded = Petstore.Api.PetStore.getPet.In_JsonCodec.Instance.Decode(ctx, wire);
                Petstore.Api.PetStore.getPet.Out result = impl.getPet(decoded);
                JToken encoded = Petstore.Api.PetStore.getPet.Out_JsonCodec.Instance.Encode(ctx, result);
                return encoded.ToString(Formatting.None);
            }
            case "listPets":
            {
                Petstore.Api.PetStore.listPets.In decoded = Petstore.Api.PetStore.listPets.In_JsonCodec.Instance.Decode(ctx, wire);
                Petstore.Api.PetStore.listPets.Out result = impl.listPets(decoded);
                JToken encoded = Petstore.Api.PetStore.listPets.Out_JsonCodec.Instance.Encode(ctx, result);
                return encoded.ToString(Formatting.None);
            }
            case "deletePet":
            {
                Petstore.Api.PetStore.deletePet.In decoded = Petstore.Api.PetStore.deletePet.In_JsonCodec.Instance.Decode(ctx, wire);
                Petstore.Api.PetStore.deletePet.Out result = impl.deletePet(decoded);
                JToken encoded = Petstore.Api.PetStore.deletePet.Out_JsonCodec.Instance.Encode(ctx, result);
                return encoded.ToString(Formatting.None);
            }
            default:
                throw new ArgumentException($"Unknown method: {method}");
        }
    }

    private static void HandleRequest(
        HttpListenerContext context,
        PetStoreImpl impl,
        BaboonCodecContext ctx,
        ManualResetEventSlim shutdown
    )
    {
        HttpListenerRequest request = context.Request;
        HttpListenerResponse response = context.Response;

        string path = request.Url!.AbsolutePath;

        if (path == "/health" && request.HttpMethod == "GET")
        {
            response.StatusCode = 200;
            response.ContentType = "text/plain";
            using StreamWriter sw = new StreamWriter(response.OutputStream);
            sw.Write("ok");
            return;
        }

        if (path == "/reset" && request.HttpMethod == "POST")
        {
            impl.Reset();
            response.StatusCode = 200;
            response.ContentType = "text/plain";
            using StreamWriter sw = new StreamWriter(response.OutputStream);
            sw.Write("ok");
            return;
        }

        if (path == "/shutdown" && request.HttpMethod == "POST")
        {
            response.StatusCode = 200;
            response.ContentType = "text/plain";
            using StreamWriter sw = new StreamWriter(response.OutputStream);
            sw.Write("ok");
            shutdown.Set();
            return;
        }

        if (request.HttpMethod == "POST")
        {
            string[] parts = path.Split('/', StringSplitOptions.RemoveEmptyEntries);
            if (parts.Length == 2)
            {
                string method = parts[1];

                string body;
                using (StreamReader sr = new StreamReader(request.InputStream))
                {
                    body = sr.ReadToEnd();
                }

                string result = InvokeJson(method, body, impl, ctx);

                response.StatusCode = 200;
                response.ContentType = "application/json";
                using StreamWriter sw = new StreamWriter(response.OutputStream);
                sw.Write(result);
                return;
            }
        }

        response.StatusCode = 404;
        response.ContentType = "text/plain";
        using (StreamWriter sw = new StreamWriter(response.OutputStream))
        {
            sw.Write("Not Found");
        }
    }
}
