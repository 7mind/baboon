using System;
using System.Net.Http;
using System.Text;
using Baboon.Runtime.Shared;

namespace ServiceTest;

public static class PetStoreClient
{
    public static void Run(string host, int port)
    {
        BaboonCodecContext ctx = BaboonCodecContext.Default;
        HttpClient http = new HttpClient();
        string baseUrl = $"http://{host}:{port}";

        Petstore.Api.PetStoreClient jsonClient = new Petstore.Api.PetStoreClient(
            (service, method, data) => PostBytes(http, $"{baseUrl}/{service}/{method}", data),
            (service, method, data) => Post(http, $"{baseUrl}/{service}/{method}", data)
        );

        // JSON pass — drives the generated *Json methods over the string transport.
        RunScenario(http, baseUrl, jsonClient, ctx, useUeba: false);

        // UEBA pass — same scenario via the generated bare methods over the binary transport.
        RunScenario(http, baseUrl, jsonClient, ctx, useUeba: true);

        Console.WriteLine("OK");
    }

    private static void RunScenario(
        HttpClient http,
        string baseUrl,
        Petstore.Api.PetStoreClient client,
        BaboonCodecContext ctx,
        bool useUeba
    )
    {
        string label = useUeba ? "ueba" : "json";

        // Reset
        Post(http, $"{baseUrl}/reset", "");

        // Add Buddy
        Petstore.Api.PetStore.AddPet.In addBuddyIn = new Petstore.Api.PetStore.AddPet.In(
            "Buddy", Petstore.Api.PetStatus.Available, "dog"
        );
        Petstore.Api.PetStore.AddPet.Out addBuddyOut =
            useUeba ? client.addPet(addBuddyIn, ctx) : client.addPetJson(addBuddyIn, ctx);
        long buddyId = addBuddyOut.Pet.Id;
        Check(addBuddyOut.Pet.Name == "Buddy", $"[{label}] expected name Buddy, got {addBuddyOut.Pet.Name}");

        // Add Whiskers
        Petstore.Api.PetStore.AddPet.In addWhiskersIn = new Petstore.Api.PetStore.AddPet.In(
            "Whiskers", Petstore.Api.PetStatus.Pending, "cat"
        );
        Petstore.Api.PetStore.AddPet.Out addWhiskersOut =
            useUeba ? client.addPet(addWhiskersIn, ctx) : client.addPetJson(addWhiskersIn, ctx);
        long whiskersId = addWhiskersOut.Pet.Id;
        Check(addWhiskersOut.Pet.Name == "Whiskers", $"[{label}] expected name Whiskers, got {addWhiskersOut.Pet.Name}");

        // List pets (expect 2)
        Petstore.Api.PetStore.ListPets.In listIn = new Petstore.Api.PetStore.ListPets.In();
        Petstore.Api.PetStore.ListPets.Out listOut =
            useUeba ? client.listPets(listIn, ctx) : client.listPetsJson(listIn, ctx);
        Check(listOut.Pets.Count == 2, $"[{label}] expected 2 pets, got {listOut.Pets.Count}");

        // Get Buddy
        Petstore.Api.PetStore.GetPet.In getBuddyIn = new Petstore.Api.PetStore.GetPet.In(buddyId);
        Petstore.Api.PetStore.GetPet.Out getBuddyOut =
            useUeba ? client.getPet(getBuddyIn, ctx) : client.getPetJson(getBuddyIn, ctx);
        Check(getBuddyOut.Pet.Name == "Buddy", $"[{label}] expected Buddy, got {getBuddyOut.Pet.Name}");
        Check(getBuddyOut.Pet.Status == Petstore.Api.PetStatus.Available, $"[{label}] expected Available, got {getBuddyOut.Pet.Status}");
        Check(getBuddyOut.Pet.Tag == "dog", $"[{label}] expected tag dog, got {getBuddyOut.Pet.Tag}");

        // Delete Whiskers
        Petstore.Api.PetStore.DeletePet.In deleteIn = new Petstore.Api.PetStore.DeletePet.In(whiskersId);
        Petstore.Api.PetStore.DeletePet.Out deleteOut =
            useUeba ? client.deletePet(deleteIn, ctx) : client.deletePetJson(deleteIn, ctx);
        Check(deleteOut.Deleted == true, $"[{label}] expected deleted=True, got {deleteOut.Deleted}");

        // List pets again (expect 1)
        Petstore.Api.PetStore.ListPets.Out list2Out =
            useUeba ? client.listPets(listIn, ctx) : client.listPetsJson(listIn, ctx);
        Check(list2Out.Pets.Count == 1, $"[{label}] expected 1 pet, got {list2Out.Pets.Count}");
        Check(list2Out.Pets[0].Name == "Buddy", $"[{label}] expected remaining pet Buddy, got {list2Out.Pets[0].Name}");
    }

    private static string Post(HttpClient http, string url, string body)
    {
        StringContent content = new StringContent(body, Encoding.UTF8, "application/json");
        HttpResponseMessage response = http.PostAsync(url, content).Result;
        response.EnsureSuccessStatusCode();
        return response.Content.ReadAsStringAsync().Result;
    }

    private static byte[] PostBytes(HttpClient http, string url, byte[] body)
    {
        ByteArrayContent content = new ByteArrayContent(body);
        content.Headers.ContentType = new System.Net.Http.Headers.MediaTypeHeaderValue("application/octet-stream");
        HttpResponseMessage response = http.PostAsync(url, content).Result;
        response.EnsureSuccessStatusCode();
        return response.Content.ReadAsByteArrayAsync().Result;
    }

    private static void Check(bool cond, string msg)
    {
        if (!cond) throw new System.Exception(msg);
    }
}
