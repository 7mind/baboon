using System;
using System.Diagnostics;
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

        Petstore.Api.PetStoreClient client = new Petstore.Api.PetStoreClient(
            (service, method, data) => Post(http, $"{baseUrl}/{service}/{method}", data)
        );

        // Reset
        Post(http, $"{baseUrl}/reset", "");

        // Add Buddy
        Petstore.Api.PetStore.AddPet.In addBuddyIn = new Petstore.Api.PetStore.AddPet.In(
            "Buddy", Petstore.Api.PetStatus.Available, "dog"
        );
        Petstore.Api.PetStore.AddPet.Out addBuddyOut = client.addPetJson(addBuddyIn, ctx);
        long buddyId = addBuddyOut.Pet.Id;
        Debug.Assert(addBuddyOut.Pet.Name == "Buddy", $"expected name Buddy, got {addBuddyOut.Pet.Name}");

        // Add Whiskers
        Petstore.Api.PetStore.AddPet.In addWhiskersIn = new Petstore.Api.PetStore.AddPet.In(
            "Whiskers", Petstore.Api.PetStatus.Pending, "cat"
        );
        Petstore.Api.PetStore.AddPet.Out addWhiskersOut = client.addPetJson(addWhiskersIn, ctx);
        long whiskersId = addWhiskersOut.Pet.Id;
        Debug.Assert(addWhiskersOut.Pet.Name == "Whiskers", $"expected name Whiskers, got {addWhiskersOut.Pet.Name}");

        // List pets (expect 2)
        Petstore.Api.PetStore.ListPets.In listIn = new Petstore.Api.PetStore.ListPets.In();
        Petstore.Api.PetStore.ListPets.Out listOut = client.listPetsJson(listIn, ctx);
        Debug.Assert(listOut.Pets.Count == 2, $"expected 2 pets, got {listOut.Pets.Count}");

        // Get Buddy
        Petstore.Api.PetStore.GetPet.In getBuddyIn = new Petstore.Api.PetStore.GetPet.In(buddyId);
        Petstore.Api.PetStore.GetPet.Out getBuddyOut = client.getPetJson(getBuddyIn, ctx);
        Debug.Assert(getBuddyOut.Pet.Name == "Buddy", $"expected Buddy, got {getBuddyOut.Pet.Name}");
        Debug.Assert(getBuddyOut.Pet.Status == Petstore.Api.PetStatus.Available, $"expected Available, got {getBuddyOut.Pet.Status}");
        Debug.Assert(getBuddyOut.Pet.Tag == "dog", $"expected tag dog, got {getBuddyOut.Pet.Tag}");

        // Delete Whiskers
        Petstore.Api.PetStore.DeletePet.In deleteIn = new Petstore.Api.PetStore.DeletePet.In(whiskersId);
        Petstore.Api.PetStore.DeletePet.Out deleteOut = client.deletePetJson(deleteIn, ctx);
        Debug.Assert(deleteOut.Deleted == true, $"expected deleted=True, got {deleteOut.Deleted}");

        // List pets again (expect 1)
        Petstore.Api.PetStore.ListPets.Out list2Out = client.listPetsJson(listIn, ctx);
        Debug.Assert(list2Out.Pets.Count == 1, $"expected 1 pet, got {list2Out.Pets.Count}");
        Debug.Assert(list2Out.Pets[0].Name == "Buddy", $"expected remaining pet Buddy, got {list2Out.Pets[0].Name}");

        Console.WriteLine("OK");
    }

    private static string Post(HttpClient http, string url, string body)
    {
        StringContent content = new StringContent(body, Encoding.UTF8, "application/json");
        HttpResponseMessage response = http.PostAsync(url, content).Result;
        response.EnsureSuccessStatusCode();
        return response.Content.ReadAsStringAsync().Result;
    }
}
