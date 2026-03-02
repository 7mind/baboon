using System;
using System.Diagnostics;
using System.Net.Http;
using System.Text;
using Baboon.Runtime.Shared;
using Newtonsoft.Json.Linq;

namespace ServiceTest;

public static class PetStoreClient
{
    public static void Run(string host, int port)
    {
        BaboonCodecContext ctx = BaboonCodecContext.Default;
        HttpClient http = new HttpClient();
        string baseUrl = $"http://{host}:{port}";

        // Reset
        Post(http, $"{baseUrl}/reset", "");

        // Add Buddy
        Petstore.Api.PetStore.addPet.In addBuddyIn = new Petstore.Api.PetStore.addPet.In(
            "Buddy", Petstore.Api.PetStatus.Available, "dog"
        );
        JToken addBuddyEncoded = Petstore.Api.PetStore.addPet.In_JsonCodec.Instance.Encode(ctx, addBuddyIn);
        string addBuddyResp = Post(http, $"{baseUrl}/PetStore/addPet", addBuddyEncoded.ToString());
        Petstore.Api.PetStore.addPet.Out addBuddyOut = Petstore.Api.PetStore.addPet.Out_JsonCodec.Instance.Decode(ctx, JToken.Parse(addBuddyResp));
        long buddyId = addBuddyOut.Pet.Id;
        Debug.Assert(addBuddyOut.Pet.Name == "Buddy", $"expected name Buddy, got {addBuddyOut.Pet.Name}");

        // Add Whiskers
        Petstore.Api.PetStore.addPet.In addWhiskersIn = new Petstore.Api.PetStore.addPet.In(
            "Whiskers", Petstore.Api.PetStatus.Pending, "cat"
        );
        JToken addWhiskersEncoded = Petstore.Api.PetStore.addPet.In_JsonCodec.Instance.Encode(ctx, addWhiskersIn);
        string addWhiskersResp = Post(http, $"{baseUrl}/PetStore/addPet", addWhiskersEncoded.ToString());
        Petstore.Api.PetStore.addPet.Out addWhiskersOut = Petstore.Api.PetStore.addPet.Out_JsonCodec.Instance.Decode(ctx, JToken.Parse(addWhiskersResp));
        long whiskersId = addWhiskersOut.Pet.Id;
        Debug.Assert(addWhiskersOut.Pet.Name == "Whiskers", $"expected name Whiskers, got {addWhiskersOut.Pet.Name}");

        // List pets (expect 2)
        Petstore.Api.PetStore.listPets.In listIn = new Petstore.Api.PetStore.listPets.In();
        JToken listEncoded = Petstore.Api.PetStore.listPets.In_JsonCodec.Instance.Encode(ctx, listIn);
        string listResp = Post(http, $"{baseUrl}/PetStore/listPets", listEncoded.ToString());
        Petstore.Api.PetStore.listPets.Out listOut = Petstore.Api.PetStore.listPets.Out_JsonCodec.Instance.Decode(ctx, JToken.Parse(listResp));
        Debug.Assert(listOut.Pets.Count == 2, $"expected 2 pets, got {listOut.Pets.Count}");

        // Get Buddy
        Petstore.Api.PetStore.getPet.In getBuddyIn = new Petstore.Api.PetStore.getPet.In(buddyId);
        JToken getBuddyEncoded = Petstore.Api.PetStore.getPet.In_JsonCodec.Instance.Encode(ctx, getBuddyIn);
        string getBuddyResp = Post(http, $"{baseUrl}/PetStore/getPet", getBuddyEncoded.ToString());
        Petstore.Api.PetStore.getPet.Out getBuddyOut = Petstore.Api.PetStore.getPet.Out_JsonCodec.Instance.Decode(ctx, JToken.Parse(getBuddyResp));
        Debug.Assert(getBuddyOut.Pet.Name == "Buddy", $"expected Buddy, got {getBuddyOut.Pet.Name}");
        Debug.Assert(getBuddyOut.Pet.Status == Petstore.Api.PetStatus.Available, $"expected Available, got {getBuddyOut.Pet.Status}");
        Debug.Assert(getBuddyOut.Pet.Tag == "dog", $"expected tag dog, got {getBuddyOut.Pet.Tag}");

        // Delete Whiskers
        Petstore.Api.PetStore.deletePet.In deleteIn = new Petstore.Api.PetStore.deletePet.In(whiskersId);
        JToken deleteEncoded = Petstore.Api.PetStore.deletePet.In_JsonCodec.Instance.Encode(ctx, deleteIn);
        string deleteResp = Post(http, $"{baseUrl}/PetStore/deletePet", deleteEncoded.ToString());
        Petstore.Api.PetStore.deletePet.Out deleteOut = Petstore.Api.PetStore.deletePet.Out_JsonCodec.Instance.Decode(ctx, JToken.Parse(deleteResp));
        Debug.Assert(deleteOut.Deleted == true, $"expected deleted=True, got {deleteOut.Deleted}");

        // List pets again (expect 1)
        string list2Resp = Post(http, $"{baseUrl}/PetStore/listPets", listEncoded.ToString());
        Petstore.Api.PetStore.listPets.Out list2Out = Petstore.Api.PetStore.listPets.Out_JsonCodec.Instance.Decode(ctx, JToken.Parse(list2Resp));
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
