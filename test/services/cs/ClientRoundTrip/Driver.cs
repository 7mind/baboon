using System;
using System.Collections.Generic;
using System.Linq;
using Baboon.Runtime.Shared;
using Petstore.Api;

namespace ClientRoundTrip;

// In-process server implementation of the generated service interface.
public sealed class Impl : IPetStore
{
    private readonly Dictionary<long, Pet> _pets = new();
    private long _next = 1;

    public PetStore.AddPet.Out AddPet(PetStore.AddPet.In a)
    {
        long id = _next++;
        Pet pet = new Pet(id, a.Name, a.Status, a.Tag);
        _pets[id] = pet;
        return new PetStore.AddPet.Out(pet);
    }

    public PetStore.GetPet.Out GetPet(PetStore.GetPet.In a) => new PetStore.GetPet.Out(_pets[a.Id]);

    public PetStore.ListPets.Out ListPets(PetStore.ListPets.In a) =>
        new PetStore.ListPets.Out(_pets.Values.OrderBy(p => p.Id).ToList());

    public PetStore.DeletePet.Out DeletePet(PetStore.DeletePet.In a) =>
        new PetStore.DeletePet.Out(_pets.Remove(a.Id));
}

// Exercises the generated PetStoreClient end-to-end with an in-process
// transport that forwards each (service, method, data) call straight into the
// generated PetStoreWiring server invoker. This validates the full
// client-encode -> transport -> server-invoke -> client-decode loop for both
// the UEBA (bare-name) and JSON (Json-suffixed) generated client methods.
public static class Driver
{
    public static int Main()
    {
        BaboonCodecContext ctx = BaboonCodecContext.Default;

        RunUeba(ctx);
        RunJson(ctx);

        Console.WriteLine("OK");
        return 0;
    }

    private static PetStoreClient NewClient(Impl impl, BaboonCodecContext ctx)
    {
        Func<string, string, byte[], byte[]> transportUeba =
            (svc, method, data) => PetStoreWiring.InvokeUeba(new BaboonMethodId(svc, method), data, impl, ctx);
        Func<string, string, string, string> transportJson =
            (svc, method, data) => PetStoreWiring.InvokeJson(new BaboonMethodId(svc, method), data, impl, ctx);
        return new PetStoreClient(transportUeba, transportJson);
    }

    private static void RunUeba(BaboonCodecContext ctx)
    {
        Impl impl = new Impl();
        PetStoreClient client = NewClient(impl, ctx);

        PetStore.AddPet.Out addOut = client.AddPet(new PetStore.AddPet.In("Buddy", PetStatus.Available, "dog"), ctx);
        Assert(addOut.Pet.Name == "Buddy", $"UEBA AddPet name: {addOut.Pet.Name}");
        long id = addOut.Pet.Id;

        PetStore.GetPet.Out getOut = client.GetPet(new PetStore.GetPet.In(id), ctx);
        Assert(getOut.Pet.Status == PetStatus.Available, "UEBA GetPet status");
        Assert(getOut.Pet.Tag == "dog", $"UEBA GetPet tag: {getOut.Pet.Tag}");

        PetStore.ListPets.Out listOut = client.ListPets(new PetStore.ListPets.In(), ctx);
        Assert(listOut.Pets.Count == 1, $"UEBA ListPets count: {listOut.Pets.Count}");

        PetStore.DeletePet.Out delOut = client.DeletePet(new PetStore.DeletePet.In(id), ctx);
        Assert(delOut.Deleted, "UEBA DeletePet");

        Console.WriteLine("UEBA round-trip OK");
    }

    private static void RunJson(BaboonCodecContext ctx)
    {
        Impl impl = new Impl();
        PetStoreClient client = NewClient(impl, ctx);

        PetStore.AddPet.Out addOut = client.AddPetJson(new PetStore.AddPet.In("Whiskers", PetStatus.Pending, "cat"), ctx);
        Assert(addOut.Pet.Name == "Whiskers", $"JSON AddPet name: {addOut.Pet.Name}");
        long id = addOut.Pet.Id;

        PetStore.GetPet.Out getOut = client.GetPetJson(new PetStore.GetPet.In(id), ctx);
        Assert(getOut.Pet.Status == PetStatus.Pending, "JSON GetPet status");

        PetStore.ListPets.Out listOut = client.ListPetsJson(new PetStore.ListPets.In(), ctx);
        Assert(listOut.Pets.Count == 1, $"JSON ListPets count: {listOut.Pets.Count}");

        PetStore.DeletePet.Out delOut = client.DeletePetJson(new PetStore.DeletePet.In(id), ctx);
        Assert(delOut.Deleted, "JSON DeletePet");

        Console.WriteLine("JSON round-trip OK");
    }

    private static void Assert(bool cond, string msg)
    {
        if (!cond)
        {
            Console.Error.WriteLine("ASSERT FAILED: " + msg);
            Environment.Exit(1);
        }
    }
}
