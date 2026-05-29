using System;
using System.Collections.Generic;
using System.Linq;

namespace ServiceTest;

public class PetStoreImpl : Petstore.Api.IPetStore
{
    private readonly Dictionary<long, Petstore.Api.Pet> _pets = new();
    private long _nextId = 1;

    public void Reset()
    {
        _pets.Clear();
        _nextId = 1;
    }

    public Petstore.Api.PetStore.AddPet.Out addPet(Petstore.Api.PetStore.AddPet.In arg)
    {
        long id = _nextId++;
        Petstore.Api.Pet pet = new Petstore.Api.Pet(id, arg.Name, arg.Status, arg.Tag);
        _pets[id] = pet;
        return new Petstore.Api.PetStore.AddPet.Out(pet);
    }

    public Petstore.Api.PetStore.GetPet.Out getPet(Petstore.Api.PetStore.GetPet.In arg)
    {
        if (!_pets.TryGetValue(arg.Id, out Petstore.Api.Pet? pet))
        {
            throw new KeyNotFoundException($"Pet not found: id={arg.Id}");
        }
        return new Petstore.Api.PetStore.GetPet.Out(pet);
    }

    public Petstore.Api.PetStore.ListPets.Out listPets(Petstore.Api.PetStore.ListPets.In arg)
    {
        List<Petstore.Api.Pet> sorted = _pets.Values.OrderBy(p => p.Id).ToList();
        return new Petstore.Api.PetStore.ListPets.Out(sorted);
    }

    public Petstore.Api.PetStore.DeletePet.Out deletePet(Petstore.Api.PetStore.DeletePet.In arg)
    {
        bool existed = _pets.Remove(arg.Id);
        return new Petstore.Api.PetStore.DeletePet.Out(existed);
    }
}
