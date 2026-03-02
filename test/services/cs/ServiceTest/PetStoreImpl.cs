using System;
using System.Collections.Generic;
using System.Linq;

namespace ServiceTest;

public class PetStoreImpl : Petstore.Api.PetStore.PetStore
{
    private readonly Dictionary<long, Petstore.Api.Pet> _pets = new();
    private long _nextId = 1;

    public void Reset()
    {
        _pets.Clear();
        _nextId = 1;
    }

    public Petstore.Api.PetStore.addPet.Out addPet(Petstore.Api.PetStore.addPet.In arg)
    {
        long id = _nextId++;
        Petstore.Api.Pet pet = new Petstore.Api.Pet(id, arg.Name, arg.Status, arg.Tag);
        _pets[id] = pet;
        return new Petstore.Api.PetStore.addPet.Out(pet);
    }

    public Petstore.Api.PetStore.getPet.Out getPet(Petstore.Api.PetStore.getPet.In arg)
    {
        if (!_pets.TryGetValue(arg.Id, out Petstore.Api.Pet? pet))
        {
            throw new KeyNotFoundException($"Pet not found: id={arg.Id}");
        }
        return new Petstore.Api.PetStore.getPet.Out(pet);
    }

    public Petstore.Api.PetStore.listPets.Out listPets(Petstore.Api.PetStore.listPets.In arg)
    {
        List<Petstore.Api.Pet> sorted = _pets.Values.OrderBy(p => p.Id).ToList();
        return new Petstore.Api.PetStore.listPets.Out(sorted);
    }

    public Petstore.Api.PetStore.deletePet.Out deletePet(Petstore.Api.PetStore.deletePet.In arg)
    {
        bool existed = _pets.Remove(arg.Id);
        return new Petstore.Api.PetStore.deletePet.Out(existed);
    }
}
