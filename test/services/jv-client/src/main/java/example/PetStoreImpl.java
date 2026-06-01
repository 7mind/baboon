package example;

import petstore.api.Pet;
import petstore.api.PetStatus;

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

public final class PetStoreImpl implements petstore.api.PetStore {
    private final Map<Long, Pet> pets = new ConcurrentHashMap<>();
    private final AtomicLong nextId = new AtomicLong(1);

    public void reset() {
        pets.clear();
        nextId.set(1);
    }

    @Override
    public petstore.api.petstore.addpet.Out addPet(petstore.api.petstore.addpet.In arg) {
        long id = nextId.getAndIncrement();
        Pet pet = new Pet(id, arg.name(), arg.status(), arg.tag());
        pets.put(id, pet);
        return new petstore.api.petstore.addpet.Out(pet);
    }

    @Override
    public petstore.api.petstore.getpet.Out getPet(petstore.api.petstore.getpet.In arg) {
        Pet pet = pets.get(arg.id());
        if (pet == null) {
            throw new RuntimeException("Pet not found: " + arg.id());
        }
        return new petstore.api.petstore.getpet.Out(pet);
    }

    @Override
    public petstore.api.petstore.listpets.Out listPets(petstore.api.petstore.listpets.In arg) {
        List<Pet> sorted = pets.values().stream()
            .sorted(Comparator.comparingLong(Pet::id))
            .toList();
        return new petstore.api.petstore.listpets.Out(sorted);
    }

    @Override
    public petstore.api.petstore.deletepet.Out deletePet(petstore.api.petstore.deletepet.In arg) {
        Pet removed = pets.remove(arg.id());
        return new petstore.api.petstore.deletepet.Out(removed != null);
    }
}
