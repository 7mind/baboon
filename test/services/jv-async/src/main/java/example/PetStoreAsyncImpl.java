package example;

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

import petstore.api.Pet;
import petstore.api.PetStatus;

/**
 * Async server implementation attempting to implement the generated PetStore
 * interface with CompletableFuture<T> return types.
 *
 * This FAILS to compile because --jv-async-services=true only makes the
 * generated CLIENT return CompletableFuture<T>, but the generated server
 * interface (PetStore.java) still declares bare T return types. An async
 * server impl returning CompletableFuture<T> does not satisfy the generated
 * synchronous interface — reproducing D25.
 */
public final class PetStoreAsyncImpl implements petstore.api.PetStore {
    private final Map<Long, Pet> pets = new ConcurrentHashMap<>();
    private final AtomicLong nextId = new AtomicLong(1);

    // These methods return CompletableFuture<T>, but the generated PetStore
    // interface declares bare T return types. The @Override annotations will
    // cause javac to reject the file with:
    //   error: method does not override or implement a method from a supertype
    // and the return-type mismatch will surface as:
    //   error: addPet(petstore.api.petstore.addpet.In) in PetStoreAsyncImpl
    //   cannot override addPet(petstore.api.petstore.addpet.In) in PetStore
    //   return type CompletableFuture is not compatible with
    //   petstore.api.petstore.addpet.Out

    @Override
    public CompletableFuture<petstore.api.petstore.addpet.Out> addPet(
            petstore.api.petstore.addpet.In arg) {
        long id = nextId.getAndIncrement();
        Pet pet = new Pet(id, arg.name(), arg.status(), arg.tag());
        pets.put(id, pet);
        return CompletableFuture.completedFuture(
            new petstore.api.petstore.addpet.Out(pet));
    }

    @Override
    public CompletableFuture<petstore.api.petstore.getpet.Out> getPet(
            petstore.api.petstore.getpet.In arg) {
        Pet pet = pets.get(arg.id());
        if (pet == null) {
            throw new RuntimeException("Pet not found: " + arg.id());
        }
        return CompletableFuture.completedFuture(
            new petstore.api.petstore.getpet.Out(pet));
    }

    @Override
    public CompletableFuture<petstore.api.petstore.listpets.Out> listPets(
            petstore.api.petstore.listpets.In arg) {
        List<Pet> sorted = pets.values().stream()
            .sorted(Comparator.comparingLong(Pet::id))
            .toList();
        return CompletableFuture.completedFuture(
            new petstore.api.petstore.listpets.Out(sorted));
    }

    @Override
    public CompletableFuture<petstore.api.petstore.deletepet.Out> deletePet(
            petstore.api.petstore.deletepet.In arg) {
        Pet removed = pets.remove(arg.id());
        return CompletableFuture.completedFuture(
            new petstore.api.petstore.deletepet.Out(removed != null));
    }
}
