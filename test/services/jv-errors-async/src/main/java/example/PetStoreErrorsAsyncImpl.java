package example;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.Map;

import baboon.runtime.shared.BaboonEither;
import petstore.api.Pet;
import petstore.api.PetError;
import petstore.api.PetStatus;

/**
 * Errors+async server implementation for the petstore-errors model.
 *
 * This FAILS to compile because JvServiceWiringTranslator emits server dispatch
 * code (invokeJson / invokeUeba) that calls impl.method(v) inside a lambda
 * passed to rt.leftMap/rt.pure, treating the return value as a bare
 * BaboonEither<Err, Out>. But when --jv-async-services=true the generated
 * PetStore interface declares:
 *
 *   CompletableFuture<BaboonEither<Err, Out>> addPet(In arg);
 *
 * so impl.method(v) actually returns CompletableFuture<BaboonEither<Err, Out>>.
 * The wiring then passes that CompletableFuture to rt.leftMap expecting
 * BaboonEither<Err, Out> — a type mismatch (D26 double-wrap). Additionally,
 * errorsFutureWrap() wraps the whole container expression in another
 * CompletableFuture.completedFuture(...), producing a doubly-wrapped future.
 *
 * Expected javac errors (type mismatch at rt.pure/rt.leftMap call-sites):
 *   error: incompatible types: CompletableFuture<BaboonEither<...>> cannot be
 *   converted to BaboonEither<...>
 *
 * This reproduces defect D26 (errors+async wiring) for the Java backend.
 */
public final class PetStoreErrorsAsyncImpl implements petstore.api.PetStore {
    private final Map<Long, Pet> pets = new ConcurrentHashMap<>();
    private final AtomicLong nextId = new AtomicLong(1);

    // These methods return CompletableFuture<BaboonEither<Err, Out>> because:
    //   - --jv-async-services=true  => wrap in CompletableFuture
    //   - --service-result-no-errors=false  => wrap in BaboonEither<Err, Out>
    // The generated PetStoreWiring calls impl.addPet(v) inside rt.leftMap
    // expecting BaboonEither<Err, Out> — but gets CompletableFuture<BaboonEither<...>>
    // causing the D26 double-wrap type error.

    @Override
    public CompletableFuture<BaboonEither<petstore.api.petstore.addpet.Err, petstore.api.petstore.addpet.Out>> addPet(
            petstore.api.petstore.addpet.In arg) {
        long id = nextId.getAndIncrement();
        Pet pet = new Pet(id, arg.name(), arg.status(), arg.tag());
        pets.put(id, pet);
        return CompletableFuture.completedFuture(
            BaboonEither.right(new petstore.api.petstore.addpet.Out(pet)));
    }

    @Override
    public CompletableFuture<BaboonEither<petstore.api.petstore.getpet.Err, petstore.api.petstore.getpet.Out>> getPet(
            petstore.api.petstore.getpet.In arg) {
        Pet pet = pets.get(arg.id());
        if (pet != null) {
            return CompletableFuture.completedFuture(
                BaboonEither.right(new petstore.api.petstore.getpet.Out(pet)));
        } else {
            return CompletableFuture.completedFuture(
                BaboonEither.left(new petstore.api.petstore.getpet.Err(
                    new PetError(404, "Pet not found: " + arg.id()))));
        }
    }
}
