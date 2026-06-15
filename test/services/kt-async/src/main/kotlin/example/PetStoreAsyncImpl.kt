package example

import java.util.concurrent.atomic.AtomicLong

/**
 * Async server implementation attempting to implement the generated PetStore
 * interface with `suspend fun` methods.
 *
 * This FAILS to compile because `--kt-async-services=true` does NOT yet make
 * the generated interface methods `suspend fun` (KtDefnTranslator:446 emits
 * `fun $name(...)` unconditionally, and KtServiceWiringTranslator invokes the
 * impl synchronously). An async server impl overriding with `suspend fun`
 * cannot satisfy the generated non-suspend interface — reproducing D25.
 *
 * Expected kotlinc error:
 *   'suspend fun' overrides nothing (the interface declares plain 'fun')
 *   or: overriding 'suspend fun' with non-suspend declaration is not allowed.
 */
class PetStoreAsyncImpl : petstore.api.PetStore {
    private val pets = mutableMapOf<Long, petstore.api.Pet>()
    private val nextId = AtomicLong(1)

    fun reset() {
        pets.clear()
        nextId.set(1)
    }

    // These methods use `suspend` but the generated PetStore interface declares
    // plain `fun`. The Kotlin compiler rejects overriding a non-suspend
    // function with a suspend one:
    //   error: 'suspend' modifier is not applicable on this function
    //   (overriding non-suspend declaration)
    // reproducing D25.

    override suspend fun addPet(arg: petstore.api.petstore.addpet.In): petstore.api.petstore.addpet.Out {
        val id = nextId.getAndIncrement()
        val pet = petstore.api.Pet(
            id = id,
            name = arg.name,
            status = arg.status,
            tag = arg.tag
        )
        pets[id] = pet
        return petstore.api.petstore.addpet.Out(pet = pet)
    }

    override suspend fun getPet(arg: petstore.api.petstore.getpet.In): petstore.api.petstore.getpet.Out {
        val pet = pets[arg.id] ?: throw NoSuchElementException("Pet not found: id=${arg.id}")
        return petstore.api.petstore.getpet.Out(pet = pet)
    }

    override suspend fun listPets(arg: petstore.api.petstore.listpets.In): petstore.api.petstore.listpets.Out {
        val sorted = pets.values.sortedBy { it.id }
        return petstore.api.petstore.listpets.Out(pets = sorted)
    }

    override suspend fun deletePet(arg: petstore.api.petstore.deletepet.In): petstore.api.petstore.deletepet.Out {
        val existed = pets.remove(arg.id) != null
        return petstore.api.petstore.deletepet.Out(deleted = existed)
    }
}
