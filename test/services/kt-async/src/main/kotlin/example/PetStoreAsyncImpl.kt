package example

import java.util.concurrent.atomic.AtomicLong

/**
 * Async server implementation of the generated PetStore interface using
 * `suspend fun` methods.
 *
 * With --kt-async-services=true (fixed in T76/T77), the generated PetStore
 * interface declares `suspend fun` methods and the server dispatchers
 * `suspend`-call the impl. This impl compiles and round-trips correctly as a
 * GREEN regression guard for D25.
 */
class PetStoreAsyncImpl : petstore.api.PetStore {
    private val pets = mutableMapOf<Long, petstore.api.Pet>()
    private val nextId = AtomicLong(1)

    fun reset() {
        pets.clear()
        nextId.set(1)
    }

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
