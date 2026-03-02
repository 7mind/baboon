package example

import java.util.concurrent.atomic.AtomicLong

class PetStoreImpl : petstore.api.PetStore {
    private val pets = mutableMapOf<Long, petstore.api.Pet>()
    private val nextId = AtomicLong(1)

    fun reset() {
        pets.clear()
        nextId.set(1)
    }

    override fun addPet(arg: petstore.api.petstore.addpet.In): petstore.api.petstore.addpet.Out {
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

    override fun getPet(arg: petstore.api.petstore.getpet.In): petstore.api.petstore.getpet.Out {
        val pet = pets[arg.id] ?: throw NoSuchElementException("Pet not found: id=${arg.id}")
        return petstore.api.petstore.getpet.Out(pet = pet)
    }

    override fun listPets(arg: petstore.api.petstore.listpets.In): petstore.api.petstore.listpets.Out {
        val sorted = pets.values.sortedBy { it.id }
        return petstore.api.petstore.listpets.Out(pets = sorted)
    }

    override fun deletePet(arg: petstore.api.petstore.deletepet.In): petstore.api.petstore.deletepet.Out {
        val existed = pets.remove(arg.id) != null
        return petstore.api.petstore.deletepet.Out(deleted = existed)
    }
}
