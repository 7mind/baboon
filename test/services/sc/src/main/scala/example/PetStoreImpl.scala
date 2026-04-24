package example

import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable

class PetStoreImpl extends petstore.api.PetStore {
  private val idCounter = new AtomicLong(0)
  private val pets      = mutable.Map.empty[Long, petstore.api.Pet]

  def reset(): Unit = {
    idCounter.set(0)
    pets.clear()
  }

  override def addPet(arg: petstore.api.petstore.addpet.In): petstore.api.petstore.addpet.Out = {
    val id = idCounter.incrementAndGet()
    val pet = petstore.api.Pet(
      id     = id,
      name   = arg.name,
      status = arg.status,
      tag    = arg.tag,
    )
    pets.put(id, pet)
    petstore.api.petstore.addpet.Out(pet)
  }

  override def getPet(arg: petstore.api.petstore.getpet.In): petstore.api.petstore.getpet.Out = {
    val pet = pets.getOrElse(arg.id, throw new NoSuchElementException(s"Pet ${arg.id} not found"))
    petstore.api.petstore.getpet.Out(pet)
  }

  override def listPets(arg: petstore.api.petstore.listpets.In): petstore.api.petstore.listpets.Out = {
    petstore.api.petstore.listpets.Out(pets.values.toList.sortBy(_.id))
  }

  override def deletePet(arg: petstore.api.petstore.deletepet.In): petstore.api.petstore.deletepet.Out = {
    val removed = pets.remove(arg.id).isDefined
    petstore.api.petstore.deletepet.Out(removed)
  }
}
