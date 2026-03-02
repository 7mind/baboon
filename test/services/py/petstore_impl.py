from Generated.petstore.api.Pet import Pet
from Generated.petstore.api.PetStatus import PetStatus
from Generated.petstore.api.petstore.addpet.In import In as AddPetIn
from Generated.petstore.api.petstore.addpet.Out import Out as AddPetOut
from Generated.petstore.api.petstore.getpet.In import In as GetPetIn
from Generated.petstore.api.petstore.getpet.Out import Out as GetPetOut
from Generated.petstore.api.petstore.listpets.In import In as ListPetsIn
from Generated.petstore.api.petstore.listpets.Out import Out as ListPetsOut
from Generated.petstore.api.petstore.deletepet.In import In as DeletePetIn
from Generated.petstore.api.petstore.deletepet.Out import Out as DeletePetOut

class PetStoreImpl:
    def __init__(self) -> None:
        self._pets: dict[int, Pet] = {}
        self._next_id: int = 1

    def reset(self) -> None:
        self._pets.clear()
        self._next_id = 1

    def add_pet(self, inp: AddPetIn) -> AddPetOut:
        pet_id = self._next_id
        self._next_id += 1
        pet = Pet(id=pet_id, name=inp.name, status=inp.status, tag=inp.tag)
        self._pets[pet_id] = pet
        return AddPetOut(pet=pet)

    def get_pet(self, inp: GetPetIn) -> GetPetOut:
        pet = self._pets.get(inp.id)
        if pet is None:
            raise KeyError(f"Pet not found: id={inp.id}")
        return GetPetOut(pet=pet)

    def list_pets(self, inp: ListPetsIn) -> ListPetsOut:
        sorted_pets = sorted(self._pets.values(), key=lambda p: p.id)
        return ListPetsOut(pets=sorted_pets)

    def delete_pet(self, inp: DeletePetIn) -> DeletePetOut:
        existed = inp.id in self._pets
        if existed:
            del self._pets[inp.id]
        return DeletePetOut(deleted=existed)
