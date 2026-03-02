import { Pet } from './generated/petstore/api/Pet.js';
import { PetStatus } from './generated/petstore/api/PetStatus.js';
import { In as AddPetIn } from './generated/petstore/api/petstore/addpet/in.js';
import { Out as AddPetOut } from './generated/petstore/api/petstore/addpet/out.js';
import { In as GetPetIn } from './generated/petstore/api/petstore/getpet/in.js';
import { Out as GetPetOut } from './generated/petstore/api/petstore/getpet/out.js';
import { In as ListPetsIn } from './generated/petstore/api/petstore/listpets/in.js';
import { Out as ListPetsOut } from './generated/petstore/api/petstore/listpets/out.js';
import { In as DeletePetIn } from './generated/petstore/api/petstore/deletepet/in.js';
import { Out as DeletePetOut } from './generated/petstore/api/petstore/deletepet/out.js';

export class PetStoreImpl {
  private pets: Map<bigint, Pet> = new Map();
  private nextId: bigint = 1n;

  reset(): void {
    this.pets.clear();
    this.nextId = 1n;
  }

  addPet(input: AddPetIn): AddPetOut {
    const id = this.nextId++;
    const pet = new Pet(id, input.name, input.status, input.tag);
    this.pets.set(id, pet);
    return new AddPetOut(pet);
  }

  getPet(input: GetPetIn): GetPetOut {
    const pet = this.pets.get(input.id);
    if (pet === undefined) {
      throw new Error(`Pet not found: id=${input.id}`);
    }
    return new GetPetOut(pet);
  }

  listPets(_input: ListPetsIn): ListPetsOut {
    const sorted = [...this.pets.values()].sort((a, b) =>
      a.id < b.id ? -1 : a.id > b.id ? 1 : 0
    );
    return new ListPetsOut(sorted);
  }

  deletePet(input: DeletePetIn): DeletePetOut {
    const existed = this.pets.delete(input.id);
    return new DeletePetOut(existed);
  }
}
