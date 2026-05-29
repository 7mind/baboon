import { Pet, PetStore } from './generated/petstore/api/index.js';

export class PetStoreImpl {
  private pets: Map<bigint, Pet> = new Map();
  private nextId: bigint = 1n;

  reset(): void {
    this.pets.clear();
    this.nextId = 1n;
  }

  addPet(input: PetStore.methods.addpet.In): PetStore.methods.addpet.Out {
    const id = this.nextId++;
    const pet = new Pet(id, input.name, input.status, input.tag);
    this.pets.set(id, pet);
    return new PetStore.methods.addpet.Out(pet);
  }

  getPet(input: PetStore.methods.getpet.In): PetStore.methods.getpet.Out {
    const pet = this.pets.get(input.id);
    if (pet === undefined) {
      throw new Error(`Pet not found: id=${input.id}`);
    }
    return new PetStore.methods.getpet.Out(pet);
  }

  listPets(_input: PetStore.methods.listpets.In): PetStore.methods.listpets.Out {
    const sorted = [...this.pets.values()].sort((a, b) =>
      a.id < b.id ? -1 : a.id > b.id ? 1 : 0
    );
    return new PetStore.methods.listpets.Out(sorted);
  }

  deletePet(input: PetStore.methods.deletepet.In): PetStore.methods.deletepet.Out {
    const existed = this.pets.delete(input.id);
    return new PetStore.methods.deletepet.Out(existed);
  }
}
