import { Pet, PetStore } from './generated/petstore/api/index.js';

// Async petstore impl: methods return Promise<Out> to satisfy the
// service interface generated with --ts-async-services=true.
export class PetStoreAsyncImpl {
  private pets: Map<bigint, Pet> = new Map();
  private nextId: bigint = 1n;

  reset(): void {
    this.pets.clear();
    this.nextId = 1n;
  }

  async addPet(arg: PetStore.methods.addpet.In): Promise<PetStore.methods.addpet.Out> {
    const id = this.nextId++;
    const pet = new Pet(id, arg.name, arg.status, arg.tag);
    this.pets.set(id, pet);
    return new PetStore.methods.addpet.Out(pet);
  }

  async getPet(arg: PetStore.methods.getpet.In): Promise<PetStore.methods.getpet.Out> {
    const pet = this.pets.get(arg.id);
    if (pet === undefined) {
      throw new Error(`Pet not found: id=${arg.id}`);
    }
    return new PetStore.methods.getpet.Out(pet);
  }

  async listPets(_arg: PetStore.methods.listpets.In): Promise<PetStore.methods.listpets.Out> {
    const sorted = [...this.pets.values()].sort((a, b) =>
      a.id < b.id ? -1 : a.id > b.id ? 1 : 0
    );
    return new PetStore.methods.listpets.Out(sorted);
  }

  async deletePet(arg: PetStore.methods.deletepet.In): Promise<PetStore.methods.deletepet.Out> {
    const existed = this.pets.delete(arg.id);
    return new PetStore.methods.deletepet.Out(existed);
  }
}
