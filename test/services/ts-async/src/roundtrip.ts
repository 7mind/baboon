// In-process async wiring round-trip for the TS async service lane (T84).
// Exercises the async invoke dispatchers (invokeJson_PetStore / invokeUeba_PetStore)
// generated with --ts-async-services=true --service-result-no-errors=true.
// Client is always Promise-based (T83 Option B — client transport callbacks are async).
import { BaboonCodecContext } from './generated/BaboonSharedRuntime.js';
import type { BaboonMethodId } from './generated/BaboonSharedRuntime.js';
import { PetStore } from './generated/petstore/api/index.js';
import { PetStatus } from './generated/petstore/api/PetStatus.js';
import { PetStoreAsyncImpl } from './petstore_impl.js';

function assert(condition: boolean, msg: string): void {
  if (!condition) throw new Error(`Assertion failed: ${msg}`);
}

async function runJson(impl: PetStoreAsyncImpl, ctx: BaboonCodecContext): Promise<void> {
  impl.reset();

  // --- direct async dispatcher (JSON) ---
  const addMethod: BaboonMethodId = { serviceName: 'PetStore', methodName: 'addPet' };
  const addInJson = JSON.stringify({ name: 'Buddy', status: 'Available', tag: 'dog' });
  const addOutJson = await PetStore.invokeJson_PetStore(addMethod, addInJson, impl, ctx);
  const addOutObj = JSON.parse(addOutJson) as { pet: { id: unknown; name: string } };
  assert(addOutObj.pet.name === 'Buddy', `expected name Buddy, got ${addOutObj.pet.name}`);

  // --- generated client over in-process async transport (JSON) ---
  const client = new PetStore.PetStoreClient(
    async (service, method, data) =>
      PetStore.invokeUeba_PetStore({ serviceName: service, methodName: method }, data, impl, ctx),
    async (service, method, data) =>
      PetStore.invokeJson_PetStore({ serviceName: service, methodName: method }, data, impl, ctx)
  );

  impl.reset();
  const addPetIn = PetStore.methods.addpet.In;
  const listPetsIn = PetStore.methods.listpets.In;
  const getPetIn = PetStore.methods.getpet.In;
  const deletePetIn = PetStore.methods.deletepet.In;

  const buddyOut = await client.addPetJson(new addPetIn('Buddy', PetStatus.Available, 'dog'));
  const buddyId = buddyOut.pet.id;
  assert(buddyOut.pet.name === 'Buddy', `expected Buddy, got ${buddyOut.pet.name}`);

  const whiskersOut = await client.addPetJson(new addPetIn('Whiskers', PetStatus.Pending, 'cat'));
  const whiskersId = whiskersOut.pet.id;
  assert(whiskersOut.pet.name === 'Whiskers', `expected Whiskers, got ${whiskersOut.pet.name}`);

  const listOut = await client.listPetsJson(new listPetsIn());
  assert(listOut.pets.length === 2, `expected 2 pets, got ${listOut.pets.length}`);

  const getBuddyOut = await client.getPetJson(new getPetIn(buddyId));
  assert(getBuddyOut.pet.name === 'Buddy', `expected Buddy, got ${getBuddyOut.pet.name}`);
  assert(getBuddyOut.pet.status === PetStatus.Available, `expected Available, got ${getBuddyOut.pet.status}`);
  assert(getBuddyOut.pet.tag === 'dog', `expected tag dog, got ${getBuddyOut.pet.tag}`);

  const deleteOut = await client.deletePetJson(new deletePetIn(whiskersId));
  assert(deleteOut.deleted === true, `expected deleted=true, got ${deleteOut.deleted}`);

  const list2Out = await client.listPetsJson(new listPetsIn());
  assert(list2Out.pets.length === 1, `expected 1 pet, got ${list2Out.pets.length}`);
  assert(list2Out.pets[0].name === 'Buddy', `expected remaining pet Buddy, got ${list2Out.pets[0].name}`);
}

async function runUeba(impl: PetStoreAsyncImpl, ctx: BaboonCodecContext): Promise<void> {
  impl.reset();

  // --- generated client over in-process async transport (UEBA binary) ---
  const client = new PetStore.PetStoreClient(
    async (service, method, data) =>
      PetStore.invokeUeba_PetStore({ serviceName: service, methodName: method }, data, impl, ctx),
    async (service, method, data) =>
      PetStore.invokeJson_PetStore({ serviceName: service, methodName: method }, data, impl, ctx)
  );

  const addPetIn = PetStore.methods.addpet.In;
  const listPetsIn = PetStore.methods.listpets.In;
  const getPetIn = PetStore.methods.getpet.In;
  const deletePetIn = PetStore.methods.deletepet.In;

  const buddyOut = await client.addPet(new addPetIn('Buddy', PetStatus.Available, 'dog'));
  const buddyId = buddyOut.pet.id;
  assert(buddyOut.pet.name === 'Buddy', `[ueba] expected Buddy, got ${buddyOut.pet.name}`);

  const whiskersOut = await client.addPet(new addPetIn('Whiskers', PetStatus.Pending, 'cat'));
  const whiskersId = whiskersOut.pet.id;
  assert(whiskersOut.pet.name === 'Whiskers', `[ueba] expected Whiskers, got ${whiskersOut.pet.name}`);

  const listOut = await client.listPets(new listPetsIn());
  assert(listOut.pets.length === 2, `[ueba] expected 2 pets, got ${listOut.pets.length}`);

  const getBuddyOut = await client.getPet(new getPetIn(buddyId));
  assert(getBuddyOut.pet.name === 'Buddy', `[ueba] expected Buddy, got ${getBuddyOut.pet.name}`);
  assert(getBuddyOut.pet.status === PetStatus.Available, `[ueba] expected Available, got ${getBuddyOut.pet.status}`);
  assert(getBuddyOut.pet.tag === 'dog', `[ueba] expected tag dog, got ${getBuddyOut.pet.tag}`);

  const deleteOut = await client.deletePet(new deletePetIn(whiskersId));
  assert(deleteOut.deleted === true, `[ueba] expected deleted=true, got ${deleteOut.deleted}`);

  const list2Out = await client.listPets(new listPetsIn());
  assert(list2Out.pets.length === 1, `[ueba] expected 1 pet, got ${list2Out.pets.length}`);
  assert(list2Out.pets[0].name === 'Buddy', `[ueba] expected remaining pet Buddy, got ${list2Out.pets[0].name}`);
}

async function main(): Promise<void> {
  const ctx = BaboonCodecContext.Default;
  const impl = new PetStoreAsyncImpl();

  await runJson(impl, ctx);
  await runUeba(impl, ctx);

  console.log('OK');
}

main().catch((err) => {
  console.error('FAILED:', err);
  process.exit(1);
});
