import * as http from 'node:http';
import { PetStore } from './generated/petstore/api/index.js';
import { PetStatus } from './generated/petstore/api/PetStatus.js';

// Per-method input types are reached through the service namespace barrel.
const AddPetIn = PetStore.methods.addpet.In;
const GetPetIn = PetStore.methods.getpet.In;
const ListPetsIn = PetStore.methods.listpets.In;
const DeletePetIn = PetStore.methods.deletepet.In;

function post(host: string, port: number, path: string, body: string): Promise<string> {
  return new Promise((resolve, reject) => {
    const req = http.request({ host, port, path, method: 'POST', headers: { 'Content-Type': 'application/json' } }, (res) => {
      const chunks: Buffer[] = [];
      res.on('data', (chunk) => chunks.push(chunk));
      res.on('end', () => {
        const responseBody = Buffer.concat(chunks).toString('utf-8');
        if (res.statusCode !== 200) {
          reject(new Error(`HTTP ${res.statusCode}: ${responseBody}`));
          return;
        }
        resolve(responseBody);
      });
    });
    req.on('error', reject);
    req.end(body);
  });
}

function postBytes(host: string, port: number, path: string, body: Uint8Array): Promise<Uint8Array> {
  return new Promise((resolve, reject) => {
    const req = http.request({ host, port, path, method: 'POST', headers: { 'Content-Type': 'application/octet-stream' } }, (res) => {
      const chunks: Buffer[] = [];
      res.on('data', (chunk) => chunks.push(chunk));
      res.on('end', () => {
        const responseBuf = Buffer.concat(chunks);
        if (res.statusCode !== 200) {
          reject(new Error(`HTTP ${res.statusCode}: ${responseBuf.toString('utf-8')}`));
          return;
        }
        resolve(new Uint8Array(responseBuf));
      });
    });
    req.on('error', reject);
    req.end(Buffer.from(body));
  });
}

function assert(condition: boolean, msg: string): void {
  if (!condition) throw new Error(`Assertion failed: ${msg}`);
}

type PetStoreClientType = InstanceType<typeof PetStore.PetStoreClient>;

async function runJson(host: string, port: number, client: PetStoreClientType): Promise<void> {
  // --- JSON scenario ---
  await post(host, port, '/reset', '');

  const addBuddyOut = await client.addPetJson(new AddPetIn('Buddy', PetStatus.Available, 'dog'));
  const buddyId = addBuddyOut.pet.id;
  assert(addBuddyOut.pet.name === 'Buddy', `expected name Buddy, got ${addBuddyOut.pet.name}`);

  const addWhiskersOut = await client.addPetJson(new AddPetIn('Whiskers', PetStatus.Pending, 'cat'));
  const whiskersId = addWhiskersOut.pet.id;
  assert(addWhiskersOut.pet.name === 'Whiskers', `expected name Whiskers, got ${addWhiskersOut.pet.name}`);

  const listOut = await client.listPetsJson(new ListPetsIn());
  assert(listOut.pets.length === 2, `expected 2 pets, got ${listOut.pets.length}`);

  const getBuddyOut = await client.getPetJson(new GetPetIn(buddyId));
  assert(getBuddyOut.pet.name === 'Buddy', `expected Buddy, got ${getBuddyOut.pet.name}`);
  assert(getBuddyOut.pet.status === PetStatus.Available, `expected Available, got ${getBuddyOut.pet.status}`);
  assert(getBuddyOut.pet.tag === 'dog', `expected tag dog, got ${getBuddyOut.pet.tag}`);

  const deleteOut = await client.deletePetJson(new DeletePetIn(whiskersId));
  assert(deleteOut.deleted === true, `expected deleted=true, got ${deleteOut.deleted}`);

  const list2Out = await client.listPetsJson(new ListPetsIn());
  assert(list2Out.pets.length === 1, `expected 1 pet, got ${list2Out.pets.length}`);
  assert(list2Out.pets[0].name === 'Buddy', `expected remaining pet Buddy, got ${list2Out.pets[0].name}`);
}

async function runUeba(host: string, port: number, client: PetStoreClientType): Promise<void> {
  // --- UEBA scenario (bare method names, binary transport) ---
  // Identical operations, asserting the same results over the UEBA codec.
  await post(host, port, '/reset', '');

  const uebaAddBuddy = await client.addPet(new AddPetIn('Buddy', PetStatus.Available, 'dog'));
  const uebaBuddyId = uebaAddBuddy.pet.id;
  assert(uebaAddBuddy.pet.name === 'Buddy', `[ueba] expected name Buddy, got ${uebaAddBuddy.pet.name}`);

  const uebaAddWhiskers = await client.addPet(new AddPetIn('Whiskers', PetStatus.Pending, 'cat'));
  const uebaWhiskersId = uebaAddWhiskers.pet.id;
  assert(uebaAddWhiskers.pet.name === 'Whiskers', `[ueba] expected name Whiskers, got ${uebaAddWhiskers.pet.name}`);

  const uebaList = await client.listPets(new ListPetsIn());
  assert(uebaList.pets.length === 2, `[ueba] expected 2 pets, got ${uebaList.pets.length}`);

  const uebaGetBuddy = await client.getPet(new GetPetIn(uebaBuddyId));
  assert(uebaGetBuddy.pet.name === 'Buddy', `[ueba] expected Buddy, got ${uebaGetBuddy.pet.name}`);
  assert(uebaGetBuddy.pet.status === PetStatus.Available, `[ueba] expected Available, got ${uebaGetBuddy.pet.status}`);
  assert(uebaGetBuddy.pet.tag === 'dog', `[ueba] expected tag dog, got ${uebaGetBuddy.pet.tag}`);

  const uebaDelete = await client.deletePet(new DeletePetIn(uebaWhiskersId));
  assert(uebaDelete.deleted === true, `[ueba] expected deleted=true, got ${uebaDelete.deleted}`);

  const uebaList2 = await client.listPets(new ListPetsIn());
  assert(uebaList2.pets.length === 1, `[ueba] expected 1 pet, got ${uebaList2.pets.length}`);
  assert(uebaList2.pets[0].name === 'Buddy', `[ueba] expected remaining pet Buddy, got ${uebaList2.pets[0].name}`);
}

export async function runClient(host: string, port: number, codec: string = 'both'): Promise<void> {
  // Drive the compiler-generated client over HTTP. The generated client takes
  // both a binary (UEBA) transport and a string (JSON) transport.
  const client = new PetStore.PetStoreClient(
    (service, method, data) => postBytes(host, port, `/${service}/${method}`, data),
    (service, method, data) => post(host, port, `/${service}/${method}`, data)
  );

  if (codec === 'json') {
    await runJson(host, port, client);
  } else if (codec === 'ueba') {
    await runUeba(host, port, client);
  } else if (codec === 'both') {
    await runJson(host, port, client);
    await runUeba(host, port, client);
  } else {
    throw new Error(`Unknown --codec value: ${codec} (expected json|ueba|both)`);
  }

  console.log('OK');
}
