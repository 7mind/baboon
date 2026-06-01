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

function assert(condition: boolean, msg: string): void {
  if (!condition) throw new Error(`Assertion failed: ${msg}`);
}

export async function runClient(host: string, port: number): Promise<void> {
  // Drive the compiler-generated client over an HTTP/JSON transport.
  const client = new PetStore.PetStoreClient(
    (service, method, data) => post(host, port, `/${service}/${method}`, data)
  );

  // Reset
  await post(host, port, '/reset', '');

  // Add Buddy
  const addBuddyOut = await client.addPetJson(new AddPetIn('Buddy', PetStatus.Available, 'dog'));
  const buddyId = addBuddyOut.pet.id;
  assert(addBuddyOut.pet.name === 'Buddy', `expected name Buddy, got ${addBuddyOut.pet.name}`);

  // Add Whiskers
  const addWhiskersOut = await client.addPetJson(new AddPetIn('Whiskers', PetStatus.Pending, 'cat'));
  const whiskersId = addWhiskersOut.pet.id;
  assert(addWhiskersOut.pet.name === 'Whiskers', `expected name Whiskers, got ${addWhiskersOut.pet.name}`);

  // List pets (expect 2)
  const listOut = await client.listPetsJson(new ListPetsIn());
  assert(listOut.pets.length === 2, `expected 2 pets, got ${listOut.pets.length}`);

  // Get Buddy
  const getBuddyOut = await client.getPetJson(new GetPetIn(buddyId));
  assert(getBuddyOut.pet.name === 'Buddy', `expected Buddy, got ${getBuddyOut.pet.name}`);
  assert(getBuddyOut.pet.status === PetStatus.Available, `expected Available, got ${getBuddyOut.pet.status}`);
  assert(getBuddyOut.pet.tag === 'dog', `expected tag dog, got ${getBuddyOut.pet.tag}`);

  // Delete Whiskers
  const deleteOut = await client.deletePetJson(new DeletePetIn(whiskersId));
  assert(deleteOut.deleted === true, `expected deleted=true, got ${deleteOut.deleted}`);

  // List pets again (expect 1)
  const list2Out = await client.listPetsJson(new ListPetsIn());
  assert(list2Out.pets.length === 1, `expected 1 pet, got ${list2Out.pets.length}`);
  assert(list2Out.pets[0].name === 'Buddy', `expected remaining pet Buddy, got ${list2Out.pets[0].name}`);

  console.log('OK');
}
