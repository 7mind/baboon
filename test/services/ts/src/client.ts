import * as http from 'node:http';
import { BaboonCodecContext } from './generated/BaboonSharedRuntime.js';
import { PetStore } from './generated/petstore/api/index.js';
import { PetStatus } from './generated/petstore/api/PetStatus.js';

// Per-method message types and codecs are reached through the service
// namespace barrel: PetStore.methods.<method>.{In, Out, In_JsonCodec, ...}.
const AddPetIn = PetStore.methods.addpet.In;
const AddPetInCodec = PetStore.methods.addpet.In_JsonCodec;
const AddPetOutCodec = PetStore.methods.addpet.Out_JsonCodec;
const GetPetIn = PetStore.methods.getpet.In;
const GetPetInCodec = PetStore.methods.getpet.In_JsonCodec;
const GetPetOutCodec = PetStore.methods.getpet.Out_JsonCodec;
const ListPetsIn = PetStore.methods.listpets.In;
const ListPetsInCodec = PetStore.methods.listpets.In_JsonCodec;
const ListPetsOutCodec = PetStore.methods.listpets.Out_JsonCodec;
const DeletePetIn = PetStore.methods.deletepet.In;
const DeletePetInCodec = PetStore.methods.deletepet.In_JsonCodec;
const DeletePetOutCodec = PetStore.methods.deletepet.Out_JsonCodec;

const ctx = BaboonCodecContext.Default;

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
  // Reset
  await post(host, port, '/reset', '');

  // Add Buddy
  const addBuddyInput = new AddPetIn('Buddy', PetStatus.Available, 'dog');
  const addBuddyJson = JSON.stringify(AddPetInCodec.instance.encode(ctx, addBuddyInput));
  const addBuddyResp = await post(host, port, '/PetStore/addPet', addBuddyJson);
  const addBuddyOut = AddPetOutCodec.instance.decode(ctx, JSON.parse(addBuddyResp));
  const buddyId = addBuddyOut.pet.id;
  assert(addBuddyOut.pet.name === 'Buddy', `expected name Buddy, got ${addBuddyOut.pet.name}`);

  // Add Whiskers
  const addWhiskersInput = new AddPetIn('Whiskers', PetStatus.Pending, 'cat');
  const addWhiskersJson = JSON.stringify(AddPetInCodec.instance.encode(ctx, addWhiskersInput));
  const addWhiskersResp = await post(host, port, '/PetStore/addPet', addWhiskersJson);
  const addWhiskersOut = AddPetOutCodec.instance.decode(ctx, JSON.parse(addWhiskersResp));
  const whiskersId = addWhiskersOut.pet.id;
  assert(addWhiskersOut.pet.name === 'Whiskers', `expected name Whiskers, got ${addWhiskersOut.pet.name}`);

  // List pets (expect 2)
  const listInput = new ListPetsIn();
  const listJson = JSON.stringify(ListPetsInCodec.instance.encode(ctx, listInput));
  const listResp = await post(host, port, '/PetStore/listPets', listJson);
  const listOut = ListPetsOutCodec.instance.decode(ctx, JSON.parse(listResp));
  assert(listOut.pets.length === 2, `expected 2 pets, got ${listOut.pets.length}`);

  // Get Buddy
  const getBuddyInput = new GetPetIn(buddyId);
  const getBuddyJson = JSON.stringify(GetPetInCodec.instance.encode(ctx, getBuddyInput));
  const getBuddyResp = await post(host, port, '/PetStore/getPet', getBuddyJson);
  const getBuddyOut = GetPetOutCodec.instance.decode(ctx, JSON.parse(getBuddyResp));
  assert(getBuddyOut.pet.name === 'Buddy', `expected Buddy, got ${getBuddyOut.pet.name}`);
  assert(getBuddyOut.pet.status === PetStatus.Available, `expected Available, got ${getBuddyOut.pet.status}`);
  assert(getBuddyOut.pet.tag === 'dog', `expected tag dog, got ${getBuddyOut.pet.tag}`);

  // Delete Whiskers
  const deleteInput = new DeletePetIn(whiskersId);
  const deleteJson = JSON.stringify(DeletePetInCodec.instance.encode(ctx, deleteInput));
  const deleteResp = await post(host, port, '/PetStore/deletePet', deleteJson);
  const deleteOut = DeletePetOutCodec.instance.decode(ctx, JSON.parse(deleteResp));
  assert(deleteOut.deleted === true, `expected deleted=true, got ${deleteOut.deleted}`);

  // List pets again (expect 1)
  const list2Resp = await post(host, port, '/PetStore/listPets', listJson);
  const list2Out = ListPetsOutCodec.instance.decode(ctx, JSON.parse(list2Resp));
  assert(list2Out.pets.length === 1, `expected 1 pet, got ${list2Out.pets.length}`);
  assert(list2Out.pets[0].name === 'Buddy', `expected remaining pet Buddy, got ${list2Out.pets[0].name}`);

  console.log('OK');
}
