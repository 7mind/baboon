import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:baboon_runtime/baboon_runtime.dart';
import 'generated/petstore/api/pet_status.dart';
import 'generated/petstore/api/pet_store.dart';
import 'generated/petstore/api/petstore/addpet/in.dart' as addpet;
import 'generated/petstore/api/petstore/getpet/in.dart' as getpet;
import 'generated/petstore/api/petstore/listpets/in.dart' as listpets;
import 'generated/petstore/api/petstore/deletepet/in.dart' as deletepet;

Future<String> _post(HttpClient client, String host, int port, String path, String body) async {
  final request = await client.postUrl(Uri.parse('http://$host:$port$path'));
  final bytes = utf8.encode(body);
  request.headers.contentType = ContentType.json;
  request.contentLength = bytes.length;
  request.add(bytes);
  final response = await request.close();
  return await utf8.decoder.bind(response).join();
}

Future<Uint8List> postBytes(
    HttpClient client, String host, int port, String path, List<int> data) async {
  final request = await client.postUrl(Uri.parse('http://$host:$port$path'));
  request.headers.contentType = ContentType('application', 'octet-stream');
  request.contentLength = data.length;
  request.add(data);
  final response = await request.close();
  final chunks = await response.fold<List<int>>(
    <int>[],
    (acc, chunk) => acc..addAll(chunk),
  );
  return Uint8List.fromList(chunks);
}

Future<void> runClient(String host, int port) async {
  final client = HttpClient();
  try {
    // Reset
    await _post(client, host, port, '/reset', '');

    // When both codecs are active the generated PetStoreClient takes two
    // transports: a binary UEBA transport (first) and a JSON transport
    // (second). Bare method names (`addPet`) drive UEBA; `*Json` names drive
    // JSON. We back each with HTTP, distinguishing them by Content-Type.
    final petStore = PetStoreClient(
      (service, method, data) async =>
          postBytes(client, host, port, '/$service/$method', data),
      (service, method, data) async =>
          _post(client, host, port, '/$service/$method', data),
    );

    // ---- JSON scenario -----------------------------------------------------

    // Add Buddy
    final addBuddyOut = await petStore.addPetJson(
      addpet.in_(name: 'Buddy', status: PetStatus.Available, tag: 'dog'),
    );
    final buddyId = addBuddyOut.pet.id;
    assert(addBuddyOut.pet.name == 'Buddy', 'expected name Buddy, got ${addBuddyOut.pet.name}');

    // Add Whiskers
    final addWhiskersOut = await petStore.addPetJson(
      addpet.in_(name: 'Whiskers', status: PetStatus.Pending, tag: 'cat'),
    );
    final whiskersId = addWhiskersOut.pet.id;
    assert(addWhiskersOut.pet.name == 'Whiskers', 'expected name Whiskers, got ${addWhiskersOut.pet.name}');

    // List pets (expect 2)
    final listOut = await petStore.listPetsJson(listpets.in_());
    assert(listOut.pets.length == 2, 'expected 2 pets, got ${listOut.pets.length}');

    // Get Buddy
    final getBuddyOut = await petStore.getPetJson(getpet.in_(id: buddyId));
    assert(getBuddyOut.pet.name == 'Buddy', 'expected Buddy, got ${getBuddyOut.pet.name}');
    assert(getBuddyOut.pet.status == PetStatus.Available, 'expected Available, got ${getBuddyOut.pet.status}');
    assert(getBuddyOut.pet.tag == 'dog', 'expected tag dog, got ${getBuddyOut.pet.tag}');

    // Delete Whiskers
    final deleteOut = await petStore.deletePetJson(deletepet.in_(id: whiskersId));
    assert(deleteOut.deleted == true, 'expected deleted=true, got ${deleteOut.deleted}');

    // List pets again (expect 1)
    final list2Out = await petStore.listPetsJson(listpets.in_());
    assert(list2Out.pets.length == 1, 'expected 1 pet, got ${list2Out.pets.length}');
    assert(list2Out.pets[0].name == 'Buddy', 'expected remaining pet Buddy, got ${list2Out.pets[0].name}');

    // ---- UEBA scenario -----------------------------------------------------
    // Re-run the identical scenario over the binary UEBA transport (bare method
    // names) and assert identical results.

    await _post(client, host, port, '/reset', '');

    // Add Buddy
    final uAddBuddyOut = await petStore.addPet(
      addpet.in_(name: 'Buddy', status: PetStatus.Available, tag: 'dog'),
    );
    final uBuddyId = uAddBuddyOut.pet.id;
    assert(uAddBuddyOut.pet.name == 'Buddy', 'ueba: expected name Buddy, got ${uAddBuddyOut.pet.name}');

    // Add Whiskers
    final uAddWhiskersOut = await petStore.addPet(
      addpet.in_(name: 'Whiskers', status: PetStatus.Pending, tag: 'cat'),
    );
    final uWhiskersId = uAddWhiskersOut.pet.id;
    assert(uAddWhiskersOut.pet.name == 'Whiskers', 'ueba: expected name Whiskers, got ${uAddWhiskersOut.pet.name}');

    // List pets (expect 2)
    final uListOut = await petStore.listPets(listpets.in_());
    assert(uListOut.pets.length == 2, 'ueba: expected 2 pets, got ${uListOut.pets.length}');

    // Get Buddy
    final uGetBuddyOut = await petStore.getPet(getpet.in_(id: uBuddyId));
    assert(uGetBuddyOut.pet.name == 'Buddy', 'ueba: expected Buddy, got ${uGetBuddyOut.pet.name}');
    assert(uGetBuddyOut.pet.status == PetStatus.Available, 'ueba: expected Available, got ${uGetBuddyOut.pet.status}');
    assert(uGetBuddyOut.pet.tag == 'dog', 'ueba: expected tag dog, got ${uGetBuddyOut.pet.tag}');

    // Delete Whiskers
    final uDeleteOut = await petStore.deletePet(deletepet.in_(id: uWhiskersId));
    assert(uDeleteOut.deleted == true, 'ueba: expected deleted=true, got ${uDeleteOut.deleted}');

    // List pets again (expect 1)
    final uList2Out = await petStore.listPets(listpets.in_());
    assert(uList2Out.pets.length == 1, 'ueba: expected 1 pet, got ${uList2Out.pets.length}');
    assert(uList2Out.pets[0].name == 'Buddy', 'ueba: expected remaining pet Buddy, got ${uList2Out.pets[0].name}');

    print('OK');
  } finally {
    client.close();
  }
}
