import 'dart:convert';
import 'dart:io';

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

Future<void> runClient(String host, int port) async {
  final client = HttpClient();
  try {
    // Reset
    await _post(client, host, port, '/reset', '');

    // The generated PetStoreClient drives JSON requests through a single
    // transport callback. We back the JSON transport with HTTP. (The acceptance
    // harness generates JSON codecs only, so the client exposes one transport;
    // JSON methods always carry a `Json` suffix regardless of whether UEBA is
    // active, matching the other backends. When UEBA is also active the client
    // takes a second transport and exposes UEBA-default + `*Json` methods.)
    final petStore = PetStoreClient(
      (service, method, data) async =>
          _post(client, host, port, '/$service/$method', data),
    );

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

    print('OK');
  } finally {
    client.close();
  }
}
