import 'dart:convert';
import 'dart:io';

import 'package:baboon_runtime/baboon_runtime.dart';
import 'generated/petstore/api/pet_status.dart';
import 'generated/petstore/api/petstore/addpet/in.dart' as addpet;
import 'generated/petstore/api/petstore/addpet/out.dart' as addpet;
import 'generated/petstore/api/petstore/getpet/in.dart' as getpet;
import 'generated/petstore/api/petstore/getpet/out.dart' as getpet;
import 'generated/petstore/api/petstore/listpets/in.dart' as listpets;
import 'generated/petstore/api/petstore/listpets/out.dart' as listpets;
import 'generated/petstore/api/petstore/deletepet/in.dart' as deletepet;
import 'generated/petstore/api/petstore/deletepet/out.dart' as deletepet;

final _ctx = BaboonCodecContext.defaultCtx;

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

    // Add Buddy
    final addBuddyIn = addpet.in_(name: 'Buddy', status: PetStatus.Available, tag: 'dog');
    final addBuddyBody = jsonEncode(addpet.in_JsonCodec.instance.encode(_ctx, addBuddyIn));
    final addBuddyResp = await _post(client, host, port, '/PetStore/addPet', addBuddyBody);
    final addBuddyOut = addpet.out_JsonCodec.instance.decode(_ctx, jsonDecode(addBuddyResp));
    final buddyId = addBuddyOut.pet.id;
    assert(addBuddyOut.pet.name == 'Buddy', 'expected name Buddy, got ${addBuddyOut.pet.name}');

    // Add Whiskers
    final addWhiskersIn = addpet.in_(name: 'Whiskers', status: PetStatus.Pending, tag: 'cat');
    final addWhiskersBody = jsonEncode(addpet.in_JsonCodec.instance.encode(_ctx, addWhiskersIn));
    final addWhiskersResp = await _post(client, host, port, '/PetStore/addPet', addWhiskersBody);
    final addWhiskersOut = addpet.out_JsonCodec.instance.decode(_ctx, jsonDecode(addWhiskersResp));
    final whiskersId = addWhiskersOut.pet.id;
    assert(addWhiskersOut.pet.name == 'Whiskers', 'expected name Whiskers, got ${addWhiskersOut.pet.name}');

    // List pets (expect 2)
    final listIn = listpets.in_();
    final listBody = jsonEncode(listpets.in_JsonCodec.instance.encode(_ctx, listIn));
    final listResp = await _post(client, host, port, '/PetStore/listPets', listBody);
    final listOut = listpets.out_JsonCodec.instance.decode(_ctx, jsonDecode(listResp));
    assert(listOut.pets.length == 2, 'expected 2 pets, got ${listOut.pets.length}');

    // Get Buddy
    final getBuddyIn = getpet.in_(id: buddyId);
    final getBuddyBody = jsonEncode(getpet.in_JsonCodec.instance.encode(_ctx, getBuddyIn));
    final getBuddyResp = await _post(client, host, port, '/PetStore/getPet', getBuddyBody);
    final getBuddyOut = getpet.out_JsonCodec.instance.decode(_ctx, jsonDecode(getBuddyResp));
    assert(getBuddyOut.pet.name == 'Buddy', 'expected Buddy, got ${getBuddyOut.pet.name}');
    assert(getBuddyOut.pet.status == PetStatus.Available, 'expected Available, got ${getBuddyOut.pet.status}');
    assert(getBuddyOut.pet.tag == 'dog', 'expected tag dog, got ${getBuddyOut.pet.tag}');

    // Delete Whiskers
    final deleteIn = deletepet.in_(id: whiskersId);
    final deleteBody = jsonEncode(deletepet.in_JsonCodec.instance.encode(_ctx, deleteIn));
    final deleteResp = await _post(client, host, port, '/PetStore/deletePet', deleteBody);
    final deleteOut = deletepet.out_JsonCodec.instance.decode(_ctx, jsonDecode(deleteResp));
    assert(deleteOut.deleted == true, 'expected deleted=true, got ${deleteOut.deleted}');

    // List pets again (expect 1)
    final list2Resp = await _post(client, host, port, '/PetStore/listPets', listBody);
    final list2Out = listpets.out_JsonCodec.instance.decode(_ctx, jsonDecode(list2Resp));
    assert(list2Out.pets.length == 1, 'expected 1 pet, got ${list2Out.pets.length}');
    assert(list2Out.pets[0].name == 'Buddy', 'expected remaining pet Buddy, got ${list2Out.pets[0].name}');

    print('OK');
  } finally {
    client.close();
  }
}
