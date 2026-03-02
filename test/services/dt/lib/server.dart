import 'dart:convert';
import 'dart:io';

import 'package:baboon_runtime/baboon_runtime.dart';
import 'generated/petstore/api/petstore/addpet/in.dart' as addpet;
import 'generated/petstore/api/petstore/addpet/out.dart' as addpet;
import 'generated/petstore/api/petstore/getpet/in.dart' as getpet;
import 'generated/petstore/api/petstore/getpet/out.dart' as getpet;
import 'generated/petstore/api/petstore/listpets/in.dart' as listpets;
import 'generated/petstore/api/petstore/listpets/out.dart' as listpets;
import 'generated/petstore/api/petstore/deletepet/in.dart' as deletepet;
import 'generated/petstore/api/petstore/deletepet/out.dart' as deletepet;
import 'petstore_impl.dart';

final _ctx = BaboonCodecContext.defaultCtx;

final _impl = PetStoreImpl();
late final HttpServer _server;

Future<void> startServer(String host, int port) async {
  _server = await HttpServer.bind(host, port);
  print('Listening on $host:$port');

  await for (final request in _server) {
    try {
      await _handleRequest(request);
    } catch (e) {
      request.response
        ..statusCode = HttpStatus.internalServerError
        ..headers.contentType = ContentType.text
        ..write(e.toString());
      await request.response.close();
    }
  }
}

Future<void> _handleRequest(HttpRequest request) async {
  final path = request.uri.path;

  if (request.method == 'GET' && path == '/health') {
    request.response
      ..statusCode = HttpStatus.ok
      ..headers.contentType = ContentType.text
      ..write('ok');
    await request.response.close();
    return;
  }

  if (request.method == 'POST' && path == '/reset') {
    _impl.reset();
    request.response
      ..statusCode = HttpStatus.ok
      ..headers.contentType = ContentType.text
      ..write('ok');
    await request.response.close();
    return;
  }

  if (request.method == 'POST' && path == '/shutdown') {
    request.response
      ..statusCode = HttpStatus.ok
      ..headers.contentType = ContentType.text
      ..write('ok');
    await request.response.close();
    await _server.close(force: true);
    return;
  }

  if (request.method == 'POST') {
    final parts = path.split('/').where((p) => p.isNotEmpty).toList();
    if (parts.length == 2) {
      final method = parts[1];
      final body = await utf8.decoder.bind(request).join();
      final wire = jsonDecode(body);
      final result = _dispatch(method, wire);
      if (result != null) {
        final responseBody = jsonEncode(result);
        request.response
          ..statusCode = HttpStatus.ok
          ..headers.contentType = ContentType.json
          ..write(responseBody);
        await request.response.close();
        return;
      }
    }
  }

  request.response
    ..statusCode = HttpStatus.notFound
    ..headers.contentType = ContentType.text
    ..write('Not Found');
  await request.response.close();
}

dynamic _dispatch(String method, dynamic wire) {
  switch (method) {
    case 'addPet':
      final input = addpet.in_JsonCodec.instance.decode(_ctx, wire);
      final output = _impl.addPet(input);
      return addpet.out_JsonCodec.instance.encode(_ctx, output);
    case 'getPet':
      final input = getpet.in_JsonCodec.instance.decode(_ctx, wire);
      final output = _impl.getPet(input);
      return getpet.out_JsonCodec.instance.encode(_ctx, output);
    case 'listPets':
      final input = listpets.in_JsonCodec.instance.decode(_ctx, wire);
      final output = _impl.listPets(input);
      return listpets.out_JsonCodec.instance.encode(_ctx, output);
    case 'deletePet':
      final input = deletepet.in_JsonCodec.instance.decode(_ctx, wire);
      final output = _impl.deletePet(input);
      return deletepet.out_JsonCodec.instance.encode(_ctx, output);
    default:
      return null;
  }
}
