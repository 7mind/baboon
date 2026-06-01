import 'dart:convert';
import 'dart:io';

import 'package:baboon_runtime/baboon_runtime.dart';
import 'generated/petstore/api/pet_store.dart';
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
      final serviceName = parts[0];
      final method = parts[1];
      final body = await utf8.decoder.bind(request).join();
      try {
        // Route through the generated server wiring's static JSON dispatcher.
        final responseBody = PetStoreWiring.invokeJson(
          BaboonMethodId(serviceName, method),
          body,
          _impl,
          _ctx,
        );
        request.response
          ..statusCode = HttpStatus.ok
          ..headers.contentType = ContentType.json
          ..write(responseBody);
        await request.response.close();
        return;
      } on BaboonWiringException catch (e) {
        request.response
          ..statusCode = HttpStatus.notFound
          ..headers.contentType = ContentType.text
          ..write(e.toString());
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
