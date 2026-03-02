import 'dart:io';

import 'package:dt_service/server.dart';
import 'package:dt_service/client.dart';

Future<void> main(List<String> args) async {
  if (args.isEmpty) {
    stderr.writeln('Usage: dart run dt_service <server|client> [--host HOST] [--port PORT]');
    exit(1);
  }

  final mode = args[0];
  var host = '127.0.0.1';
  var port = 8080;

  for (var i = 1; i < args.length; i++) {
    if (args[i] == '--host' && i + 1 < args.length) {
      host = args[i + 1];
      i++;
    } else if (args[i] == '--port' && i + 1 < args.length) {
      port = int.parse(args[i + 1]);
      i++;
    }
  }

  switch (mode) {
    case 'server':
      await startServer(host, port);
    case 'client':
      await runClient(host, port);
    default:
      stderr.writeln('Unknown mode: $mode (expected server or client)');
      exit(1);
  }
}
