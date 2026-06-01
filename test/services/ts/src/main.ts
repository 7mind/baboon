import { startServer } from './server.js';
import { runClient } from './client.js';

const args = process.argv.slice(2);
const mode = args[0];

function parseArgs(args: string[]): { host: string; port: number; codec: string } {
  let host = '127.0.0.1';
  let port = 18080;
  let codec = 'both';
  for (let i = 1; i < args.length; i++) {
    if (args[i] === '--host' && i + 1 < args.length) {
      host = args[++i];
    } else if (args[i] === '--port' && i + 1 < args.length) {
      port = parseInt(args[++i], 10);
    } else if (args[i] === '--codec' && i + 1 < args.length) {
      codec = args[++i];
    }
  }
  return { host, port, codec };
}

const { host, port, codec } = parseArgs(args);

if (mode === 'server') {
  startServer(host, port);
} else if (mode === 'client') {
  runClient(host, port, codec).then(
    () => process.exit(0),
    (err) => {
      console.error('Client failed:', err);
      process.exit(1);
    }
  );
} else {
  console.error('Usage: main.ts <server|client> --host <host> --port <port>');
  process.exit(1);
}
